module Parser where
  import Data.Char (chr, ord, toUpper, isDigit, digitToInt)
  import Control.Applicative
  import Control.Monad
  import Text.ParserCombinators.Parsec (
      Parser
    , try
    , string
    , skipMany
    , skipMany1
    , oneOf
    , noneOf
    , satisfy
    , (<?>)
    , char
    )
  import qualified Text.Parsec.Token as T
  import Text.Parsec.Language (javaStyle)

  oneLineComment :: Parser ()
  oneLineComment =
    do{ try (string "//")
      ; skipMany (satisfy (/= '\n'))
      ; return ()
      }

  multiLineComment :: Parser ()
  multiLineComment =
    do { try (string "/*")
       ; inComment
       }

  inComment :: Parser ()
  inComment = inCommentSingle

  stringCharParser :: Parser Char
  stringCharParser =
        normalStringCharParser
    <|> escapedCharParser
    <|> escapedAsciiParser
    <|> escapedShortUnicodeParser
    <|> escapedLongUnicodeParser

  normalStringCharParser :: Parser Char
  normalStringCharParser = noneOf ['"', '\n', '\r', '\\']

  escapedCharParser :: Parser Char
  escapedCharParser = do
    void (char '\\')
    ch <- oneOf "nrtf\"\\"
    case ch of
      'n' -> return '\n'
      'r' -> return '\r'
      't' -> return '\t'
      'f' -> return '\f'
      _ -> return ch

  times :: Int -> Parser a -> Parser [a]
  times n p = sequence (replicate n p)

  hexDigitParser :: Parser Int
  hexDigitParser = do
    c <- oneOf (['0' .. '9'] ++ ['A' .. 'F'] ++ ['a' .. 'f'])
    if isDigit c then return (digitToInt c)
    else return (ord (toUpper c) - ord('A') + 10)

  fromBaseLTR :: [Int] -> Int -> Int
  fromBaseLTR digits base =
    foldl (\acc digit -> acc * base + digit) 0 digits

  escapedAsciiParser :: Parser Char
  escapedAsciiParser = do
    void (string "\\x")
    digits <- 2 `times` hexDigitParser
    let x = digits `fromBaseLTR` 16
    return (chr x)



  newtype StringLiteral = StringLiteral String

  stringLiteralParser :: Parser StringLiteral
  stringLiteralParser = do
    void (char '"')
    stringChars <- many stringCharParser
    void (char '"')
    return (StringLiteral stringChars)

{-
  r ["](
      ([^"\n\r])
    | ([\][nrtf"\])
    | ([\][x][0-9a-fA-F]{2})
    | ([\][u][0-9a-fA-F]{4})
    | ([\][U][0-9a-fA-F]{8})
    )*
    ["]
-}

  {-
  inCommentMulti
    =   do{ try (string (commentEnd languageDef)) ; return () }
    <|> do{ multiLineComment                     ; inCommentMulti }
    <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
    <|> do{ oneOf startEnd                       ; inCommentMulti }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)
  -}

  inCommentSingle :: Parser ()
  inCommentSingle
    =   do{ try (string "*/"); return () }
    <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
    <|> do{ oneOf startEnd                      ; inCommentSingle }
    <?> "end of comment"
    where
      startEnd   = "/*"

  stringLiteral   =
    do{ str <- between (char '"')
                       (char '"' <?> "end of string")
                       (many stringChar)
      ; return (foldr (maybe id (:)) "" str)
      }

{-

  kOuterLanguageDef :: T.LanguageDef st
  kOuterLanguageDef = javaStyle {
    T.caseSensitive = True,
    T.reservedNames = [
      "requires",
      "module",
      "endmodule",
      "import",
      "syntax",
      "rule",
      "context",
      "configuration"
    ]
  }

i

  kOuterTokenParser = T.makeTokenParser kOuterLanguageDef

  idParser = T.identifier kOuterTokenParser
  reservedParser = T.reserved kOuterTokenParser
  stringLiteralParser = T.stringLiteral kOuterTokenParser

  newtype KImport = KImport {getImport :: String}
  newtype KModule = KModule {getModule :: String}

  data KFile = KFile {
    imports :: [KImport],
    modules :: [KModule]
  }

  kFileParser :: Parser KFile
  kFileParser = do
    imports <- many importParser
    modules <- many moduleParser
    return (KFile {imports = imports, modules = modules})

  importParser :: Parser KImport
  importParser = do
    reservedParser "requires"
    imp <- stringLiteralParser
    return (KImport imp)

  lineParser :: Parser String
  lineParser = do
    whiteSpace
    notFollowedBy (reservedParser "endmodule")
    line <- many ()


  moduleParser :: Parser KModule
  moduleParser = do
    reservedParser "module"
    moduleName <- idParser

-}
module Lang.OPL.Calculus.Parser where

data Token =
    Keyword String
  | Indent
  | Unindent
  | Whitespace
  | Newline
  | Comment String
  | Symbol String

keywords :: [String]
keywords =
  [ "="
  , "."
  , "->"
  , "<-"
  , "-<"
  , ">->"
  , "forall"
  , "fun"
  , "tfun"
  , "wire"
  ]

keywordsLongestFirst :: [String]
keywordsLongestFirst = sortBy ((>=) `on` length) keywords

tokenKeyword :: Parser String
tokenKeyword = msum $ each keywordsLongestFirst $ string

tokenWhitespace :: Parser String
tokenWhitespace = seq1 $ any " "

tokenNewline :: Parser String
tokenNewline = string "\n" <|> string "\n\r"

tokenComment :: Parser String
tokenComment = undefined

tokenSymbol :: Parser String
tokenSymbol = do
  x <- alpha <|> any "_"
  xs <- seq $ alphaNumeric <|> punctuation
  return $ x:xs

tokenComment :: Parser String
tokenComment = msum
  [ do 
      string "--"
      endBy tokenNewline
  , tokenNestedComment
  ]

tokenNestedComment :: Parser String
tokenNestedComment = msum
  [ string "{-"
    tokenNestedComment
    string "-}"
  , char $ alpha <|> num <|> punctuation <|> space <|> newline
  ]

token :: Parser Token
token = msum
  [ liftM Keyword tokenKeyword
  , liftM Whitespace tokenWhitespace
  , liftM Comment tokenComment
  , liftM Symbol tokenSymbol
  ]

def :: Parser Def
def = 

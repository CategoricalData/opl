module Lang.OPL.Lexer where

import Lang.OPL.Annotated
import Data.Text (Text)
import Control.Monad
import Control.Applicative
import Data.Function
import Data.List
import Lang.OPL.Syntax
import Text.Parsec (SourcePos, ParseError)
import Text.Parsec.Text (Parser)
import qualified Text.Parsec as P

-------------------- Specification --------------------

type AnnToken = Annotated SourcePos Token

data Token =
    WhitespaceToken
  | CommentToken
  | PunctuationToken String
  | KeywordToken String
  | PathToken Path
  deriving (Eq, Ord, Show)

punctuation :: [String]
punctuation = 
  [ "("
  , ")"
  , ","
  , "->"
  , ":"
  , ":="
  , "<-"
  , "=>"
  , "=[]="
  , "@"
  , "["
  , "]"
  , "_"
  ]

keywords :: [String]
keywords =
  [ "algebra"
  , "all"
  , "apply"
  , "box"
  , "define"
  , "end"
  , "external"
  , "import"
  , "internal"
  , "module"
  , "none"
  , "only"
  , "plug"
  , "provide"
  , "qualified"
  , "require"
  , "where"
  , "wiring"
  ]

commentLeader :: String
commentLeader = "#"

nestedCommentOpen :: String
nestedCommentOpen = "#|"

nestedCommentClose :: String
nestedCommentClose = "|#"

-------------------- Whitespace --------------------

tokenizeWhitespace :: Parser String
tokenizeWhitespace = P.many1 $ P.oneOf " \t\n\r"

-------------------- Comments --------------------

tokenizeComment :: Parser ()
tokenizeComment = do
  m <- P.optionMaybe $ P.try $ P.string nestedCommentOpen
  case m of
    Just _ -> tokenizeNestedComment 1
    Nothing -> tokenizeFlatComment
  
tokenizeFlatComment :: Parser ()
tokenizeFlatComment = do
  P.string commentLeader
  P.many $ P.noneOf "\n\r"
  P.newline
  return ()

tokenizeNestedComment :: Int -> Parser ()
tokenizeNestedComment 0 = return ()
tokenizeNestedComment n = msum
  [ do
      P.try $ P.string nestedCommentOpen
      tokenizeNestedComment (n+1)
  , do
      P.try $ P.string nestedCommentClose
      tokenizeNestedComment (n-1)
  , do
      P.anyChar
      tokenizeNestedComment n
  ]

-------------------- Punctuation --------------------

punctuationLongestFirst :: [String]
punctuationLongestFirst = sortBy (flipCompare `on` length) punctuation

tokenizePunctuation :: Parser String
tokenizePunctuation = msum $ map (P.try . P.string) punctuationLongestFirst

-------------------- Keywords --------------------

keywordsLongestFirst :: [String]
keywordsLongestFirst = sortBy (flipCompare `on` length) keywords

tokenizeKeyword :: Parser String
tokenizeKeyword = do
  kwd <- msum $ map (P.try . P.string) keywordsLongestFirst
  P.notFollowedBy tokenizePath
  return kwd

-------------------- Symbols --------------------

tokenizeSymbol :: Parser String
tokenizeSymbol = do
  x <- P.letter `mplus` P.oneOf "_"
  xs <- P.many $ P.alphaNum `mplus` P.oneOf "_'"
  return $ x:xs

-------------------- Paths --------------------

tokenizePath :: Parser Path
tokenizePath = do
  s <- tokenizeSymbol
  msum
    [ do
        P.char '.'
        p <- tokenizePath
        return $ Name s :.: p
    , return $ SingletonPath $ Name s
    ]

-------------------- Main --------------------

preTokenize :: Parser [AnnToken]
preTokenize = do
  ts <- P.many $ annotate $ msum
    [ liftM (const WhitespaceToken) tokenizeWhitespace
    , liftM (const CommentToken) tokenizeComment
    , liftM PunctuationToken tokenizePunctuation
    , P.try $ liftM KeywordToken tokenizeKeyword
    , liftM PathToken tokenizePath
    ]
  P.eof
  return ts

tokenize :: Parser [AnnToken]
tokenize = liftM (filter keeper) preTokenize
  where
    keeper x = and
      [ stripAnnotation x /= WhitespaceToken
      , stripAnnotation x /= CommentToken
      ]

runTokenize :: String -> Text -> Either ParseError [AnnToken]
runTokenize = P.parse tokenize

-------------------- Punctuation --------------------

flipCompare :: (Ord a) => a -> a -> Ordering
flipCompare x y = case compare x y of
  LT -> GT
  EQ -> EQ
  GT -> LT

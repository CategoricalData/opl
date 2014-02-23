module Lang.OPL.Parser where

import Lang.OPL.Annotated
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Lang.OPL.Lexer
import Lang.OPL.Syntax
import Text.Parsec (Parsec, ParseError, SourcePos)
import qualified Text.Parsec as P

type Parser = Parsec [AnnToken] ()

-------------------- Primitives --------------------

token :: (Token -> Maybe a) -> Parser a
token f = P.token (show . stripAnnotation) annotation (f . stripAnnotation)

tokenEqual :: Token -> Parser Token
tokenEqual t = token satisfy
  where
    satisfy x = if x == t then Just x else Nothing

pun :: String -> Parser ()
pun = void . tokenEqual . PunctuationToken

key :: String -> Parser ()
key = void . tokenEqual . KeywordToken

keys :: [String] -> Parser ()
keys = mapM_ key

name :: Parser Name
name = token $ \ t ->
  case t of
    PathToken (SingletonPath n) -> Just n
    _ -> Nothing

aname :: Parser AName
aname = annotate name

ttype :: Parser Type
ttype = aname

path :: Parser Path
path = token $ \ t ->
  case t of
    PathToken p -> Just p
    _ -> Nothing

apath :: Parser APath
apath = annotate path

-------------------- Helpers --------------------

mapping :: Parser Mapping
mapping = do
  n <- aname
  pun "<-"
  m <- aname
  return $ Mapping n m

mappingList :: Parser [Mapping]
mappingList = mapping `P.sepBy` pun ","

binder :: Parser Binder
binder = do
  n <- aname
  pun ":"
  t <- ttype
  return $ Binder n t

binderList :: Parser [Binder]
binderList = binder `P.sepBy` pun ","

plug :: Parser Plug
plug = do
  n <- aname
  pun "<-"
  p <- apath
  return $ Plug n p

plugList :: Parser [Plug]
plugList = plug `P.sepBy` pun ","

eexport :: Parser Export
eexport = do
  n <- aname
  key "as"
  m <- aname
  return $ Mapping n m

exportList :: Parser [Export]
exportList = eexport `P.sepBy` pun ","

-------------------- Boxes --------------------

box :: Parser Box
box = do
  key "with"
  ins <- row "input"
  outs <- row "output"
  key "end"
  return $ Box ins outs
  where
    row kwd = do
      key kwd
      binderList

-------------------- Wirings --------------------

wiringDiagram :: Parser WiringDiagram
wiringDiagram = do
  key "with"
  internal <- P.many $ do
    keys ["internal", "box"]
    b <- binder
    keys ["with", "input"]
    ps <- plugList
    return $ Wiring b ps
  external <- do
    keys ["external", "box"]
    b <- binder
    keys ["with", "output"]
    ps <- plugList
    return $ Wiring b ps
  key "end"
  return $ WiringDiagram internal external

wiringComposition :: Parser WiringComposition
wiringComposition = do
  key "with"
  external <- do
    keys ["external", "w.d."]
    aname
  internal <- P.many $ do
    keys ["internal", "w.d."]
    m <- mapping
    keys ["export", "internal", "box"]
    es <- exportList
    return $ Filling m es
  key "end"
  return $ WiringComposition external internal

-------------------- Defs --------------------

parseDef :: Parser Def
parseDef = msum
  [ do
      key "box"
      n <- aname
      b <- box
      return $ BoxDef n b
  , P.try $ do
      key "wiring"
      key "diagram"
      n <- aname
      wd <- wiringDiagram
      return $ WiringDiagramDef n wd
  , do
      key "wiring"
      key "composition"
      n <- aname
      wc <- wiringComposition
      return $ WiringCompositionDef n wc
  ]

-------------------- Main --------------------

parseTokens :: Parser [Def]
parseTokens = P.many parseDef <* P.eof

runParse :: String -> Text -> Either ParseError [Def]
runParse name = P.parse tokenize name >=> P.parse parseTokens name

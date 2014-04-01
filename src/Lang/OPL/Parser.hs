module Lang.OPL.Parser where

import Prelude ()
import FP
import Lang.OPL.Annotated
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Lang.OPL.Lexer
import Lang.OPL.Syntax
import Lang.OPL.Common
import Text.Parsec (ParsecT, ParseError, SourcePos, SourceName)
import qualified Text.Parsec as P

data Env = Env
  { _precDLL :: DumbLattice
  , _precLevelL :: Level
  } deriving (Eq, Ord, Show)
makeLens ''Env

type Parser = ParsecT [AnnToken] () (Reader Env)
type instance MEnv Parser = Env

-------------------- Precedence --------------------

lteM :: Level -> Parser Bool
lteM l = do
  dl <- askView precDLL
  cl <- askView precLevelL
  return $ dlLte dl l cl

atLevel :: Level -> Parser a -> Parser a
atLevel = localViewSet precLevelL

guardLevel :: Level -> Parser a -> Parser a
guardLevel l aM = do
  b <- lteM l
  when (not b) mzero
  atLevel l aM

guardLevelParen :: Level -> Parser a -> Parser a
guardLevelParen l aM = msum
  [ parens aM
  , guardLevel l aM
  ]

parens :: Parser a -> Parser a
parens p = pun "(" *> atLevel TopLevel p <* pun ")"
 
-------------------- Primitives --------------------

token :: (Token -> Maybe a) -> Parser a
token f = P.tokenPrim (show . stripAnnotation) (\ _ t _ -> annotation t) (f . stripAnnotation)

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

path :: Parser Path
path = token $ \ t ->
  case t of
    PathToken p -> Just p
    _ -> Nothing

apath :: Parser APath
apath = annotate path

-------------------- Boxes --------------------

plugType :: Parser PlugType
plugType = liftM PlugType aname

boxArrow :: Parser BoxArrow
boxArrow = guardLevelParen (level "=[]=") $ do
  ins <- P.many $ atLevel BotLevel plugType
  pun "=[]="
  outs <- P.many $ atLevel BotLevel plugType
  return $ BoxArrow ins outs

box :: Parser Box
box = msum
  [ P.try $ liftM ArrowBox boxArrow
  , liftM VarBox aname
  ]

-------------------- Wiring --------------------

wiringArrow :: Parser WiringArrow
wiringArrow = guardLevelParen (level "->") $ do
  ins <- P.many $ atLevel BotLevel box
  pun "->"
  out <- box
  return $ WiringArrow ins out

wiringType :: Parser WiringType
wiringType = msum
  [ P.try $ do
      n <- aname
      pun "@"
      wt <- wiringType
      return $ LiftWiringType n wt
  , P.try $ liftM ArrowWiringType wiringArrow
  , P.try $ liftM BoxWiringType box
  , liftM VarWiringType aname
  ]

boxBinder :: Parser BoxBinder
boxBinder = guardLevelParen (level "=[]=") $ do
  ins <- P.many $ atLevel BotLevel aname
  pun "=[]="
  outs <- P.many $ atLevel BotLevel aname
  return $ BoxBinder ins outs

plug :: Parser Plug
plug = do
  n <- aname
  pun "<-"
  p <- apath
  return $ Plug n p

wiring :: Parser Wiring
wiring = do
  n <- aname
  pun ":"
  b <- atLevel BotLevel box
  pun "["
  bb <- boxBinder
  pun "]"
  key "plug"
  ps <- plug `P.sepBy` pun ","
  return $ Wiring n b bb ps

wiringDiagram :: Parser WiringDiagram
wiringDiagram = do
  key "wiring"
  key "internal"
  ins <- P.many wiring
  key "external"
  out <- wiring
  key "end"
  return $ WiringDiagram ins out

renaming :: Parser Renaming
renaming = do
  from <- P.many aname
  pun "=>"
  to <- P.many aname
  return $ Renaming from to

wiringExpFlat :: Parser WiringExp
wiringExpFlat = msum
  [ liftM DiagramWiringExp wiringDiagram
  , P.try $ guardLevelParen (level " ") $ do
      n <- aname
      e <- wiringExp
      return $ LiftWiringExp n e
  , liftM VarWiringExp aname
  ]

wiringExpPost :: Parser WiringExp
wiringExpPost = msum
  [ P.try $ do
      e <- atLevel BotLevel wiringExpFlat
      pun "["
      r <- renaming
      pun "]"
      return $ RenamingWiringExp r e
  , wiringExpFlat
  ]

maybeDefineExp :: Parser (Maybe WiringExp)
maybeDefineExp = msum
  [ pun "_" >> return Nothing
  , liftM Just wiringExp
  ]

wiringExpChain :: WiringExp -> Parser WiringExp
wiringExpChain e = msum
  [ do
      pun "<-"
      es <- P.many $ atLevel BotLevel maybeDefineExp
      wiringExpChain $ ApplyWiringExp e es
  , return e
  ]

wiringExp :: Parser WiringExp
wiringExp = msum
  [ parens wiringExp
  , P.try $ guardLevelParen (level "<-") $ do
      e <- wiringExpPost
      wiringExpChain e
  , wiringExpPost
  ]

-------------------- Statements --------------------

decl :: Parser Decl
decl = msum
  [ do
      key "algebra"
      liftM AlgebraDecl aname
  , do
      key "box"
      liftM BoxDecl aname
  , do
      key "module"
      liftM ModuleDecl aname
  , do
      key "define"
      n <- aname
      pun ":"
      t <- wiringType
      return $ WiringDecl n t
  ]

def :: Parser Def
def = msum
  [ do
      key "algebra"
      n <- aname
      pun ":="
      a <- aname
      return $ AlgebraDef n a
  , do
      key "box"
      n <- aname
      pun ":="
      b <- box
      return $ BoxDef n b
  , do 
      key "module"
      n <- aname
      pun ":="
      m <- moduleGuts
      key "end"
      return $ ModuleDef n m
  , do
      key "define"
      n <- aname
      pun ":="
      e <- wiringExp
      return $ WiringDef n e
  ]

statement :: Parser Statement
statement = msum
  [ P.try $ liftM DefStatement def
  , liftM DeclStatement decl
  ]

-------------------- Modules --------------------

pimport :: Parser Import
pimport = do
  key "import"
  p <- apath
  q <- P.option False $ key "qualified" >> return True
  aps <- P.option Nothing $ do
    key "apply"
    ss <- P.many statement
    return $ Just ss
  os <- P.option Nothing $ do
    key "only"
    ds <- P.many decl
    return $ Just ds
  return $ Import p q aps os

provides :: Parser Provides
provides = msum
  [ key "all" >> return AllProvides
  , key "none" >> return NoneProvides
  , liftM ExplicitProvides $ P.many decl
  ]
  
moduleGuts :: Parser Module
moduleGuts = do
  (ds, is, ps) <- P.option ([], [], Nothing) $ do
    ds <- P.option [] $ do
      key "require"
      P.many decl
    is <- P.many pimport
    ps <- P.option Nothing $ do
      key "provide"
      liftM Just provides
    key "where"
    return (ds, is, ps)
  ss <- P.many statement
  return $ Module ds is ps ss

-------------------- Main --------------------

topLevel :: Parser TLModule
topLevel = liftM TLModule moduleGuts <* P.eof

parse :: Parser a -> DumbLattice -> SourceName -> [AnnToken] -> Either ParseError a
parse p dl name = flip runReader (Env dl TopLevel) . P.runParserT p () name

parseConstraints :: DumbLattice
parseConstraints = compile [ (" ", "<-"), ("=[]=", "->") ]

runParse :: String -> Text -> Either ParseError TLModule
runParse name = P.parse tokenize name >=> parse topLevel parseConstraints name

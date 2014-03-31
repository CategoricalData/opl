module Lang.OPL.Parser where

import Prelude ()
import FP
import Lang.OPL.Annotated
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Lang.OPL.Lexer
import Lang.OPL.Syntax
import Text.Parsec (ParsecT, ParseError, SourcePos, SourceName)
import qualified Text.Parsec as P

data Env = Env
  { _precDL :: DumbLattice
  , _precLevel :: Level
  } deriving (Eq, Ord, Show)
makeLens ''Env

type Parser = ParsecT [AnnToken] () (Reader Env)
type instance MEnv Parser = Env

-------------------- Precedence --------------------

lteM :: Level -> Parser Bool
lteM l = do
  dl <- askView precDL
  cl <- askView precLevel
  return $ dlLte dl l cl

atLevel :: Level -> Parser a -> Parser a
atLevel = localViewSet precLevel

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
  [ P.try $ liftM ArrowWiringType wiringArrow
  , liftM BoxWiringType box
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

-------------------- Expressions --------------------

defineType :: Parser DefineType
defineType = msum
  [ P.try $ do
      n <- aname
      pun "@"
      wt <- wiringType
      return $ LiftDefineType n wt
  , liftM WiringDefineType wiringType
  ]

renaming :: Parser Renaming
renaming = do
  from <- P.many aname
  pun "=>"
  to <- P.many aname
  return $ Renaming from to

defineExpFlat :: Parser DefineExp
defineExpFlat = msum
  [ liftM DiagramDefineExp wiringDiagram
  , P.try $ guardLevelParen (level " ") $ do
      n <- aname
      e <- defineExp
      return $ LiftDefineExp n e
  , liftM VarDefineExp aname
  ]

defineExpPost :: Parser DefineExp
defineExpPost = msum
  [ P.try $ do
      e <- atLevel BotLevel defineExpFlat
      pun "["
      r <- renaming
      pun "]"
      return $ RenamingDefineExp e r
  , defineExpFlat
  ]

maybeDefineExp :: Parser (Maybe DefineExp)
maybeDefineExp = msum
  [ pun "_" >> return Nothing
  , liftM Just defineExp
  ]

defineExpChain :: DefineExp -> Parser DefineExp
defineExpChain e = msum
  [ do
      pun "<-"
      es <- P.many $ atLevel BotLevel maybeDefineExp
      defineExpChain $ ApplyDefineExp e es
  , return e
  ]

defineExp :: Parser DefineExp
defineExp = msum
  [ parens defineExp
  , P.try $ guardLevelParen (level "<-") $ do
      e <- defineExpPost
      defineExpChain e
  , defineExpPost
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
  key "module"
  ds <- P.option [] $ do
    key "require"
    P.many decl
  is <- P.many pimport
  ps <- P.option Nothing $ do
    key "provide"
    liftM Just provides
  key "where"
  ss <- P.many statement
  return $ Module ds is ps ss

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
      t <- defineType
      return $ DefineDecl n t
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
      e <- defineExp
      return $ DefineDef n e
  ]

statement :: Parser Statement
statement = msum
  [ P.try $ liftM DefStatement def
  , liftM DeclStatement decl
  ]

-------------------- Main --------------------

topLevel :: Parser Module
topLevel = moduleGuts <* P.eof

parse :: Parser a -> DumbLattice -> SourceName -> [AnnToken] -> Either ParseError a
parse p dl name = flip runReader (Env dl TopLevel) . P.runParserT p () name

parseConstraints :: DumbLattice
parseConstraints = compile [ (" ", "<-"), ("=[]=", "->") ]

runParse :: String -> Text -> Either ParseError Module
runParse name = P.parse tokenize name >=> parse topLevel parseConstraints name

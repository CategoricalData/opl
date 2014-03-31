module Lang.OPL.Syntax where

import Prelude ()
import FP
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lang.OPL.Annotated
import Text.Parsec (SourcePos)
import qualified FP.Pretty as P

---------- Names and Paths ----------

type NameAnn = SourcePos

newtype Name = Name { nameVal :: String }
  deriving (Eq, Ord, Show)
instance Pretty Name where
  pretty = P.string . nameVal

type AName = Annotated NameAnn Name

data Path = SingletonPath Name | Name :.: Path
  deriving (Eq, Ord, Show)
instance Pretty Path where
  pretty (SingletonPath n) = pretty n
  pretty (n :.: p) = do
    pretty n
    P.punctuation $ P.string "."
    pretty p

pathRoot :: Path -> Name
pathRoot (SingletonPath n) = n
pathRoot (n :.: _) = n

type APath = Annotated NameAnn Path

---------- Boxes ----------

newtype PlugType = PlugType AName
  deriving (Eq, Ord, Show)
instance Pretty PlugType where
  pretty (PlugType n) = pretty n

data BoxArrow = BoxArrow
  { boxArrowInputs :: [PlugType]
  , boxArrowOutputs :: [PlugType]
  } deriving (Eq, Ord, Show)
instance Pretty BoxArrow where
  pretty (BoxArrow ins outs) = 
    P.guardLevel (level "=[]=") 
    $ P.hsep 
    $ concat
      [ map pretty ins
      , return $ P.punctuation $ P.string "=[]="
      , map pretty outs
      ]

data Box =
    VarBox AName
  | ArrowBox BoxArrow
  deriving (Eq, Ord, Show)
instance Pretty Box where
  pretty (VarBox n) = pretty n
  pretty (ArrowBox ab) = pretty ab

---------- Wiring Diagrams ----------

data WiringArrow = WiringArrow
  { wiringArrowInputs :: [Box]
  , wiringArrowOutputs :: Box
  } deriving (Eq, Ord, Show)
instance Pretty WiringArrow where
  precLattice Proxy = compile [("=[]=", "->")] 
  pretty (WiringArrow ins out) = 
    P.guardLevel (level "->") 
    $ P.hsep 
    $ concat
      [ map (P.atLevel BotLevel . pretty) ins
      , return $ P.punctuation $ P.string "->"
      , return $ pretty out
      ]
data WiringType = 
    BoxWiringType Box
  | ArrowWiringType WiringArrow
  deriving (Eq, Ord, Show)
instance Pretty WiringType where
  pretty (BoxWiringType b) = pretty b
  pretty (ArrowWiringType a) = pretty a

data BoxBinder = BoxBinder
  { boxBinderInputs :: [AName]
  , boxBinderOutputs :: [AName]
  } deriving (Eq, Ord, Show)
instance Pretty BoxBinder where
  pretty (BoxBinder ins outs) =
    P.hsep $ concat
      [ map pretty ins
      , return $ P.punctuation $ P.string "=[]="
      , map pretty outs
      ]

data Plug = Plug
  { plugName :: AName
  , plugPath :: APath
  } deriving (Eq, Ord, Show)
instance Pretty Plug where
  pretty (Plug n p) =
    P.hsep
      [ pretty n
      , P.punctuation $ P.string "<-"
      , pretty p
      ]

data Wiring = Wiring
  { wiringName :: AName
  , wiringBox :: Box
  , wiringBoxBinder :: BoxBinder
  , wiringPlugs :: [Plug]
  } deriving (Eq, Ord, Show)
instance Pretty Wiring where
  pretty (Wiring n b bb ps) = do
    P.hsep
      [ P.binder $ pretty n
      , P.punctuation $ P.string ":"
      , do
          P.atLevel BotLevel $ pretty b
          P.punctuation $ P.string "["
          pretty bb
          P.punctuation $ P.string "]"
      , P.keyword $ P.string "plug"
      , do
          s <- askView P.styleOptionsL
          localViewSet P.styleOptionsL (P.StyleOptions P.PreAlignStyle P.NoBuffer 2) $
            P.encloseSepDropIndent "" "" ", " $ map (localViewSet P.styleOptionsL s . pretty) ps
      ]

data WiringDiagram = WiringDiagram
  { wiringDiagramInternalBoxes :: [Wiring]
  , wiringDiagramExternalBox :: Wiring
  } deriving (Eq, Ord, Show)
instance Pretty WiringDiagram where
  pretty (WiringDiagram ins out) = P.group $ do
    P.keyword $ P.string "wiring"
    P.dropIndent $
      P.vsep $ map P.group
        [ do
            P.keyword $ P.string "internal"
            P.dropIndent $ do
              P.vsep $ map pretty ins
        , do
            P.keyword $ P.string "external"
            P.dropIndent $ pretty out
        ]
    P.hardLine
    P.keyword $ P.string "end"

---------- Expressions ----------

data DefineType =
    WiringDefineType WiringType
  | LiftDefineType AName WiringType
  deriving (Eq, Ord, Show)
instance Pretty DefineType where
  pretty (WiringDefineType wt) = pretty wt
  pretty (LiftDefineType n wt) = P.hsep
    [ pretty n
    , P.punctuation $ P.string "@"
    , pretty wt
    ]

data Renaming = Renaming
  { renamingFrom :: [AName]
  , renamingTo :: [AName]
  } deriving (Eq, Ord, Show)
instance Pretty Renaming where
  pretty (Renaming from to) = P.hsep $ concat
    [ map pretty from
    , return $ P.punctuation $ P.string "=>"
    , map pretty to
    ]

data DefineExp =
    VarDefineExp AName
  | DiagramDefineExp WiringDiagram
  | LiftDefineExp AName DefineExp
  | RenamingDefineExp DefineExp Renaming
  | ApplyDefineExp DefineExp [Maybe DefineExp]
  deriving (Eq, Ord, Show)
instance Pretty DefineExp where
  precLattice Proxy = compile [(" ", "<-")]
  pretty (VarDefineExp n) = pretty n
  pretty (DiagramDefineExp wd) = pretty wd
  pretty (LiftDefineExp n e) = P.guardLevel (level " ") $ P.hsep
    [ pretty n
    , pretty e
    ]
  pretty (RenamingDefineExp e r) = do
    pretty e
    P.punctuation $ P.string "["
    pretty r
    P.punctuation $ P.string "]"
  pretty (ApplyDefineExp e eMs) = P.guardLevel (level "<-") $ P.hsep $ concat
    [ return $ pretty e
    , return $ P.punctuation $ P.string "<-"
    , map (P.atLevel BotLevel . maybe (P.punctuation $ P.string "_") pretty) eMs
    ]

---------- Modules ----------

data Import = Import
  { importPath :: APath
  , importQualified :: Bool
  , importApply :: Maybe [Statement]
  , importOnly :: Maybe [Decl]
  } deriving (Eq, Ord, Show)
instance Pretty Import where
  pretty (Import p q apsM osM) = P.group $ do
    P.hsep $ concat
      [ return $ P.keyword $ P.string "import"
      , return $ pretty p
      , if not q then mzero else return $ P.keyword $ P.string "qualified"
      ]
    P.dropIndent $
      P.vsep $ map P.group $ concat
        [ flip (maybe mzero) apsM $ \ aps -> return $ do
            P.keyword $ P.string "apply"
            P.dropIndent $
              P.vsep $ map pretty aps
        , flip (maybe mzero) osM $ \ os -> return $ do
            P.keyword $ P.string "only"
            P.dropIndent $
              P.vsep $ map pretty os
        ]

data Provides =
    AllProvides 
  | NoneProvides 
  | ExplicitProvides [Decl]
  deriving (Eq, Ord, Show)
instance Pretty Provides where
  pretty AllProvides = P.keyword $ P.string "all"
  pretty NoneProvides = P.keyword $ P.string "none"
  pretty (ExplicitProvides ds) = P.dropIndent $ P.vsep $ map pretty ds

data Module = Module
  { moduleRequires :: [Decl]
  , moduleImports :: [Import]
  , moduleProvides :: Maybe Provides
  , moduleStatements :: [Statement]
  } deriving (Eq, Ord, Show)
instance Pretty Module where
  precLattice Proxy =
    Map.unionsWith Set.union
      [ precLattice (proxy :: Proxy WiringArrow)
      , precLattice (proxy :: Proxy DefineExp)
      ]
  pretty (Module rs is pM ss) = P.group $ do
    P.keyword $ P.string "module"
    P.dropIndent $
      P.vsep $ concat
        [ if null rs
            then mzero
            else return $ P.group $ do
              P.keyword $ P.string "require"
              P.dropIndent $
                P.vsep $ map pretty rs
        , map pretty is
        , flip (maybe mzero) pM $ \ p -> return $ P.group $ do
            P.keyword $ P.string "provide"
            pretty p
        ]
    P.hardLine
    P.keyword $ P.string "where"
    P.dropIndent $ P.vsep $ map pretty ss
    P.hardLine
    P.keyword $ P.string "end"

---------- Top Level Definitions ----------

data Decl =
    AlgebraDecl AName
  | BoxDecl AName
  | ModuleDecl AName
  | DefineDecl AName DefineType
  deriving (Eq, Ord, Show)
instance Pretty Decl where
  pretty (AlgebraDecl n) = P.hsep
    [ P.keyword $ P.string "algebra"
    , pretty n
    ]
  pretty (BoxDecl n) = P.hsep
    [ P.keyword $ P.string "box"
    , pretty n
    ]
  pretty (ModuleDecl n) = P.hsep
    [ P.keyword $ P.string "module"
    , pretty n
    ]
  pretty (DefineDecl n t) = P.hsep
    [ P.keyword $ P.string "define"
    , pretty n
    , P.punctuation $ P.string ":"
    , pretty t
    ]

data Def =
    AlgebraDef AName AName
  | BoxDef AName Box
  | ModuleDef AName Module
  | DefineDef AName DefineExp
  deriving (Eq, Ord, Show)
instance Pretty Def where
  pretty (AlgebraDef n a) = P.hsep
    [ P.keyword $ P.string "algebra"
    , pretty n
    , P.punctuation $ P.string ":="
    , pretty a
    ]
  pretty (BoxDef n b) = P.hsep
    [ P.keyword $ P.string "box"
    , pretty n
    , P.punctuation $ P.string ":="
    , pretty b
    ]
  pretty (ModuleDef n m) = do
    P.hsep
      [ P.keyword $ P.string "module"
      , pretty n
      , P.punctuation $ P.string ":="
      , pretty m
      ]
  pretty (DefineDef n e) = P.hsep
    [ P.keyword $ P.string "define"
    , pretty n
    , P.punctuation $ P.string ":="
    , pretty e
    ]

data Statement =
    DeclStatement Decl 
  | DefStatement Def
  deriving (Eq, Ord, Show)
instance Pretty Statement where
  pretty (DeclStatement d) = pretty d
  pretty (DefStatement d) = pretty d


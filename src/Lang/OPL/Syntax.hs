module Lang.OPL.Syntax where

import Prelude ()
import FP
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FP.Pretty as P
import Lang.OPL.Common

---------- Boxes ----------

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
    VarWiringType AName
  | LiftWiringType AName WiringType
  | BoxWiringType Box
  | ArrowWiringType WiringArrow
  deriving (Eq, Ord, Show)
instance Pretty WiringType where
  pretty (VarWiringType n) = pretty n
  pretty (LiftWiringType n wt) = P.hsep
    [ pretty n
    , P.punctuation $ P.string "@"
    , pretty wt
    ]
  pretty (BoxWiringType b) = pretty b
  pretty (ArrowWiringType a) = pretty a

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
          localViewSet P.styleOptionsL (P.StyleOptions P.PostStyle P.NoBuffer 2) $
            P.encloseSepDropIndent "" "" ", " $ map (localViewSet P.styleOptionsL s . pretty) ps
      ]

data WiringDiagram = WiringDiagram
  { wiringDiagramInternalBoxes :: [Wiring]
  , wiringDiagramExternalBox :: Wiring
  } deriving (Eq, Ord, Show)
instance Pretty WiringDiagram where
  pretty (WiringDiagram ins out) = P.group $ do
    P.keyword $ P.string "wiring"
    P.vsep $
      [ P.dropIndent $
          P.vsep $ map P.group
            [ do
                P.keyword $ P.string "internal"
                P.dropIndent $ do
                  P.vsep $ map pretty ins
            , do
                P.keyword $ P.string "external"
                P.dropIndent $ pretty out
            ]
      , P.keyword $ P.string "end"
      ]

newtype Underscored a = Underscored { unUnderscored :: Maybe a }
instance (Pretty a) => Pretty (Underscored a) where
  pretty (Underscored Nothing) = P.string "_"
  pretty (Underscored (Just x)) = pretty x

data WiringExp =
    VarWiringExp AName
  | LiftWiringExp AName WiringExp
  | DiagramWiringExp WiringDiagram
  | RenamingWiringExp Renaming WiringExp
  | ApplyWiringExp WiringExp [Maybe WiringExp]
  deriving (Eq, Ord, Show)
instance Pretty WiringExp where
  precLattice Proxy = compile [(" ", "<-")]
  pretty (VarWiringExp n) = pretty n
  pretty (DiagramWiringExp wd) = pretty wd
  pretty (LiftWiringExp n e) = P.guardLevel (level " ") $ P.hsep
    [ pretty n
    , pretty e
    ]
  pretty (RenamingWiringExp e r) = do
    pretty e
    P.punctuation $ P.string "["
    pretty r
    P.punctuation $ P.string "]"
  pretty (ApplyWiringExp e eMs) = P.guardLevel (level "<-") $ P.hsep $ concat
    [ return $ pretty e
    , return $ P.punctuation $ P.string "<-"
    , map (P.atLevel BotLevel . pretty . Underscored) eMs
    ]

---------- Top Level Definitions ----------

data Decl =
    AlgebraDecl AName
  | ModuleDecl AName
  | BoxDecl AName
  | WiringDecl AName WiringType
  deriving (Eq, Ord, Show)
instance Pretty Decl where
  pretty (AlgebraDecl n) = P.hsep
    [ P.keyword $ P.string "algebra"
    , pretty n
    ]
  pretty (ModuleDecl n) = P.hsep
    [ P.keyword $ P.string "module"
    , pretty n
    ]
  pretty (BoxDecl n) = P.hsep
    [ P.keyword $ P.string "box"
    , pretty n
    ]
  pretty (WiringDecl n t) = P.hsep
    [ P.keyword $ P.string "define"
    , pretty n
    , P.punctuation $ P.string ":"
    , pretty t
    ]

data Def =
    AlgebraDef AName AName
  | ModuleDef AName Module
  | BoxDef AName Box
  | WiringDef AName WiringExp
  deriving (Eq, Ord, Show)
instance Pretty Def where
  pretty (AlgebraDef n a) = P.hsep
    [ P.keyword $ P.string "algebra"
    , pretty n
    , P.punctuation $ P.string ":="
    , pretty a
    ]
  pretty (ModuleDef n m) = do
    P.hsep
      [ P.keyword $ P.string "module"
      , pretty n
      , P.punctuation $ P.string ":="
      , pretty m
      ]
  pretty (BoxDef n b) = P.hsep
    [ P.keyword $ P.string "box"
    , pretty n
    , P.punctuation $ P.string ":="
    , pretty b
    ]
  pretty (WiringDef n e) = P.hsep
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
  pretty (ExplicitProvides ds) = P.group $ P.dropIndent $ P.vsep $ map pretty ds

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
      , precLattice (proxy :: Proxy WiringExp)
      ]
  pretty (Module rs is pM ss) = P.group $ do
    case (rs, is, pM) of
      ([], [], Nothing) ->
        P.dropIndent $ P.vsep $ map pretty ss
      _ -> do
        P.dropIndent $ P.vsep $ concat
          [ if null rs then mzero else 
              return $ P.group $ do
                P.keyword $ P.string "require"
                P.dropIndent $ P.vsep $ map pretty rs
          , map pretty is
          , flip (maybe mzero) pM $ \ p -> return $ P.hsep
              [ P.keyword $ P.string "provide"
              , pretty p
              ]
          ]
        P.hardLine
        P.keyword $ P.string "where"
        P.dropIndent $ P.vsep $ map pretty ss
    P.hardLine
    P.keyword $ P.string "end"

newtype TLModule = TLModule Module
  deriving (Eq, Ord, Show)
instance (Pretty TLModule) where
  pretty (TLModule (Module rs is pM ss)) = P.vsep $
    case (rs, is, pM) of
      ([], [], Nothing) -> map pretty ss
      _ -> concat
        [ if null rs then mzero else
            return $ P.group $ do
              P.keyword $ P.string "require"
              P.dropIndent $ P.vsep $ map pretty rs
        , map pretty is
        , flip (maybe mzero) pM $ \ p -> return $ P.hsep
            [ P.keyword $ P.string "provide"
            , pretty p
            ]
        , return $ P.keyword $ P.string "where"
        , map pretty ss
        ]


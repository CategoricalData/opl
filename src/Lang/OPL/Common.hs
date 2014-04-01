module Lang.OPL.Common where

import Prelude ()
import FP
import Text.Parsec (SourcePos)
import qualified FP.Pretty as P
import Lang.OPL.Annotated

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

newtype PlugType = PlugType { getPlugType :: AName }
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

---------- Wiring ----------

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


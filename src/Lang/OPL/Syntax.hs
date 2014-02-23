module Lang.OPL.Syntax where

import Lang.OPL.Annotated
import Text.Parsec (SourcePos)

type Ann = SourcePos

type Name = String
type AName = Annotated Ann Name

data Path = SingletonPath Name | Name :.: Path
  deriving (Eq, Ord, Show)

pathRoot :: Path -> Name
pathRoot (SingletonPath n) = n
pathRoot (n :.: _) = n

prettyPath :: Path -> String
prettyPath (SingletonPath n) = n
prettyPath (n :.: p) = n ++ "." ++ prettyPath p

type APath = Annotated Ann Path

type Type = AName
data Mapping = Mapping
  { mappingKey :: AName
  , mappingValue :: AName
  } deriving (Eq, Ord, Show)

mappingToTuple :: Mapping -> (AName, AName)
mappingToTuple (Mapping key value) = (key, value)

lookupMapping :: AName -> [Mapping] -> Maybe AName
lookupMapping n = lookup n . map mappingToTuple

data Binder = Binder
  { binderName :: AName
  , binderType :: Type
  } deriving (Eq, Ord, Show)

binderToTuple :: Binder -> (AName, Type)
binderToTuple (Binder n t) = (n, t)

lookupBinder :: AName -> [Binder] -> Maybe Type
lookupBinder n = lookup n . map binderToTuple

data Plug = Plug
  { plugName :: AName
  , plugPath :: APath
  } deriving (Eq, Ord, Show)
type Export = Mapping

data Box = Box
  { boxInputs :: [Binder]
  , boxOutputs :: [Binder]
  } deriving (Eq, Ord, Show)

data Wiring = Wiring
  { wiringBinder :: Binder
  , wiringPlugs :: [Plug]
  } deriving (Eq, Ord, Show)

wiringName :: Wiring -> AName
wiringName = binderName . wiringBinder

data WiringDiagram = WiringDiagram
  { wiringDiagramInternalBoxes :: [Wiring]
  , wiringDiagramExternalBox :: Wiring
  } deriving (Eq, Ord, Show)

data BoxBinder = BoxBinder
  { boxBinderName :: AName
  , boxBinderBox :: Box
  } deriving (Eq, Ord, Show)

boxBinderToTuple :: BoxBinder -> (AName, Box)
boxBinderToTuple (BoxBinder aname box) = (aname, box)

lookupBoxBinder :: AName -> [BoxBinder] -> Maybe Box
lookupBoxBinder n = lookup n . map boxBinderToTuple

data WiringDiagramType = WiringDiagramType
  { wiringDiagramTypeInternalBoxBinders :: [BoxBinder]
  , wiringDiagramTypeExternalBox :: Box
  } deriving (Eq, Ord, Show)

data Filling = Filling
  { fillingMapping :: Mapping
  , fillingExports :: [Export]
  } deriving (Eq, Ord, Show)

data WiringComposition = WiringComposition
  { wiringCompositionExternalWdName :: AName
  , wiringCompositionInternalWdFillings :: [Filling]
  } deriving (Eq, Ord, Show)

data Def =
    BoxDef AName Box
  | WiringDiagramDef AName WiringDiagram
  | WiringCompositionDef AName WiringComposition
  deriving (Eq, Ord, Show)

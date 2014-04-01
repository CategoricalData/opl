module Lang.OPL.Semantics where

import qualified Lang.OPL.Syntax as S
import Prelude ()
import FP
import Lang.OPL.Common

class ToS a b | a -> b where
  toS :: a -> b
prettyS :: (ToS a b, Pretty b, MonadPretty m) => a -> m ()
prettyS = pretty . toS

---------- Boxes ----------

data Box = Box
  { boxInputs :: [PlugType]
  , boxOutputs :: [PlugType]
  } deriving (Eq, Ord, Show)
instance ToS Box S.Box where
  toS (Box ins outs) = S.ArrowBox $ BoxArrow ins outs
instance Pretty Box where pretty = prettyS

---------- Wiring Diagrams ----------

data WiringArrow = WiringArrow
  { wiringArrowInputs :: [Box]
  , wiringArrowOutput :: Box
  } deriving (Eq, Ord, Show)
instance ToS WiringArrow S.WiringArrow where
  toS (WiringArrow ins out) = S.WiringArrow (map toS ins) (toS out)
instance Pretty WiringArrow where pretty = prettyS

data WiringType =
    LiftWiringType AName WiringType
  | BoxWiringType Box
  | ArrowWiringType WiringArrow
  deriving (Eq, Ord, Show)
instance ToS WiringType S.WiringType where
  toS (LiftWiringType n wt) = S.LiftWiringType n (toS wt)
  toS (BoxWiringType b) = S.BoxWiringType (toS b)
  toS (ArrowWiringType a) = S.ArrowWiringType (toS a)
instance Pretty WiringType where pretty = prettyS

data Wiring = Wiring
  { wiringName :: AName
  , wiringBox :: Box
  , wiringBoxBinder :: BoxBinder
  , wiringPlugs :: [Plug]
  } deriving (Eq, Ord, Show)
instance ToS Wiring S.Wiring where
  toS (Wiring n b bb ps) = S.Wiring n (toS b) bb ps
instance Pretty Wiring where pretty = prettyS

data WiringDiagram = WiringDiagram
  { wiringDiagramInternalBoxes :: [Wiring]
  , wiringDiagramExternalBox :: Wiring
  } deriving (Eq, Ord, Show)
instance ToS WiringDiagram S.WiringDiagram where
  toS (WiringDiagram ins out) = S.WiringDiagram (map toS ins) (toS out)
instance Pretty WiringDiagram where pretty = prettyS

data WiringExp =
    DiagramWiringExp WiringDiagram
  | LiftWiringExp AName WiringExp
  | RenamingWiringExp Renaming WiringExp
  | ApplyWiringExp WiringExp [Maybe WiringExp]
  deriving (Eq, Ord, Show)
instance ToS WiringExp S.WiringExp where
  toS (DiagramWiringExp wd) = S.DiagramWiringExp (toS wd)
  toS (LiftWiringExp n wd) = S.LiftWiringExp n (toS wd)
  toS (RenamingWiringExp r wd) = S.RenamingWiringExp r (toS wd)
  toS (ApplyWiringExp wd wdMs) = S.ApplyWiringExp (toS wd) (fmap (fmap toS) wdMs)
instance Pretty WiringExp where pretty = prettyS

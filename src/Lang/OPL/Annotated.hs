module Lang.OPL.Annotated where

import Prelude ()
import FP
import Text.Parsec (ParsecT, SourcePos)
import qualified Text.Parsec as P

data Annotated a t = Annotated
  { annotation :: a
  , stripAnnotation :: t
  } deriving (Show)

instance (Eq t) => Eq (Annotated a t) where
  (==) = (==) `on` stripAnnotation
instance (Ord t) => Ord (Annotated a t) where
  compare = compare `on` stripAnnotation
instance (Pretty t) => Pretty (Annotated a t) where
  pretty = pretty . stripAnnotation

annotate :: (P.Stream s m t) => ParsecT s u m a -> ParsecT s u m (Annotated SourcePos a)
annotate p = do
  l <- P.getPosition
  liftM (Annotated l) p


module Util.Parsec where

import Text.Parsec (ParseError)
import Util.Either

ioParser :: (String -> a -> Either ParseError b) -> (String -> a -> IO b)
ioParser p n i = eitherToIO $ p n i

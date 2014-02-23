module Util.Either where

eitherToIO :: (Show e) => Either e a -> IO a
eitherToIO aM =
  case aM of
    Left e -> fail $ show e
    Right a -> return a

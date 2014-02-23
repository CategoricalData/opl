module Lang.OPL.Message where

import Prelude()
import FP
import qualified Data.List as List
import qualified FP.Pretty as P
import Text.Parsec (SourcePos)
import System.Console.ANSI

type Context = [(SourcePos, String)]
data Message = Message
  { messagePhase :: String
  , messageContext :: Context
  , messageTitle :: String
  , messageDescription :: Maybe String
  } deriving (Eq, Ord, Show)

instance Pretty Message where
  pretty m = do
    P.text "error during phase: " 
    P.localConsole (mappend $ setConsoleColor Dull Magenta) $ 
      P.string (messagePhase m)
    P.hardLine
    let reason = do
          P.localConsole (mappend $ setConsoleColor Dull Red) $
            P.string $ messageTitle m
          case messageDescription m of
            Nothing -> return ()
            Just d -> do
              P.hardLine
              P.string d
    (\ f -> List.foldl' f reason $ messageContext m) $ \ i (l, s) -> do
      P.text "in " 
      P.localConsole (mappend $ setConsoleColor Dull Cyan) $
        P.string s 
      P.localConsole (mappend $ setConsoleColor Dull Yellow) $ do
        P.text " ["
        P.string $ show l
        P.text "]"
      P.hardLine
      P.space 2 >> P.align i



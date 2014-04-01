module Lang.OPL.Message where

import Prelude()
import FP
import qualified Data.List as List
import qualified FP.Pretty as P
import Text.Parsec (SourcePos)
import System.Console.ANSI

type Context = [(Maybe SourcePos, PrettyString)]
data Message = Message
  { messagePhase :: PrettyString
  , messageContext :: Context
  , messageTitle :: PrettyString
  , messageDescription :: Maybe PrettyString
  } deriving (Eq, Ord, Show)

instance Pretty Message where
  pretty m = do
    P.text "error during phase: " 
    P.localConsole (mappend $ setConsoleColor Dull Magenta) $ pretty $ messagePhase m
    P.hardLine
    let reason = do
          P.localConsole (mappend $ setConsoleColor Dull Red) $ pretty $ messageTitle m
          case messageDescription m of
            Nothing -> return ()
            Just d -> do
              P.hardLine
              pretty d
    (\ f -> List.foldl' f reason $ messageContext m) $ \ i (lM, s) -> do
      P.text "in " 
      P.localConsole (mappend $ setConsoleColor Dull Cyan) $
        pretty s 
      P.localConsole (mappend $ setConsoleColor Dull Yellow) $
        case lM of
          Nothing -> return ()
          Just l -> do
            P.text " ["
            P.string $ show l
            P.text "]"
      P.hardLine
      P.space 2 >> P.align i



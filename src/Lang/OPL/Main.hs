module Main where

import Prelude ()
import FP
import System.Environment
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Text.Parsec as P
import Util.Parsec
import Lang.OPL.Lexer
import Lang.OPL.Parser
import Lang.OPL.Check
import Lang.OPL.CheckMonad
import Control.Applicative

sampleFile :: FilePath
sampleFile = "opl_source/1.opl"

mainTokenize :: FilePath -> IO ()
mainTokenize path = do
  input <- T.readFile path
  ts <- ioParser runTokenize sampleFile input
  putStrLn $ show ts

mainParse :: FilePath -> IO ()
mainParse path = do
  input <- T.readFile path
  ds <- ioParser runParse sampleFile input
  -- ds <- ioParser runParse "<>" "import lib2 apply box boxy := A only box B"
  pprintLn ds
  where
      runParse' name = P.parse tokenize name >=> parse pimport parseConstraints name

-- mainTypeCheck :: FilePath -> IO ()
-- mainTypeCheck path = do
--   input <- T.readFile path
--   ds <- ioParser runParse sampleFile input
--   execCheckMonadIO0 $ check ds
--   putStrLn $ "SUCCESS"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> mainParse path
    _ -> putStrLn "expecting: opl <filename>"

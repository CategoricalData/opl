module Main where

import Prelude ()
import FP
import System.Environment
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified FP.Pretty as P
import Util.Parsec
import Lang.OPL.Lexer
import Lang.OPL.Parser
import Lang.OPL.Check
import Lang.OPL.CheckMonad
import Control.Applicative

mainTokenize :: FilePath -> IO ()
mainTokenize path = do
  input <- T.readFile path
  ts <- ioParser runTokenize path input
  putStrLn $ show ts

mainParse :: FilePath -> IO ()
mainParse path = do
  input <- T.readFile path
  m <- ioParser runParse path input
  pprintLn m
  where
      runParse' name = P.parse tokenize name >=> parse pimport parseConstraints name

mainTypeCheck :: FilePath -> IO ()
mainTypeCheck path = do
  input <- T.readFile path
  m <- ioParser runParse path input
  execCheckMonadIO0 $ check m
  putStrLn "SUCCESS"
  putStrLn "<printing program>"
  pprintLn m

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> mainTypeCheck path
    _ -> putStrLn "expecting: opl <filename>"

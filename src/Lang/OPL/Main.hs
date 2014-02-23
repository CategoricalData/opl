module Main where

import System.Environment
import qualified Data.Text.IO as T
import qualified Text.Parsec as P
import Util.Parsec
import Lang.OPL.Lexer
import Lang.OPL.Parser
import Lang.OPL.TypeChecker
import Lang.OPL.TypeCheckMonad
import Control.Applicative

sampleFile :: FilePath
sampleFile = "opl_source/sample.opl"

mainTokenize :: FilePath -> IO ()
mainTokenize path = do
  input <- T.readFile path
  ts <- ioParser runTokenize sampleFile input
  putStrLn $ show ts

mainParse :: FilePath -> IO ()
mainParse path = do
  input <- T.readFile path
  ds <- ioParser runParse sampleFile input
  putStrLn $ show ds

mainTypeCheck :: FilePath -> IO ()
mainTypeCheck path = do
  input <- T.readFile path
  ds <- ioParser runParse sampleFile input
  s <- execCheckMonadIO checkEnv0 checkState0 $ check ds
  putStrLn $ show s

main :: IO ()
main = do
  [path] <- getArgs
  mainTypeCheck path

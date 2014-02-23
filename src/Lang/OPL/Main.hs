module Main where

import System.Environment
import qualified Data.Text.IO as T
import qualified Text.Parsec as P
import Util.Parsec
import Lang.OPL.Lexer
import Lang.OPL.Parser
import Lang.OPL.Check
import Lang.OPL.CheckMonad
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
  execCheckMonadIO0 $ check ds
  putStrLn $ "SUCCESS"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> mainTypeCheck path
    _ -> putStrLn "expecting: opl <filename>"

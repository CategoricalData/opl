module Main where

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

mainTokenize :: IO ()
mainTokenize = do
  input <- T.readFile sampleFile
  ts <- ioParser runTokenize sampleFile input
  putStrLn $ show ts

mainParse :: IO ()
mainParse = do
  input <- T.readFile sampleFile
  ds <- ioParser runParse sampleFile input
  putStrLn $ show ds

mainTypeCheck :: IO ()
mainTypeCheck = do
  input <- T.readFile sampleFile
  ds <- ioParser runParse sampleFile input
  s <- execCheckMonadIO checkEnv0 checkState0 $ check ds
  putStrLn $ show s

main :: IO ()
main = mainTypeCheck

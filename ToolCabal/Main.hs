module Main where

import Text.ParserCombinators.UU.Utils

import Tools.Core.Types
import Tools.BasicTools.Types
import Tools.Cabal.BuildInfo

import Tools.Cabal.Shake
import Tools.Cabal.FileFormatParser
import Cabal

main :: IO ()
main = do
  src <- readFile "pkg.toolpkg"
  ff <- return $ runParser "Parse FileFormat" pFileFormat src
  mainShake ff $ BuildInfo "_build" (KnownPackageManager GHC) deafening ["bytecode"] ["plain"]
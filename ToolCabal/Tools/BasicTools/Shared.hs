module Tools.BasicTools.Shared 
(
  module Tools.BasicTools.Types,
  module Tools.BasicTools.Shared
)
where

import Control.Monad.Except

import Tools.Core.ToolMonad
import Tools.Core.Plugin
import Tools.Core.Types
import Tools.BasicTools.Types

import Cabal

storeBasicTools :: ToolMonad ()
storeBasicTools = do
  storeOutputTool "haddock" Haddock
  storeOutputTool "HsColour" HsColour
  storePreProcessor "uuagc" UUAGC
  storePreProcessor "ccphs" CppHs
  storePackageManager "ghc" GHC
  storePackageManager "uhc" UHC

makeBasicConfiguration :: String -> ToolMonad ()
makeBasicConfiguration s = 
  catchError (do 
    loadFromStore s
    makeConfiguration)
  (\_ -> do
  storeBasicTools
  loadFromStore s
  makeConfiguration)

findPreProcessorOrNewPlugin :: String -> KnownPreProcessor
findPreProcessorOrNewPlugin s = case s of
    "uuagc" -> KnownPreProcessor UUAGC
    "ccphs" -> KnownPreProcessor CppHs
    "ghc" -> KnownPreProcessor GHC
    "uhc" -> KnownPreProcessor UHC
    _ -> KnownPreProcessor $ newPlugin' True True False (simpleConfiguredProgram s (FoundOnSystem s)) s

findPackageManagerOrNewPlugin :: String -> KnownPackageManager
findPackageManagerOrNewPlugin s = case s of
    "ghc" -> KnownPackageManager GHC
    "uhc" -> KnownPackageManager UHC
    _ -> KnownPackageManager $ newPlugin' True True True (simpleConfiguredProgram s (FoundOnSystem s)) s
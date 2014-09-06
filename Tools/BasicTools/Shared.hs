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
  storeCompiler "ghc" GHC
  storeCompiler "uhc" UHC

makeBasicConfiguration :: String -> ToolMonad ()
makeBasicConfiguration s = 
  catchError (do 
    loadFromStore s
    makeConfiguration)
  (\_ -> do
  storeBasicTools
  loadFromStore s
  makeConfiguration)

findToolOrNewPlugin :: String -> KnownTool
findToolOrNewPlugin s = case s of
    "haddock" -> KnownTool Haddock
    "HsColour" -> KnownTool HsColour
    "uuagc" -> KnownTool UUAGC
    "ccphs" -> KnownTool CppHs
    "ghc" -> KnownTool GHC
    "uhc" -> KnownTool UHC
    _ -> KnownTool $ newPlugin' False False False (simpleConfiguredProgram s (FoundOnSystem s)) s

findOutputToolOrNewPlugin :: String -> KnownOutputTool
findOutputToolOrNewPlugin s = case s of
    "haddock" -> KnownOutputTool Haddock
    "HsColour" -> KnownOutputTool HsColour
    "uuagc" -> KnownOutputTool UUAGC
    "ccphs" -> KnownOutputTool CppHs
    "ghc" -> KnownOutputTool GHC
    "uhc" -> KnownOutputTool UHC
    _ -> KnownOutputTool $ newPlugin' True False False (simpleConfiguredProgram s (FoundOnSystem s)) s

findPreProcessorOrNewPlugin :: String -> KnownPreProcessor
findPreProcessorOrNewPlugin s = case s of
    "uuagc" -> KnownPreProcessor UUAGC
    "ccphs" -> KnownPreProcessor CppHs
    "ghc" -> KnownPreProcessor GHC
    "uhc" -> KnownPreProcessor UHC
    _ -> KnownPreProcessor $ newPlugin' True True False (simpleConfiguredProgram s (FoundOnSystem s)) s

findCompilerOrNewPlugin :: String -> KnownCompiler
findCompilerOrNewPlugin s = case s of
    "ghc" -> KnownCompiler GHC
    "uhc" -> KnownCompiler UHC
    _ -> KnownCompiler $ newPlugin' True True True (simpleConfiguredProgram s (FoundOnSystem s)) s
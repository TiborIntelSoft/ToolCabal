{-# LANGUAGE TemplateHaskell #-}

module Tools.Cabal.Haddock where

import Control.Lens
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate)
import System.FilePath
import System.Directory

import Tools.Core.ToolMonad
import qualified Tools.Core.TypedMonad as TM
import Tools.BasicTools.Shared
import Tools.BasicTools.Haddock

data HaddockSettings = HaddockSettings 
  {
    _useHsColour :: Bool,
    _inputFiles :: [String],
    _documentationDir :: String,
    _colorSrcDir :: String
  }

makeLenses ''HaddockSettings

cabalHaddock :: HaddockSettings -> ToolMonad String
cabalHaddock settings = do
  when (settings ^. useHsColour) (cabalHsColour settings)
  makeBasicConfiguration "haddock"
  setOutputDir $ settings ^. documentationDir
  when (settings ^. useHsColour) (do
    mapM addFlag $ S.toList $ haddockHsColourFlags $ settings ^. colorSrcDir
    return ()
    )
  mapM setArgument $ settings ^. inputFiles
  setSetting "h"
  validateFlags
  liftIO $ createDirectoryIfMissing True $ settings ^. documentationDir
  invoke

cabalHsColour :: HaddockSettings -> ToolMonad ()
cabalHsColour settings = do
  makeBasicConfiguration "HsColour"
  liftIO $ createDirectoryIfMissing True $ bdir
  hc1 <- getCurrent
  hc2 <- TM.setSetting "print-css" hc1
  hc3 <- TM.setOutputFile (bdir </> "hscolour.css") hc2
  TM.invoke hc3
  --setOutputFile $ settings ^. colorSrcDir
  setSetting "css"
  setSetting "anchor"
  hsColour <- getCurrent
  hsc <- mapM (setArgAndOutput bdir hsColour) $ settings ^. inputFiles
  ret <- mapM TM.invoke hsc
  --warn $ intercalate "\n" ret
  return ()
  --return $ intercalate "\n" ret
  where bdir = settings ^. documentationDir </> settings ^. colorSrcDir
setArgAndOutput :: String -> AnyTool -> String -> ToolMonad AnyTool
setArgAndOutput bdir ct s = do
  ct1 <- TM.setArgument s ct
  TM.setOutputFile (bdir </> ss) ct1
  where ss = addExtension (dropExtension s) "html"
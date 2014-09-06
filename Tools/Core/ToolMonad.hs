module Tools.Core.ToolMonad 
(
  -- * ToolMonad functions
  module Tools.Core.ToolMonad,
  C.run,
  C.runError,
  C.storeTool,
  C.storeOutputTool,
  C.storePreProcessor,
  C.storeCompiler,
  C.store,
  C.storeAnyTool,
  C.load,
  C.loadAnyTool,
  C.loadFromStore,
  C.save,
  C.setSaveName,
  C.getCurrent,
  C.makeConfiguration,
  C.findFlags,
  C.findFlag,
  C.findSetFlags,
  C.findSetFlag,
  C.setSetting,
  C.resetSetting,
  C.addSetting,
  C.replaceSetting,
  C.removeSetting,
  C.validateFlags,
  C.addFlag,
  C.replaceFlag,
  C.removeFlag,
  C.invoke,
  C.setPreProcess,
  C.printCurrent,
  C.setArgument,
  C.warn,
  C.debug,
  C.notice,
  C.info,
  C.withOtherCurrent,

  -- * Data types 
  ToolMonad,
  C.AnyTool
  )
where

import Data.Set (Set)
import qualified Data.Set as S

import Tools.Flag
import Cabal (Verbosity)

import Tools.Core.Core (ToolMonad, Target)
import qualified Tools.Core.Core as C

import Debug

-- | Alias for @saveCurrent@
save' :: String -> ToolMonad ()
save' = C.saveCurrent

-- | Alias for @addInputFileC@
addInputFile :: String -> ToolMonad ()
addInputFile = C.addInputFileC

-- | Alias for @setOutputFileC@
setOutputFile :: String -> ToolMonad ()
setOutputFile = C.setOutputFileC

-- | Alias for @setOutputDirC@
setOutputDir :: String -> ToolMonad ()
setOutputDir = C.setOutputDirC

-- | Alias for @addInputDirC@
addInputDir :: String -> ToolMonad ()
addInputDir = C.addInputDirC

-- | Alias for @prepareTransFormC@
prepareTransForm :: Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ()
prepareTransForm = C.prepareTransFormC

getHidePkgsFlag :: ToolMonad Flag
getHidePkgsFlag = C.getHidePkgsFlagC

getPackageFlag :: String -> ToolMonad Flag
getPackageFlag = C.getPackageFlagC
module Tools.Core.ToolMonad 
(
  -- * ToolMonad functions
  module Tools.Core.ToolMonad,
  C.run,
  C.runError,
  C.storeTool,
  C.storeOutputTool,
  C.storePreProcessor,
  C.storePackageManager,
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

save' :: String -> ToolMonad ()
save' = C.saveC

setInputFile :: String -> ToolMonad ()
setInputFile = C.setInputFileC

setOutputFile :: String -> ToolMonad ()
setOutputFile = C.setOutputFileC

setOutputDir :: String -> ToolMonad ()
setOutputDir = C.setOutputDirC

addInputDir :: String -> ToolMonad ()
addInputDir = C.addInputDirC

prepareTransForm :: Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ()
prepareTransForm = C.prepareTransFormC
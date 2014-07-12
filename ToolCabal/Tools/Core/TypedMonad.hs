module Tools.Core.TypedMonad 
(
  -- * ToolMonad functions
  module Tools.Core.TypedMonad,
  C.run,
  C.runError,
  C.storeTool,
  C.storeOutputTool,
  C.storePreProcessor,
  C.store,
  C.storeAnyTool,
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

import Tools.Core.Core (ToolMonad, AnyTool, Target, ConfiguredTool)
import qualified Tools.Core.Core as C

loadFromStore :: String -> ToolMonad AnyTool
loadFromStore = C.loadFromStore'

makeConfiguration :: AnyTool -> ToolMonad AnyTool
makeConfiguration = C.makeConfiguration'

findFlags :: String -> AnyTool -> ToolMonad [Flag]
findFlags = C.findFlags'

findFlag :: String -> AnyTool -> ToolMonad Flag
findFlag = C.findFlag'

findSetFlags :: String -> AnyTool -> ToolMonad [Flag]
findSetFlags = C.findSetFlags'

findSetFlag :: String -> String -> AnyTool -> ToolMonad Flag
findSetFlag = C.findSetFlag'

setSetting :: String -> AnyTool -> ToolMonad AnyTool
setSetting = C.setSetting'

resetSetting :: String  -> AnyTool -> ToolMonad AnyTool
resetSetting = C.resetSetting'

addSetting :: String -> String -> AnyTool -> ToolMonad AnyTool
addSetting = C.addSetting'

replaceSetting :: String -> String -> AnyTool -> ToolMonad AnyTool
replaceSetting = C.replaceSetting'

removeSetting :: String -> String -> AnyTool -> ToolMonad AnyTool
removeSetting = C.removeSetting'

validateFlags :: AnyTool -> ToolMonad Bool
validateFlags = C.validateFlags'

addFlag :: Flag -> AnyTool -> ToolMonad AnyTool
addFlag = C.addFlag'

replaceFlag :: Flag -> Flag -> AnyTool -> ToolMonad AnyTool
replaceFlag = C.replaceFlag'

removeFlag :: Flag -> AnyTool -> ToolMonad AnyTool
removeFlag = C.removeFlag'

invoke :: AnyTool -> ToolMonad String
invoke = C.invoke'

setPreProcess :: String -> String -> Set Flag -> AnyTool -> ToolMonad AnyTool
setPreProcess = C.setPreProcess'

setArgument :: String -> AnyTool -> ToolMonad AnyTool
setArgument = C.setArgument'

setInputFile :: String -> AnyTool -> ToolMonad AnyTool
setInputFile = C.setInputFile'

setOutputFile :: String -> AnyTool -> ToolMonad AnyTool
setOutputFile = C.setOutputFile'

setOutputDir :: String -> AnyTool -> ToolMonad AnyTool
setOutputDir = C.setOutputDir'

addInputDir :: String -> AnyTool -> ToolMonad AnyTool
addInputDir = C.addInputDir'

prepareTransForm :: Target -> String -> FilePath -> [[FilePath]] -> AnyTool -> ToolMonad ConfiguredTool
prepareTransForm = C.prepareTransForm'
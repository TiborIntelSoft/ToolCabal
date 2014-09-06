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
  C.ToolMonad,
  C.AnyTool,
  C.StateActions(..)
  )
where

import Data.Set (Set)
import qualified Data.Set as S

import Tools.Flag

import Tools.Core.Core (ToolMonad, AnyTool, Target, ConfiguredTool, StateActions)
import qualified Tools.Core.Core as C

-- | Alias for @loadFromStore'@
loadFromStore :: String -> ToolMonad AnyTool
loadFromStore = C.loadFromStore'

-- | Alias for @makeConfiguration'@
makeConfiguration :: StateActions s => s -> ToolMonad AnyTool
makeConfiguration = C.makeConfiguration'

-- | Alias for @findFlags'@
findFlags :: StateActions s => s -> String -> ToolMonad [Flag]
findFlags = C.findFlags'

-- | Alias for @findFlag'@
findFlag :: StateActions s => s -> String -> ToolMonad Flag
findFlag = C.findFlag'

-- | Alias for @findSetFlags'@
findSetFlags :: StateActions s => s -> String -> ToolMonad [Flag]
findSetFlags = C.findSetFlags'

-- | Alias for @findSetFlag'@
findSetFlag :: StateActions s => s -> String -> String -> ToolMonad Flag
findSetFlag = C.findSetFlag'

-- | Alias for @setSetting'@
setSetting :: StateActions s => s -> String -> ToolMonad s
setSetting = C.setSetting'

-- | Alias for @resetSetting'@
resetSetting :: StateActions s => s -> String  -> ToolMonad s
resetSetting = C.resetSetting'

-- | Alias for @addSetting'@
addSetting :: StateActions s => s -> String -> String -> ToolMonad s
addSetting = C.addSetting'

-- | Alias for @replaceSetting'@
replaceSetting :: StateActions s => s -> String -> String -> ToolMonad s
replaceSetting = C.replaceSetting'

-- | Alias for @removeSetting'@
removeSetting :: StateActions s => s -> String -> String -> ToolMonad s
removeSetting = C.removeSetting'

-- | Alias for @validateFlags'@
validateFlags :: StateActions s => s -> ToolMonad Bool
validateFlags = C.validateFlags'

-- | Alias for @addFlag'@
addFlag :: StateActions s => s -> Flag -> ToolMonad s
addFlag = C.addFlag'

-- | Alias for @replaceFlag'@
replaceFlag :: StateActions s => s -> Flag -> Flag -> ToolMonad s
replaceFlag = C.replaceFlag'

-- | Alias for @removeFlag'@
removeFlag :: StateActions s => s -> Flag -> ToolMonad s
removeFlag = C.removeFlag'

-- | Alias for @invoke'@
invoke :: StateActions s => s -> ToolMonad String
invoke = C.invoke'

-- | Alias for @setPreProcess'@
setPreProcess :: StateActions s => s -> String -> String -> Set Flag -> ToolMonad s
setPreProcess = C.setPreProcess'

-- | Alias for @setArgument'@
setArgument :: StateActions s => s -> String -> ToolMonad s
setArgument = C.setArgument'
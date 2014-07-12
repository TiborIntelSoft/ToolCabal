module Tools.BasicTools.HsColour where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Tools.Core.ToolMonad
import Tools.BasicTools.Shared

--runHaddock :: ToolMonad String
runHaddock = do
  makeBasicConfiguration "HsColour"
  setSetting "version" 
  validateFlags
  invoke

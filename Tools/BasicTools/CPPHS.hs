module Tools.BasicTools.CPPHS where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Tools.Core.ToolMonad
import Tools.BasicTools.Shared

--runHaddock :: ToolMonad String
runCPPHS = do
  makeBasicConfiguration "cpphs"
  setSetting "version" 
  validateFlags
  invoke
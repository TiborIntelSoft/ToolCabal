module Tools.BasicTools.UUAGC where

import Data.Set (Set)
import qualified Data.Set as S
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Tools.Core.ToolMonad
import Tools.BasicTools.Shared
import Tools.Flag

--runHaddock :: ToolMonad String
runUUAGC = do
  makeBasicConfiguration "uuagc"
  --printCurrent
  setPreProcess "Assembly.ag" "Assembly.hs" $ S.fromList [newShortFlag "dH"]
  x <- invoke
  --printCurrent
  return x
 

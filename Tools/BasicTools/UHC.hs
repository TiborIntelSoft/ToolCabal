module Tools.BasicTools.UHC where

import Data.Set (Set)
import qualified Data.Set as S
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Tools.Core.ToolMonad
import Tools.BasicTools.Shared
import Tools.Flag

--runHaddock :: ToolMonad String
runUHC = do
  makeBasicConfiguration "uhc"
  addInputFile "HsTest.hs"
  setOutputFile "HsTest"
  setOutputDir "dist"
  --printCurrent
  x <- invoke
  --printCurrent
  return x


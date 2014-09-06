module Tools.BasicTools.GHC where

import Data.Set (Set)
import qualified Data.Set as S
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Tools.Core.ToolMonad
import Tools.Flag
import Tools.BasicTools.Shared

--runHaddock :: ToolMonad String
runGHC = do
  makeBasicConfiguration "ghc"
  addInputFile "HsTest.hs"  
  setOutputDir "dist"
  --printCurrent
  x <- invoke
  --printCurrent
  return x


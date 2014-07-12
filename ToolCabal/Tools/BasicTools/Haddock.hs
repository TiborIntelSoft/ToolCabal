module Tools.BasicTools.Haddock where

import Control.Lens
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import System.FilePath
import Tools.Core.ToolMonad
import Tools.BasicTools.Shared
import Tools.Flag

--runHaddock :: ToolMonad String
runHaddock = do
  makeBasicConfiguration "haddock"
  setSetting "version" 
  validateFlags
  invoke

haddockHsColourFlags :: String -> Set Flag
haddockHsColourFlags baseDir = S.fromList [
  newFlagWithArgument "source-module" $ baseDir </> "%{MODULE}.html",
  newFlagWithArgument "source-entity" $ baseDir </> "%{MODULE}.html#%{NAME}"
  ]

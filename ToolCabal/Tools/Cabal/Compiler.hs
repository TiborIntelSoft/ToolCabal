module Tools.Cabal.Compiler where

import Tools.Core.Types
import Tools.BasicTools.Shared

class PreProcessor c => Compiler c where
  
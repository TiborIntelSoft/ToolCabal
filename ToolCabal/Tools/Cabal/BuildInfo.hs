{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Tools.Cabal.BuildInfo where

import Control.Lens
import Tools.Core.Types
import Cabal

-- odir must be an empty directory or clean will delete too much
data BuildInfo = BuildInfo 
  {
    _outputDirectory :: FilePath,
    _compiler :: KnownPackageManager,
    _verbosity :: Verbosity,
    _targets :: [String],
    _flavours :: [String]
  }

deriving instance Show BuildInfo

makeLenses ''BuildInfo

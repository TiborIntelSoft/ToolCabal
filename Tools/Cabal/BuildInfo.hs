{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Tools.Cabal.BuildInfo where

import Control.Lens
import Tools.Core.Types
import Data.Map (Map)

import Tools.Flag

import Cabal

-- odir must be an empty directory or clean will delete too much
data BuildInfo = BuildInfo 
  {
    _outputDirectory :: FilePath,
    _compiler :: KnownCompiler,
    _compilerTargetTypes :: Map Target [(String, FileType)],
    _verbosity :: Verbosity,
    _targets :: [String],
    _flavours :: [String],
    _toolTargets :: [String],
    _toolFlavours :: [String],
    _exeNames :: Map Target [String],
    _libNames :: Map Target [String],
    _installLibDir :: Map Target String,
    _installIncludeDir :: String,
    _installExeDir ::  Map Target String,
    _installSources :: Map Target [String],
    _installTools :: Bool,
    _installSpecTool :: Map String Bool,
    _installExes :: Bool,
    _installSpecExe :: Map String Bool,
    _installLibrary :: Bool,
    _packageDB :: PackageDb,
    _packageDBs :: [PackageDb],
    _pluginPaths :: Map String FilePath,
    _progPaths :: Map String FilePath,
    _flagProg :: Map String [Flag],
    _argProg :: Map String [String],
    _dontShare :: Bool
  }

deriving instance Show BuildInfo

makeLenses ''BuildInfo
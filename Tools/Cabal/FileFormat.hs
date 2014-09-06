{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Tools.Cabal.FileFormat where

import Control.Lens

import Cabal
import Tools.Core.Types
import Data.Set (Set)
import qualified Data.Set as S

-- input file, processed to maybe list of files and the 
-- preprocess steps neccessary
type SourceFile = FilePath
type PreprocessFile = (String, [PreProcess])

--data ExeDependency = ExeDependency
--  {
--    _progName :: String,
--    _localProg :: Bool,
--    _progVersion :: VersionRange
--  }

--deriving instance Show ExeDependency

--data PkgDependency = PkgDependency
--  {
--    _pkgName :: String,
--    _pkgVersion :: VersionRange
--  }

--deriving instance Show PkgDependency

data SourceInfo = SourceInfo
  {
    --_usedTools :: [ExeDependency],
    _sourceFiles :: [SourceFile],
    _preprocessSources :: [PreprocessFile],
    _packages :: [String]
    --_dependencies :: [PkgDependency]
  }

deriving instance Show SourceInfo

data Library = Library 
  {
    _libName :: String,
    _libVersion :: Version,
    _exposedModules :: [String],
    _libSourceInfo :: SourceInfo
  }

deriving instance Show Library

data Executable = Executable
  {
    _exeName :: String,
    _mainModulePath :: String,
    _exeSourceInfo :: SourceInfo
  }

deriving instance Show Executable

--data ToolDescription = ToolDescription
--  {
--    _toolName :: String,
--    _mainToolModulePath :: String,
--    _toolSourceInfo :: SourceInfo
--  }

--deriving instance Show ToolDescription

data FileFormat = FileFormat
  {
    _pkgName :: String,
    _version :: Version,
    _library :: Maybe Library,
    _executables :: [Executable],
    _tools :: [Executable],
    _pkgSourceInfo :: SourceInfo
  }

deriving instance Show FileFormat

makeLenses ''FileFormat
--makeLenses ''ToolDescription
makeLenses ''Library
makeLenses ''Executable
makeLenses ''SourceInfo
--makeLenses ''ExeDependency
--makeLenses ''PkgDependency

emptySourceInfo = SourceInfo [] [] []
emptyFileFormat = FileFormat "EmptyPackage" (Version [] []) Nothing [] [] emptySourceInfo

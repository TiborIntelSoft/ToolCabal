module Tools.Core.Types 
(
  -- * Type synonyms
  Target,
  FileType,
  PreProcess,
  Setting,
  ToolMonad,

  -- * Known tool data types
  KnownTool (..),
  KnownOutputTool (..),
  KnownPreProcessor (..),
  KnownPackageManager (..),

  -- * lens accessors
  getTool, configuredProgram, flags, arguments,
  getOutputTool, configuredTool, inputDirs, outputDir, inputFiles, outputFile,
  getPreProcessor, configuredOutputTool, processFiles, baseDir,
  getPackageManager, configuredPreProcessor, packageDb,

  -- * Configured data types
  ConfiguredTool (..),
  ConfiguredOutputTool (..),
  ConfiguredPreProcessor (..),
  ConfiguredPackageManager (..),

  -- * Monad data types
  AnyTool (..),
  ErrorType (..),
  ToolState (..),

  -- * Classes
  Tool (..),
  OutputTool (..),
  PreProcessor (..),
  PackageManager (..),
  StateActions (..)
)
where

import Tools.Core.Core
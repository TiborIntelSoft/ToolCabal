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
  KnownCompiler (..),

  -- * lens accessors
  getTool, configuredProgram, flags, arguments,
  getOutputTool, configuredTool, inputDirs, outputDir, inputFiles, outputFile,
  getPreProcessor, configuredOutputTool, processFiles, baseDir,
  getCompiler, configuredPreProcessor, packageDb,

  -- * Configured data types
  ConfiguredTool (..),
  ConfiguredOutputTool (..),
  ConfiguredPreProcessor (..),
  ConfiguredCompiler (..),

  -- * Monad data types
  AnyTool (..),
  ErrorType (..),
  ToolState (..),

  -- * Other data types
  PackageDb (..),

  -- * Classes
  Tool (..),
  OutputTool (..),
  PreProcessor (..),
  Compiler (..)
)
where

import Tools.Core.Core
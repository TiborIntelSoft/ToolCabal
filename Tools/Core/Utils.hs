module Tools.Core.Utils 
(
  -- * Empty configurations
  emptyConfigureTool,
  emptyConfigureOutputTool,
  emptyConfigurePreProcessor,
  emptyConfigureCompiler,

  -- * Configurations
  configureTool,
  newConfigureOutputTool,
  configureOutputTool,
  configureFullOutputTool,
  newConfigurePreProcessor,
  configurePreProcessor,
  configureFullPreProcessor,
  newConfigureCompiler,
  configureCompiler,
  configureFullCompiler,

  -- * Utils
  verifyFlags,
  throwError,
  catchError,
  run,
  runError,

  -- * Default functions
  defaultInvoke,
  defaultInvoke',
  defaultConfigureForInvoke,
  defaultPreProcess
)
where

import Tools.Core.Core
import Control.Monad.Except

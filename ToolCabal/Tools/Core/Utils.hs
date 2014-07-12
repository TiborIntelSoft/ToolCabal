module Tools.Core.Utils 
(
  -- * Empty configurations
  emptyConfigureTool,
  emptyConfigureOutputTool,
  emptyConfigurePreProcessor,
  emptyConfigurePackageManager,

  -- * Configurations
  configureTool,
  newConfigureOutputTool,
  configureOutputTool,
  configureFullOutputTool,
  newConfigurePreProcessor,
  configurePreProcessor,
  configureFullPreProcessor,
  newConfigurePackageManager,
  configurePackageManager,
  configureFullPackageManager,

  -- * Utils
  verifyFlags,
  throwError,
  catchError,

  -- * Default functions
  defaultInvoke,
  defaultInvoke',
  defaultConfigureForInvoke,
  defaultPreProcess
)
where

import Tools.Core.Core
import Control.Monad.Except

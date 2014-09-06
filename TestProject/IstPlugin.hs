--module Main where

import Control.Lens
import System.Directory
import Control.Monad.IO.Class
import Data.Maybe (fromJust)

import Tools.Core.Plugin
import Tools.Flag
import Tools.DSet
--import Tools.Core.Types
import Tools.Core.Utils
import Tools.Core.Core

import Debug

main = mainPreProcessorPlugin IstTool

data IstTool = IstTool
  deriving (Show)

instance Tool IstTool where
  invokeTool _ ct = do 
    liftIO $ copyFile source target
    return $ "File " ++ source ++ " copied to " ++ target
    where source = head $ ct ^. arguments
          target = case getFlags "target" $ ct ^. flags of
            Just (x:_) -> fromJust $ x ^. defaultArgument

instance OutputTool IstTool where
  outputFileFlag _ = return . newFlagWithArgument "target"

instance PreProcessor IstTool where
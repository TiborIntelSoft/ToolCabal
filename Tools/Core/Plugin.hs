{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}

module Tools.Core.Plugin where

import Tools.Core.TypedMonad
import Tools.Core.Types
import Tools.Core.Utils

import Prelude hiding (init)
import qualified Prelude as P
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate, isPrefixOf)
import Development.Shake.FilePath (combine, (</>))
import Control.Lens hiding (Setting)
import Control.Monad.State
import Control.Monad.Except
import System.Environment (getArgs)

import Cabal hiding (info, notice, warn, debug, get)
import Tools.Flag
import Tools.DSet

import Debug

-- | All the data neccessary to use an external
-- program in the same way as an internal class
-- instance
data ToolPlugin = ToolPlugin {
  -- | The name of the plugin
  _pluginName :: String,
  -- | The program representing this plugin
  _pluginProgram :: ConfiguredProgram,
  -- | Wheter or not it is an @OutputTool@ plugin
  _isOutputTool :: Bool,
  -- | Wheter or not it is an @PreProcessor@ plugin
  _isPreProcessor :: Bool,
  -- | Wheter or not it is an @Compiler@ plugin
  _isPackagageManager :: Bool
  }
deriving instance Show ToolPlugin
deriving instance Read ToolPlugin
makeLenses ''ToolPlugin

instance Tool ToolPlugin where
  getName p = "Plugin: " ++ p ^. pluginName
  getProgram p = p ^. pluginProgram
  supportedFlags p _ = do
    fs <- callPlugin p "supportedFlags" []
    return $ read fs
  supportedArguments p _ = do
    fs <- callPlugin p "supportedArguments" []
    return $ read fs
  optionalArguments p _ = do
    fs <- callPlugin p "optionalArguments" []
    return $ read fs
  requiredArguments p _ = do
    fs <- callPlugin p "requiredArguments" []
    return $ read fs
  invokeTool p ct = callPlugin p "invokeTool" [showCT ct]
  --argToString p arg = callPlugin p "argToString" [arg]
  --flagToString p f = callPlugin p "flagToString" [show f]

instance OutputTool ToolPlugin where
  inputDirFlag p s = do
    if p ^. isOutputTool
      then do
        fs <- callPlugin p "inputDirFlag" [s]
        return $ read fs
      else inputDirFlag Dummy s
  outputDirFlag p s = do
    if p ^. isOutputTool
      then do
        fs <- callPlugin p "outputDirFlag" [s]
        return $ read fs
      else outputDirFlag Dummy s
  outputFileFlag p s = do
    if p ^. isOutputTool
      then do
        fs <- callPlugin p "outputFileFlag" [s]
        return $ read fs
      else outputFileFlag Dummy s
  configureForInvoke p ct = do
    if p ^. isOutputTool
      then do
        fs <- callPlugin p "configureForInvoke" [showCOT ct]
        return $ readPCT p fs
      else configureForInvoke Dummy ct

instance PreProcessor ToolPlugin where
  supportedTargets p cp = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "supportedTargets" []
        return $ read fs
      else supportedTargets Dummy cp
  targetTypes p cp ft = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "targetTypes" [ft]
        return $ read fs
      else targetTypes Dummy cp ft
  directDependencies p cp ft fp = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "directDependencies" [ft, fp]
        return $ read fs
      else directDependencies Dummy cp ft fp
  indirectDependencies p cp ft fp fps = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "indirectDependencies" [ft, fp, show fps]
        return $ read fs
      else indirectDependencies Dummy cp ft fp fps
  transFormStep p cp ft t fp fps = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "transFormStep" [showCP cp, ft, t, fp, show fps]
        return $ readPCT p fs
      else transFormStep Dummy cp ft t fp fps
  preProcess p cp = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "preProcess" [showCP cp]
        return $ readPCTs p fs
      else preProcess Dummy cp
  inputFileFlagArg p s = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "inputFileFlagArg" [s]
        return $ read fs
      else inputFileFlagArg Dummy s
  outputFileFlagArg p s = do
    if p ^. isPreProcessor
      then do
        fs <- callPlugin p "outputFileFlagArg" [s]
        return $ read fs
      else outputFileFlagArg Dummy s

instance Compiler ToolPlugin where
  --init p cpm s = do
  --  if p ^. isPackagageManager
  --    then do
  --      fs <- callPlugin p "init" [showCPM cpm, s]
  --      return $ readPCT p fs
  --    else init Dummy cpm s
  --register p cpm s = do
  --  if p ^. isPackagageManager
  --    then do
  --      fs <- callPlugin p "register" [showCPM cpm, s]
  --      return $ readPCT p fs
  --    else register Dummy cpm s
  --unregister p cpm s = do
  --  if p ^. isPackagageManager
  --    then do
  --      fs <- callPlugin p "unregister" [showCPM cpm, s]
  --      return $ readPCT p fs
  --    else unregister Dummy cpm s
  --expose p cpm s = do
  --  if p ^. isPackagageManager
  --    then do
  --      fs <- callPlugin p "expose" [showCPM cpm, s]
  --      return $ readPCT p fs
  --    else expose Dummy cpm s
  --hide p cpm s = do
  --  if p ^. isPackagageManager
  --    then do
  --      fs <- callPlugin p "hide" [showCPM cpm, s]
  --      return $ readPCT p fs
  --    else hide Dummy cpm s
  --list p cpm s = do
  --  if p ^. isPackagageManager
  --    then do
  --      fs <- callPlugin p "list" [showCPM cpm, s]
  --      return $ readPCT p fs
  --    else list Dummy cpm s

-- | Executes a call to the plugin.
-- It rethrows the error the plugin throws
callPlugin :: ToolPlugin -> String -> [String] -> ToolMonad String
callPlugin p f args = do 
  x <- defaultInvoke ct
  if isPrefixOf "Error: " x
    then throwError $ Exception x
    else return x 
  where ct = configureTool p (S.singleton $ newFlag f) $ map show args

readPCT :: ToolPlugin -> String -> ConfiguredTool
readPCT t = baseToCt t . read

readPCTs :: ToolPlugin -> String -> [ConfiguredTool]
readPCTs t = map (baseToCt t) . read

readPCOT :: ToolPlugin -> String -> ConfiguredOutputTool
readPCOT t = baseToCot t . read

readPCP :: ToolPlugin -> String -> ConfiguredPreProcessor
readPCP t = baseToCp t . read

readPCPM :: ToolPlugin -> String -> ConfiguredCompiler
readPCPM t = baseToCpm t . read

readCT :: Tool t => t -> String -> ConfiguredTool
readCT t = baseToCt t . read . read

readCTs :: Tool t => t -> String -> [ConfiguredTool]
readCTs t = map (baseToCt t) . read . read

readCOT :: OutputTool t => t -> String -> ConfiguredOutputTool
readCOT t = baseToCot t . read . read

readCP :: PreProcessor t => t -> String -> ConfiguredPreProcessor
readCP t = baseToCp t . read . read

readCPM :: Compiler t => t -> String -> ConfiguredCompiler
readCPM t = baseToCpm t . read . read

showCT :: ConfiguredTool -> String
showCT = show . ctToBase

showCTs :: [ConfiguredTool] -> String
showCTs = show . map ctToBase

showCOT :: ConfiguredOutputTool -> String
showCOT = show . cotToBase

showCP :: ConfiguredPreProcessor -> String
showCP = show . cpToBase

showCPM :: ConfiguredCompiler -> String
showCPM = show . cpmToBase

-- | The relevant data from the @ConfiguredTool@
-- data type
data CTBase = CTBase
  (Set Flag)
  [String]
  deriving (Show, Read)

-- | The relevant data from the @ConfiguredOutputTool@
-- data type
data COTBase = COTBase
  CTBase
  [FilePath]
  [FilePath]
  (Maybe FilePath)
  (Maybe FilePath)
  deriving (Show, Read)

-- | The relevant data from the @ConfiguredPreProcessor@
-- data type
data CPBase = CPBase
  COTBase
  [PreProcess]
  FilePath
  deriving (Show, Read)

-- | The relevant data from the @ConfiguredCompiler@
-- data type
data CPMBase = CPMBase
  CPBase
  [PackageDb]
  deriving (Show, Read)

ctToBase :: ConfiguredTool -> CTBase
ctToBase (ConfiguredTool _ _ f a) = CTBase f a

cotToBase :: ConfiguredOutputTool -> COTBase
cotToBase (ConfiguredOutputTool _ ct ids ifs od mof) = COTBase (ctToBase ct) ids ifs od mof

cpToBase :: ConfiguredPreProcessor -> CPBase
cpToBase (ConfiguredPreProcessor _ cot pf bd) = CPBase (cotToBase cot) pf bd

cpmToBase :: ConfiguredCompiler -> CPMBase
cpmToBase (ConfiguredCompiler _ ct ps) = CPMBase (cpToBase ct) ps

baseToCt :: Tool t => t -> CTBase -> ConfiguredTool
baseToCt t (CTBase fs a) = ConfiguredTool (KnownTool t) (getProgram t) fs a

baseToCot :: OutputTool t => t -> COTBase -> ConfiguredOutputTool
baseToCot t (COTBase ct ids ifs od mof) = 
    ConfiguredOutputTool (KnownOutputTool t) (baseToCt t ct) ids ifs od mof

baseToCp :: PreProcessor t => t -> CPBase -> ConfiguredPreProcessor
baseToCp t (CPBase ct pps p) = 
  ConfiguredPreProcessor (KnownPreProcessor t) (baseToCot t ct) pps p

baseToCpm :: Compiler t => t -> CPMBase -> ConfiguredCompiler
baseToCpm t (CPMBase ct ps) = 
  ConfiguredCompiler (KnownCompiler t) (baseToCp t ct) ps

-- * Dummy instance

data Dummy = Dummy
  deriving Show

instance Tool Dummy where
  getName _ = "Dummy instance"
  supportedFlags _ _ = throwError $ Exception "This operation is not supported"
  supportedArguments _ _ = throwError $ Exception "This operation is not supported"
  optionalArguments _ _ = throwError $ Exception "This operation is not supported"
  requiredArguments _ _ = throwError $ Exception "This operation is not supported"
  --invokeTool _ _ = throwError $ Exception "This operation is not supported"
  invokeTool _ _ = return "Warning: Invoked dummy does not do anything"
  argToString _ _ = throwError $ Exception "This operation is not supported"
  flagToString _ _ = throwError $ Exception "This operation is not supported"
instance OutputTool Dummy where
  inputDirFlag _ _ = throwError $ Exception "This operation is not supported"
  outputDirFlag _ _ = throwError $ Exception "This operation is not supported"
  outputFileFlag _ _ = throwError $ Exception "This operation is not supported"
  configureForInvoke _ _ = throwError $ Exception "This operation is not supported"
instance PreProcessor Dummy where
  supportedTargets _ _ = throwError $ Exception "This operation is not supported"
  targetTypes _ _ _= throwError $ Exception "This operation is not supported"
  directDependencies _ _ _ _ = throwError $ Exception "This operation is not supported"
  indirectDependencies _ _ _ _ _ = throwError $ Exception "This operation is not supported"
  transFormStep _ _ _ _ _ _ = throwError $ Exception "This operation is not supported"
  preProcess _ _ = throwError $ Exception "This operation is not supported"
  inputFileFlagArg _ _ = throwError $ Exception "This operation is not supported"
  outputFileFlagArg _ _ = throwError $ Exception "This operation is not supported"
instance Compiler Dummy where
  --init _ _ _ = throwError $ Exception "This operation is not supported"
  --register _ _ _ = throwError $ Exception "This operation is not supported"
  --unregister _ _ _ = throwError $ Exception "This operation is not supported"
  --expose _ _ _ = throwError $ Exception "This operation is not supported"
  --hide _ _ _ = throwError $ Exception "This operation is not supported"
  --list _ _ _ = throwError $ Exception "This operation is not supported"

-- * plugin creation

newPlugin :: String -> ConfiguredProgram -> ToolPlugin
newPlugin = newCompilerPlugin

newToolPlugin :: String -> ConfiguredProgram -> ToolPlugin
newToolPlugin s cp = newPlugin' False False False cp s

newOutputToolPlugin :: String -> ConfiguredProgram -> ToolPlugin
newOutputToolPlugin s cp = newPlugin' True False False cp s

newPreProcessorPlugin :: String -> ConfiguredProgram -> ToolPlugin
newPreProcessorPlugin s cp = newPlugin' True True False cp s

newCompilerPlugin :: String -> ConfiguredProgram -> ToolPlugin
newCompilerPlugin s cp = newPlugin' True True True cp s

newPlugin' :: Bool -> Bool -> Bool -> ConfiguredProgram -> String -> ToolPlugin
newPlugin' ot p pm cp n = ToolPlugin n cp ot p pm

-- * plugin runtime
mainPlugin :: (PreProcessor t, Compiler t) => t -> IO ()
mainPlugin = mainCompilerPlugin

mainToolPlugin :: Tool t => t -> IO ()
mainToolPlugin t = mainPlugin' t Dummy Dummy Dummy

mainOutputToolPlugin :: OutputTool t => t -> IO ()
mainOutputToolPlugin t = mainPlugin' t t Dummy Dummy

mainPreProcessorPlugin :: PreProcessor t => t -> IO ()
mainPreProcessorPlugin t = mainPlugin' t t t Dummy

mainCompilerPlugin :: Compiler t => t -> IO ()
mainCompilerPlugin t = mainPlugin' t t t t

mainPlugin' :: (Tool t1, OutputTool t2, PreProcessor t3, Compiler t4) => t1 -> t2 -> t3 -> t4 -> IO ()
mainPlugin' t ot p pm = do 
  args <- getArgs
  tm <- case args of
    (x:xs) -> return $ pluginResult t ot p pm x xs
    _ -> return $ throwError $ Exception "No function called"
  res <- run deafening tm
  putStr $ resToString res

resToString :: Either ErrorType String -> String
resToString (Left x) = "Error: " ++ show x
resToString (Right x) = x

pluginResult :: (Tool t1, OutputTool t2, PreProcessor t3, Compiler t4) => t1 -> t2 -> t3 -> t4 -> String -> [String] -> ToolMonad String
pluginResult t ot p pm f args = case f of
  "--getName" -> do
    r <- return $ getName t
    return r
  "--getProgram" -> do
    r <- return $ getProgram t
    return $ show r
  "--supportedFlags" -> do
    r <- supportedFlags t $ getProgram t
    return $ show r
  "--supportedArguments" -> do
    r <- supportedArguments t $ getProgram t
    return $ show r
  "--optionalArguments" -> do
    r <- optionalArguments t $ getProgram t
    return $ show r
  "--requiredArguments" -> do
    r <- requiredArguments t $ getProgram t
    return $ show r
  "--invokeTool" -> do
    ct <- return $ readCT t $ args !! 0
    r <- invokeTool t ct
    return r 
  "--argToString" -> do
    a <- return $ args !! 0
    r <- argToString t a
    return $ show r
  "--flagToString" -> do
    f <- return $ read $ args !! 0
    r <- flagToString t f
    return $ show r
  "--inputDirFlag" -> do
    a <- return $ args !! 0
    r <- inputDirFlag ot a
    return $ show r
  "--outputDirFlag" -> do
    a <- return $ args !! 0
    r <- outputDirFlag ot a
    return $ show r
  "--outputFileFlag" -> do
    a <- return $ args !! 0
    r <- outputFileFlag ot a
    return $ show r
  "--configureForInvoke" -> do
    ct <- return $ readCOT ot $ args !! 0
    r <- configureForInvoke ot ct
    return $ showCT r
  "--supportedTargets" -> do
    r <- supportedTargets p $ getProgram p
    return $ show r
  "--targetTypes" -> do
    ft <- return $ args !! 0
    r <- targetTypes p (getProgram p) ft
    return $ show r
  "--directDependencies" -> do
    ft <- return $ args !! 0
    fp <- return $ args !! 1
    r <- directDependencies p (getProgram p) ft fp
    return $ show r
  "--indirectDependencies" -> do
    ft <- return $ args !! 0
    fp <- return $ args !! 1
    fps <- return $ read $ args !! 2
    r <- indirectDependencies p (getProgram p) ft fp fps
    return $ show r
  "--transFormStep" -> do
    cp <- return $ readCP p $ args !! 0
    ft <- return $ args !! 1
    t <- return $ args !! 2
    fp <- return $ args !! 3
    fps <- return $ read $ args !! 3
    r <- transFormStep p cp ft t fp fps
    return $ showCT r
  "--preProcess" -> do
    cp <- return $ readCP p $ args !! 0
    r <- preProcess p cp
    return $ showCTs r
  "--inputFileFlagArg" -> do
    a <- return $ args !! 0
    r <- inputFileFlagArg p a
    return $ show r
  "--outputFileFlagArg" -> do
    a <- return $ args !! 0
    r <- outputFileFlagArg p a
    return $ show r
  --"--init" -> do
  --  cpm <- return $ readCPM pm $ args !! 0
  --  s <- return $ args !! 1
  --  r <- init pm cpm s
  --  return $ show r
  --"--register" -> do
  --  cpm <- return $ readCPM pm $ args !! 0
  --  s <- return $ args !! 1
  --  r <- register pm cpm s
  --  return $ show r
  --"--unregister" -> do
  --  cpm <- return $ readCPM pm $ args !! 0
  --  s <- return $ args !! 1
  --  r <- unregister pm cpm s
  --  return $ show r
  --"--expose" -> do
  --  cpm <- return $ readCPM pm $ args !! 0
  --  s <- return $ args !! 1
  --  r <- expose pm cpm s
  --  return $ show r
  --"--hide" -> do
  --  cpm <- return $ readCPM pm $ args !! 0
  --  s <- return $ args !! 1
  --  r <- hide pm cpm s
  --  return $ show r
  --"--list" -> do
  --  cpm <- return $ readCPM pm $ args !! 0
  --  s <- return $ args !! 1
  --  r <- list pm cpm s
  --  return $ show r
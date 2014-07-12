{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}

module Tools.Core.Core where

import Prelude hiding (init)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import System.FilePath (combine, (</>))
import Control.Lens hiding (Setting)
import Control.Monad.State
import Control.Monad.Except


import Cabal hiding (info, notice, warn, debug, get)
import Tools.Flag
import Tools.DSet

import Debug

---------------------------------------
-- * Data types
---------------------------------------

---------------------------------------
-- ** Type synonyms
---------------------------------------

type Target = String

type FileType = String

type PreProcess = (FilePath, FilePath, Set Flag)

type Setting = Either Flag String

type ToolMonad a = ExceptT ErrorType (StateT ToolState IO) a

---------------------------------------
-- ** Known tool Data types
---------------------------------------

data KnownTool =
  forall t. Tool t => KnownTool t
deriving instance Show KnownTool

data KnownOutputTool =
  forall t. OutputTool t => KnownOutputTool t
deriving instance Show KnownOutputTool

data KnownPreProcessor =
  forall t. PreProcessor t => KnownPreProcessor t
deriving instance Show KnownPreProcessor

data KnownPackageManager =
  forall t. PackageManager t => KnownPackageManager t
deriving instance Show KnownPackageManager

--data KnownCompiler =
--  forall t. Compiler t => KnownCompiler t
--deriving instance Show KnownCompiler

---------------------------------------
-- ** Configured Data types
---------------------------------------

data ConfiguredTool = ConfiguredTool
  {
    _getTool :: KnownTool,
    _configuredProgram :: ConfiguredProgram,
    _flags :: Set Flag,
    _arguments :: [String]
  }
  deriving Show

data ConfiguredOutputTool = ConfiguredOutputTool
  {
    _getOutputTool :: KnownOutputTool,
    _configuredTool :: ConfiguredTool,
    _inputDirs :: [FilePath],
    _outputDir :: Maybe FilePath,
    _inputFiles :: [FilePath],
    _outputFile :: Maybe FilePath
  }
  deriving Show

data ConfiguredPreProcessor = ConfiguredPreProcessor
  {
    _getPreProcessor :: KnownPreProcessor,
    -- | The link to the configured outputtool 
    _configuredOutputTool :: ConfiguredOutputTool,
    -- | The files to be preprocessed, sending input file to
    -- (possibly more than one) output file(s). 
    --
    -- Files should be processed in order and later files may
    -- refer to generated earlier files
    -- First argument is relative to head inputDir
    -- second to outputDir
    _processFiles :: [PreProcess],
    _baseDir :: FilePath
  }
  deriving Show

data ConfiguredPackageManager = ConfiguredPackageManager
  {
    _getPackageManager :: KnownPackageManager,
    _configuredPreProcessor :: ConfiguredPreProcessor,
    _packageDb :: [String]
  }
  deriving Show

--data ConfiguredCompiler = ConfiguredCompiler
--  {
--    _getCompiler :: KnownCompiler,
--    _configuredOutputToolComp :: ConfiguredOutputTool,
--    _configuredPackageManagerComp :: ConfiguredPackageManager,
--    _backends :: Set Setting,
--    _flavours :: Set Setting,
--    --_extensions :: Set String,
--    _executable :: Bool
--  }
--  deriving Show

---------------------------------------
-- ** Monad Data types
---------------------------------------

data AnyTool = 
  forall t. StateActions t => AnyTool t
deriving instance Show AnyTool

data ErrorType 
  = NotInvokableException String
  | NotConfigurableException String
  | Exception String
  deriving (Show)

-- fresh verbosity current, stored tools
data ToolState = ToolState{
  _fresh :: Int,
  _verbosity :: Verbosity, 
  _current :: Maybe AnyTool, -- can also store configurations
  _saveName :: String,
  _toolStore :: Map String AnyTool}
  deriving (Show)

---------------------------------------
-- * Classes
---------------------------------------

---------------------------------------
-- ** Tool Classes
---------------------------------------
-- | The Tool class. A tool is can be any extern program.
class Show t => Tool t where
  -- | The name of the tool. This should be unique among all instances.
  getName :: t -> String
  getName = show

  -- | The simple program for this tool. Needed to configure the program.
  getProgram :: t -> ConfiguredProgram
  getProgram = (\ x -> simpleConfiguredProgram x (FoundOnSystem x)) . getName

  -- | for most tools the supported flags are so different no generalization
  -- is possible.
  supportedFlags :: t -> ConfiguredProgram -> ToolMonad (DSet Flag)
  supportedFlags _ _ = return emptyDSet
  -- | arguments are non-named and don't have - or -- prepended
  supportedArguments :: t -> ConfiguredProgram -> ToolMonad [ProgArg]
  supportedArguments t cp = do 
    ra <- requiredArguments t cp 
    oa <- optionalArguments t cp
    return $ ra ++ oa
  optionalArguments :: t -> ConfiguredProgram -> ToolMonad [ProgArg]
  optionalArguments _ _ = return []
  requiredArguments :: t -> ConfiguredProgram -> ToolMonad [ProgArg]
  requiredArguments _ _ = return []

  -- | execute the Tool and returns any output
  invokeTool :: t ->  ConfiguredTool -> ToolMonad String
  invokeTool _ = defaultInvoke
  argToString :: t -> String -> ToolMonad [String]
  argToString _ a = return [a]
  flagToString :: t -> Flag -> ToolMonad [String]
  flagToString _ = return . flagToProgArg

-- | The OutputTool takes input directories or files and puts some
-- output in an outputDir
class Tool t => OutputTool t where
  -- | The flag used to specify an input or search directory
  inputDirFlag :: t -> String -> ToolMonad Flag
  inputDirFlag t _ = throwError $ Exception $ "Non-existing inputDirFlag for " ++ getName t
  -- | The flag used to specify the output directory
  outputDirFlag :: t -> String -> ToolMonad Flag
  outputDirFlag t _ = throwError $ Exception $ "Non-existing outputDirFlag for " ++ getName t
  -- | The flag used to specify the output file
  outputFileFlag :: t -> String -> ToolMonad Flag
  outputFileFlag t _ = throwError $ Exception $ "Non-existing outputFileFlag for " ++ getName t
  -- | The idea is that this takes the abstractions in the ConfiguredOutputTool and map them to arguments and
  -- flags that are injected into the already present flags and arguments of the ConfiguredTool
  configureForInvoke :: t -> ConfiguredOutputTool -> ToolMonad ConfiguredTool
  configureForInvoke _ = defaultConfigureForInvoke

-- | A class for preprocessors
class OutputTool p => PreProcessor p where
  supportedTargets :: p -> ConfiguredProgram -> ToolMonad [Target]
  supportedTargets _ _ = return []
  -- target, allowed source files
  sourceTypes :: p -> ConfiguredProgram -> Target -> ToolMonad [FileType]
  sourceTypes _ _ _ = return []
  -- | target, file, deps
  directDependencies :: p -> ConfiguredProgram -> Target -> FilePath -> ToolMonad [FilePath]
  directDependencies _ _ _ _ = return []
  -- | target, directDeps, deps
  indirectDependencies :: p -> ConfiguredProgram -> Target -> FilePath -> [FilePath] -> ToolMonad [FilePath]
  indirectDependencies _ _ _ _ _ = return []
  -- | target, allDeps per layer, invokable tool
  transFormStep :: p -> ConfiguredPreProcessor -> Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ConfiguredTool
  transFormStep _ _ _ _ _ _ = throwError $ Exception "Unsupported transFormStep"
  preProcess :: p -> ConfiguredPreProcessor -> ToolMonad [ConfiguredTool]
  preProcess _ = defaultPreProcess
  inputFileFlagArg :: p -> String -> ToolMonad Setting
  inputFileFlagArg _ = return . Right
  outputFileFlagArg :: p -> String -> ToolMonad Setting
  outputFileFlagArg p s = do 
    opf <- outputFileFlag p s
    return $ Left opf

class PreProcessor pm => PackageManager pm where
  init :: pm -> ConfiguredPackageManager -> String -> ToolMonad ConfiguredTool
  init _ _ _ = throwError $ Exception "This operation is not supported"
  register :: pm -> ConfiguredPackageManager -> String -> ToolMonad ConfiguredTool
  register _ _ _ = throwError $ Exception "This operation is not supported"
  unregister :: pm -> ConfiguredPackageManager -> String -> ToolMonad ConfiguredTool
  unregister _ _ _ = throwError $ Exception "This operation is not supported"
  expose :: pm -> ConfiguredPackageManager -> String -> ToolMonad ConfiguredTool
  expose _ _ _ = throwError $ Exception "This operation is not supported"
  hide :: pm -> ConfiguredPackageManager -> String -> ToolMonad ConfiguredTool
  hide _ _ _ = throwError $ Exception "This operation is not supported"
  list :: pm -> ConfiguredPackageManager -> String -> ToolMonad ConfiguredTool
  list _ _ _ = throwError $ Exception "This operation is not supported"
  packageFlag :: pm -> String -> ToolMonad Flag
  packageFlag _ _ = throwError $ Exception "Non existing package flag"
  hideAllPackagesFlag :: pm -> ToolMonad Flag
  hideAllPackagesFlag _ = throwError $ Exception "Non existing hide all packages flag"

--class (PackageManager c, OutputTool c) => Compiler c where
--  supportedBackends :: c -> Map String Setting
--  supportedFlavours :: c -> Map String Setting
--  --supportedExtensions :: c -> [String]
--  defaultBackend :: c -> Setting
--  defaultBackend c = Right ""
--  defaultFlavour :: c -> Setting
--  defaultFlavour c = Right ""
--  --defaultExtensions :: c -> Set String
--  build :: c -> ConfiguredCompiler -> [ConfiguredTool]
--  build _ = defaultBuild
--  --install :: c -> ConfiguredCompiler -> ConfiguredTool


---------------------------------------
-- ** Monad classes
---------------------------------------

class Show s => StateActions s where
  configure :: s -> ToolMonad AnyTool
  configure _ = throwError $ NotConfigurableException "Trying to configure a configured program"
  invokeState :: s ->  ToolMonad String
  invokeState _ = throwError $ NotInvokableException "Trying to invoke a not-configured program"
  toAnyTool :: s -> AnyTool
  toAnyTool = AnyTool
  getPossibleSettings :: s -> ToolMonad (DSet Flag)
  getPossibleSettings _ = throwError $ Exception "Program has no settings"
  setFlag :: s -> Flag -> ToolMonad s
  setFlag _ _ = throwError $ Exception "Program has no settings"
  resetFlag :: s -> Flag -> ToolMonad s
  resetFlag _ _ = throwError $ Exception "Program has no settings"
  checkFlags :: s -> ToolMonad Bool
  checkFlags _ = throwError $ Exception "Program has no settings"
  getCurrentFlags :: s -> ToolMonad (Set Flag)
  getCurrentFlags _ = throwError $ Exception "Program has no settings"
  setOutputDir :: s -> String -> ToolMonad s
  setOutputDir _ _ = throwError $ Exception "Program has no output directory"
  addInputDir :: s -> String -> ToolMonad s
  addInputDir _ _ = throwError $ Exception "Program has no input directory"
  setOutputFile :: s -> String -> ToolMonad s
  setOutputFile _ _ = throwError $ Exception "Program has no output files"
  addInputFile :: s -> String -> ToolMonad s
  addInputFile _ _ = throwError $ Exception "Program has no input files"
  addArgument :: s -> String -> ToolMonad s
  addArgument _ _ = throwError $ Exception "Program has no arguments"
  setBaseDir :: s -> String -> ToolMonad s
  setBaseDir _ _ = throwError $ Exception "Program has no arguments"
  addPreProcess :: s -> PreProcess -> ToolMonad s
  addPreProcess _ _ = throwError $ Exception "Program has no arguments"
  prepareTransForm :: s -> Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ConfiguredTool
  prepareTransForm _ _ _ _ _ = throwError $ Exception "Program is no preprocessor"

---------------------------------------
-- * KnownTool
---------------------------------------





--getTool :: String -> Maybe KnownTool
--getTool t
--  | t == getName Haddock = Just $ KnownTool Haddock
--  | t == getName HsColour = Just $ KnownTool HsColour
--  | t == getName CppHs = Just $ KnownTool CppHs
--  | t == getName UUAGC = Just $ KnownTool UUAGC
--  | t == getName GHC = Just $ KnownTool GHC
--  | t == getName UHC = Just $ KnownTool UHC
--getTool _ = Nothing

--getTool' :: String -> KnownTool
--getTool' s = case getTool s of
--  Just x -> x
--  Nothing -> error $ "Tool " ++ s ++ " not found"

--getToolName :: Tool t => t -> String
--getToolName = getName






--getOutputTool :: String -> KnownOutputTool
--getOutputTool t
--  | t == getName Haddock = KnownOutputTool Haddock
--  | t == getName HsColour = KnownOutputTool HsColour
--  | t == getName CppHs = KnownOutputTool CppHs
--  | t == getName UUAGC = KnownOutputTool UUAGC
--  | t == getName GHC = KnownOutputTool GHC
--  | t == getName UHC = KnownOutputTool UHC
--getOutputTool s = error $ "OutputTool " ++ s ++ " not found"

---------------------------------------
-- * Tool
---------------------------------------

verifyFlags :: Tool t => t -> ConfiguredTool -> ToolMonad Bool
verifyFlags t ct = do
  sfs <- supportedFlags t $ _configuredProgram ct
  return $ validFlags (_flags ct) sfs

-- | This is a debug version and this should not end up in final version
-- Verbosity should been added
defaultInvoke :: ConfiguredTool -> ToolMonad String
defaultInvoke = defaultInvoke' False

-- | has a boolean to signal if first the arguments or the flags should be printed
defaultInvoke' :: Bool -> ConfiguredTool -> ToolMonad String
defaultInvoke' a ct = do 
  arg <- sequence argM
  info $ (programId $ _configuredProgram ct) ++ " " ++ (intercalate " " $ concat arg)
  liftIO $ getProgramOutput deafening (_configuredProgram ct) $ concat arg
  where argM = if a then args ++ fs else fs ++ args
        args = map (argToString t) $ _arguments ct
        fs = map (flagToString t) $ S.toList $ _flags ct
        t = _getTool ct

-- | This holds all the data necessary to invoke the tool


-- | This is a debug version and this should not end up in final version
emptyConfigureTool :: Tool t => t -> ConfiguredTool
emptyConfigureTool t = ConfiguredTool (KnownTool t) cp S.empty []
  where cp = getProgram t

configureTool :: Tool t => t -> Set Flag -> [String] -> ConfiguredTool
configureTool t f a = (emptyConfigureTool t) {_flags = f, _arguments = a}

---------------------------------------
-- * OutputTool
---------------------------------------

defaultConfigureForInvoke :: ConfiguredOutputTool -> ToolMonad ConfiguredTool
defaultConfigureForInvoke (ConfiguredOutputTool t ct iDirs oDir iFiles oFile) = do 
  iFlags <- if null iDirs then return [] else catchError (mapM (inputDirFlag t) iDirs) (\_ -> return [])
  oFlag <- if isNothing oDir then return [] else catchError (sequence [outputDirFlag t $ fromJust oDir]) (\_ -> return [])
  oFFlag <- if isNothing oFile then return [] else catchError (sequence [outputFileFlag t $ fromJust oFile]) (\_ -> return [])
  return $ ct {_arguments = args, _flags = S.union (S.fromList $ iFlags ++ oFlag ++ oFFlag) $ _flags ct}
  where args = _arguments ct ++ iFiles

emptyConfigureOutputTool :: OutputTool t => t -> ConfiguredOutputTool
emptyConfigureOutputTool t = newConfigureOutputTool t ct
  where ct = emptyConfigureTool t

newConfigureOutputTool :: OutputTool t => t -> ConfiguredTool -> ConfiguredOutputTool
newConfigureOutputTool t ct = ConfiguredOutputTool (KnownOutputTool t) ct [] Nothing [] Nothing

configureOutputTool :: OutputTool t => t -> [FilePath] -> Maybe FilePath -> [FilePath] -> Maybe FilePath -> ConfiguredOutputTool
configureOutputTool t = configureFullOutputTool t ct
  where ct = emptyConfigureTool t

configureFullOutputTool :: OutputTool t => t -> ConfiguredTool -> [FilePath] -> Maybe FilePath -> [FilePath] -> Maybe FilePath -> ConfiguredOutputTool
configureFullOutputTool t ct iDirs oDir iFiles oFile = (newConfigureOutputTool t ct) {_inputDirs = iDirs, _outputDir = oDir, _inputFiles = iFiles, _outputFile = oFile}

---------------------------------------
-- * PreProcessor
---------------------------------------

defaultPreProcess :: ConfiguredPreProcessor -> ToolMonad [ConfiguredTool]
defaultPreProcess (ConfiguredPreProcessor p ct pf baseDir) = do
  t <- configureForInvoke (_getOutputTool ct) ct
  mapM (f t) pf
  where f t (iFile, oFile, flags) = do
          iFlag <- inputFileFlagArg p $ baseDir </> iFile
          oFlag <- outputFileFlagArg p $ baseDir </> oFile
          return $ t {_flags = _flags t `S.union` fs iFlag oFlag `S.union` flags, _arguments = _arguments t ++ args iFlag oFlag}
          where args iFlag oFlag = toArgs iFlag ++ toArgs oFlag
                fs iFlag oFlag = toFlags iFlag `S.union` toFlags oFlag
                toArgs (Right s) = [s]
                toArgs (Left _) = []
                toFlags (Right _) = S.empty
                toFlags (Left f) = S.singleton f

emptyConfigurePreProcessor :: PreProcessor t => t -> ConfiguredPreProcessor
emptyConfigurePreProcessor t = newConfigurePreProcessor t ct
  where ct = emptyConfigureOutputTool t

newConfigurePreProcessor :: PreProcessor t => t -> ConfiguredOutputTool -> ConfiguredPreProcessor
newConfigurePreProcessor t ct = ConfiguredPreProcessor (KnownPreProcessor t) ct [] ""

configurePreProcessor :: PreProcessor t => t -> [(FilePath, FilePath, Set Flag)] -> ConfiguredPreProcessor
configurePreProcessor t = configureFullPreProcessor t ct
  where ct = emptyConfigureOutputTool t

configureFullPreProcessor :: PreProcessor t => t -> ConfiguredOutputTool -> [(FilePath, FilePath, Set Flag)] -> ConfiguredPreProcessor
configureFullPreProcessor t ct f = (newConfigurePreProcessor t ct) {_processFiles = f}

---------------------------------------
-- * PackageManager
---------------------------------------

emptyConfigurePackageManager :: PackageManager t => t -> ConfiguredPackageManager
emptyConfigurePackageManager t = newConfigurePackageManager t ct
  where ct = emptyConfigurePreProcessor t

newConfigurePackageManager :: PackageManager t => t -> ConfiguredPreProcessor -> ConfiguredPackageManager
newConfigurePackageManager t ct = ConfiguredPackageManager (KnownPackageManager t) ct []

configurePackageManager :: PackageManager t => t -> [String] -> ConfiguredPackageManager
configurePackageManager t = configureFullPackageManager t ct
  where ct = emptyConfigurePreProcessor t

configureFullPackageManager :: PackageManager t => t -> ConfiguredPreProcessor -> [String] -> ConfiguredPackageManager
configureFullPackageManager t ct f = (newConfigurePackageManager t ct) {_packageDb = f}

-----------------------------------------
---- * Compiler
-----------------------------------------

--defaultBuild :: ConfiguredCompiler -> [ConfiguredTool]
--defaultBuild c = map (uncurry $ bf c) [(x,y) | x <- back, y <- flav]
--  where bf = if _executable c then buildExe else buildLib
--        back = if length b == 0 then [defaultBackend (_getCompiler c)] else b
--          where b = S.toList $ _backends c
--        flav = if length f == 0 then [defaultFlavour (_getCompiler c)] else f
--          where f = S.toList $ _flavours c

---- compiler target flavour
--buildExe :: ConfiguredCompiler -> Setting -> Setting -> ConfiguredTool
--buildExe c t f = ct
--  where ct = buildBase c t f

---- compiler target flavour
--buildLib :: ConfiguredCompiler -> Setting -> Setting -> ConfiguredTool
--buildLib c t f = ct
--  where ct = buildBase c t f

--buildBase :: ConfiguredCompiler -> Setting -> Setting -> ConfiguredTool
--buildBase c t f = ct
--  where ct1 = _configuredToolPM $ _configuredPackageManagerComp c
--        cot = _configuredOutputToolComp c
--        ct2 = configureForInvoke (_getOutputTool cot) cot
--        ct = mergeConfiguredTools ct1 ct2

--mergeConfiguredTools :: ConfiguredTool -> ConfiguredTool -> ConfiguredTool
--mergeConfiguredTools 
--  (ConfiguredTool {
--    _getTool = gt1,
--    _configuredProgram = cp1,
--    _flags = fs1,
--    _arguments = as1})
--  (ConfiguredTool {
--    _flags = fs2,
--    _arguments = as2}) 
--  = ConfiguredTool {
--    _getTool = gt1,
--    _configuredProgram = cp1,
--    _flags = fs1 `S.union` fs2,
--    _arguments = as1 ++ as2}
    

--emptyConfigureCompiler :: Compiler t => t -> ConfiguredCompiler
--emptyConfigureCompiler t = newConfigureCompiler t ct pm
--  where ct = emptyConfigureOutputTool t
--        pm = emptyConfigurePackageManager t

--newConfigureCompiler :: Compiler t => t -> ConfiguredOutputTool -> ConfiguredPackageManager -> ConfiguredCompiler
--newConfigureCompiler t ct pm = ConfiguredCompiler (KnownCompiler t) ct pm S.empty S.empty False

--configureCompiler :: Compiler t => t -> Set Setting -> Set Setting -> Bool -> ConfiguredCompiler
--configureCompiler t = configureFullCompiler t ct pm
--  where ct = emptyConfigureOutputTool t
--        pm = emptyConfigurePackageManager t

--configureFullCompiler :: Compiler t => t -> ConfiguredOutputTool -> ConfiguredPackageManager -> Set Setting -> Set Setting -> Bool -> ConfiguredCompiler
--configureFullCompiler t ct pm b f e = (newConfigureCompiler t ct pm) {_backends = b, _flavours = f, _executable = e}

---------------------------------------
-- * Messages
---------------------------------------

getVerbosity :: ToolMonad Verbosity
getVerbosity = do 
  x <- get
  return $ _verbosity x

warn :: String -> ToolMonad ()
warn msg = do
  v <- getVerbosity
  liftIO $ printMessage normal v ("Warning: " ++ msg)

debug :: String -> ToolMonad ()
debug msg = do
  v <- getVerbosity
  liftIO $ printMessage deafening v msg

notice :: String -> ToolMonad ()
notice msg = do
  v <- getVerbosity
  liftIO $ printMessage normal v msg

info :: String -> ToolMonad ()
info msg = do
  v <- getVerbosity
  liftIO $ printMessage verbose v msg

---------------------------------------
-- * Lenses
---------------------------------------

makeLenses ''ToolState
makeLenses ''ConfiguredTool
makeLenses ''ConfiguredOutputTool
makeLenses ''ConfiguredPreProcessor
makeLenses ''ConfiguredPackageManager

---------------------------------------
-- * ToolMonad
---------------------------------------

baseState :: Verbosity -> ToolState
baseState v = ToolState 0 v Nothing "Empty" M.empty

run :: Verbosity -> ToolMonad a -> IO (Either ErrorType a)
run v m = evalStateT (runExceptT m) (baseState v)

runError :: Verbosity -> ToolMonad a -> IO a
runError v t = do 
  r <- run v t
  case r of
    Left e -> error $ show e
    Right a -> return a

storeTool :: Tool t => String -> t -> ToolMonad ()
storeTool s = store s . KnownTool 

storeOutputTool :: OutputTool t => String -> t -> ToolMonad ()
storeOutputTool s = store s . KnownOutputTool 

storePreProcessor :: PreProcessor t => String -> t -> ToolMonad ()
storePreProcessor s = store s . KnownPreProcessor 

storePackageManager :: PackageManager t => String -> t -> ToolMonad ()
storePackageManager s = store s . KnownPackageManager

store :: StateActions t => String -> t -> ToolMonad ()
store s t = toolStore %= M.insert s (toAnyTool t)

storeAnyTool :: StateActions t => t -> ToolMonad String
storeAnyTool t = do
  x <- uses fresh show
  store x t
  fresh += 1
  return x

load :: StateActions t => t -> ToolMonad ()
load t = loadAnyTool $ toAnyTool t

-- Set given tool as current
loadAnyTool :: AnyTool -> ToolMonad ()
loadAnyTool t = current .= Just t

-- | lookup in the store
loadFromStore :: String -> ToolMonad ()
loadFromStore s = do
  x <- uses toolStore $ M.lookup s
  if (isJust x)
    then (saveName .= s)
    else (throwError $ Exception "AnyTool could not be found in the store")
  current .= x

-- | Use this one after loading from store
save :: ToolMonad ()
save = do
  s <- use saveName
  saveC s

saveC :: String -> ToolMonad ()
saveC s = do
  x <- getCurrent
  store s x

setSaveName :: String -> ToolMonad ()
setSaveName s = saveName .= s

getCurrent :: ToolMonad AnyTool
getCurrent = do
  x <- use current
  if (isNothing x)
    then throwError $ Exception "No current AnyTool present"
    else return $ fromJust x

makeConfiguration :: ToolMonad ()
makeConfiguration = do
  x <- getCurrent
  ct <- configure x
  current .= (Just ct)

findFlags :: String -> ToolMonad [Flag]
findFlags s = do
  x <- getCurrent
  flags <- getPossibleSettings x
  case getFlags s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

findFlag :: String -> ToolMonad Flag
findFlag s = do
  x <- getCurrent
  flags <- getPossibleSettings x
  case getEmptyFlag s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

findSetFlags :: String -> ToolMonad [Flag]
findSetFlags s = do
  x <- getCurrent
  flags <- getCurrentFlags x
  case getFlags s flags of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

findSetFlag :: String -> String -> ToolMonad Flag
findSetFlag s a = do
  x <- getCurrent
  flags <- getCurrentFlags x
  fs <- case getFlags s flags of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x
  case findFlagWithArg a fs of
    Nothing -> throwError $ Exception "Flag not found" 
    Just x -> return x 

-- tries to find the short or long flag, can crash when 
-- it is not able to
setSetting :: String -> ToolMonad ()
setSetting s = do
  x <- getCurrent
  f <- findFlag s
  ct <- setFlag x f
  current .= (Just $ toAnyTool ct)

resetSetting :: String  -> ToolMonad ()
resetSetting s = do
  x <- getCurrent
  f <- findSetFlags s
  ct <- foldM resetFlag x f
  current .= (Just $ toAnyTool ct)

addSetting :: String -> String -> ToolMonad ()
addSetting s a = do
  x <- getCurrent
  f <- findFlag s 
  ct <- setFlag x (f {_defaultArgument = Just a})
  current .= (Just $ toAnyTool ct)

replaceSetting :: String -> String -> ToolMonad ()
replaceSetting s a = do
  resetSetting s
  x <- getCurrent
  f <- findFlag s
  ct <- setFlag x (f {_defaultArgument = Just a})
  current .= (Just $ toAnyTool ct)

removeSetting :: String -> String -> ToolMonad ()
removeSetting s a = do
  x <- getCurrent
  f <- findSetFlag s a
  ct <- resetFlag x f
  current .= (Just $ toAnyTool ct)

validateFlags :: ToolMonad Bool
validateFlags = do
  x <- getCurrent
  checkFlags x

addFlag :: Flag -> ToolMonad ()
addFlag f = do
  x <- getCurrent
  ct <- setFlag x f
  current .= (Just $ toAnyTool ct)

replaceFlag :: Flag -> Flag -> ToolMonad ()
replaceFlag s f = do
  removeFlag s
  addFlag f

removeFlag :: Flag -> ToolMonad ()
removeFlag f = do
  x <- getCurrent
  ct <- resetFlag x f
  current .= (Just $ toAnyTool ct)

invoke :: ToolMonad String
invoke = do
  x <- getCurrent
  invokeState x

setPreProcess :: String -> String -> Set Flag -> ToolMonad ()
setPreProcess i o fs = do
  x <- getCurrent
  ct <- addPreProcess x (i, o, fs)
  current .= (Just $ toAnyTool ct)

setArgument :: String -> ToolMonad ()
setArgument f = do
  x <- getCurrent
  ct <- addArgument x f
  current .= (Just $ toAnyTool ct)

setInputFileC :: String -> ToolMonad ()
setInputFileC f = do
  x <- getCurrent
  ct <- addInputFile x f
  current .= (Just $ toAnyTool ct)

setOutputFileC :: String -> ToolMonad ()
setOutputFileC f = do
  x <- getCurrent
  ct <- setOutputFile x f
  current .= (Just $ toAnyTool ct)

setOutputDirC :: String -> ToolMonad ()
setOutputDirC f = do
  x <- getCurrent
  ct <- setOutputDir x f
  current .= (Just $ toAnyTool ct)

addInputDirC :: String -> ToolMonad ()
addInputDirC f = do
  x <- getCurrent
  ct <- addInputDir x f
  current .= (Just $ toAnyTool ct)

printCurrent :: ToolMonad ()
printCurrent = do
  x <- use current
  liftIO $ print x

prepareTransFormC :: Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ()
prepareTransFormC t tt fp deps = do
  x <- getCurrent
  ct <- prepareTransForm x t tt fp deps
  current .= (Just $ toAnyTool ct)

---------------------------------------
-- ** TypedMonad
---------------------------------------

loadFromStore' :: String -> ToolMonad AnyTool
loadFromStore' s = do
  x <- uses toolStore $ M.lookup s
  if (isJust x)
    then return $ fromJust x
    else throwError $ Exception "AnyTool could not be found in the store"

makeConfiguration' :: AnyTool -> ToolMonad AnyTool
makeConfiguration' = configure

findFlags' :: String -> AnyTool -> ToolMonad [Flag]
findFlags' s x = do
  flags <- getPossibleSettings x
  case getFlags s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

findFlag' :: String -> AnyTool -> ToolMonad Flag
findFlag' s x = do
  flags <- getPossibleSettings x
  case getEmptyFlag s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

findSetFlags' :: String -> AnyTool -> ToolMonad [Flag]
findSetFlags' s x = do
  flags <- getCurrentFlags x
  case getFlags s flags of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

findSetFlag' :: String -> String -> AnyTool -> ToolMonad Flag
findSetFlag' s a x = do
  flags <- getCurrentFlags x
  fs <- case getFlags s flags of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x
  case findFlagWithArg a fs of
    Nothing -> throwError $ Exception "Flag not found" 
    Just x -> return x 

-- tries to find the short or long flag, can crash when 
-- it is not able to
setSetting' :: String -> AnyTool -> ToolMonad AnyTool
setSetting' s x = do
  f <- findFlag' s x
  ct <- setFlag x f
  return $ toAnyTool ct

resetSetting' :: String -> AnyTool -> ToolMonad AnyTool
resetSetting' s x = do
  f <- findSetFlags' s x
  ct <- foldM resetFlag x f
  return $ toAnyTool ct

addSetting' :: String -> String -> AnyTool -> ToolMonad AnyTool
addSetting' s a x = do
  f <- findFlag' s x
  ct <- setFlag x (f {_defaultArgument = Just a})
  return $ toAnyTool ct

replaceSetting' :: String -> String -> AnyTool -> ToolMonad AnyTool
replaceSetting' s a ox = do
  x <- resetSetting' s ox
  f <- findFlag' s x
  ct <- setFlag x (f {_defaultArgument = Just a})
  return $ toAnyTool ct

removeSetting' :: String -> String -> AnyTool -> ToolMonad AnyTool
removeSetting' s a x = do
  f <- findSetFlag' s a x
  ct <- resetFlag x f
  return $ toAnyTool ct

validateFlags' :: AnyTool -> ToolMonad Bool
validateFlags' = checkFlags

addFlag' :: Flag -> AnyTool -> ToolMonad AnyTool
addFlag' f x = do
  ct <- setFlag x f
  return $ toAnyTool ct

replaceFlag' :: Flag -> Flag -> AnyTool -> ToolMonad AnyTool
replaceFlag' s f ox = do
  x <- removeFlag' s ox
  addFlag' f x

removeFlag' :: Flag -> AnyTool -> ToolMonad AnyTool
removeFlag' f x = do
  ct <- resetFlag x f
  return $ toAnyTool ct

invoke' :: AnyTool -> ToolMonad String
invoke' = invokeState

setPreProcess' :: String -> String -> Set Flag -> AnyTool -> ToolMonad AnyTool
setPreProcess' i o fs x = do
  ct <- addPreProcess x (i, o, fs)
  return $ toAnyTool ct

setArgument' :: String -> AnyTool -> ToolMonad AnyTool
setArgument' f x = do
  ct <- addArgument x f
  return $ toAnyTool ct

setInputFile' :: String -> AnyTool -> ToolMonad AnyTool
setInputFile' f x = do
  ct <- addInputFile x f
  return $ toAnyTool ct

setOutputFile' :: String -> AnyTool -> ToolMonad AnyTool
setOutputFile' f x = do
  ct <- setOutputFile x f
  return $ toAnyTool ct

setOutputDir' :: String -> AnyTool -> ToolMonad AnyTool
setOutputDir' f x = do
  ct <- setOutputDir x f
  return $ toAnyTool ct

addInputDir' :: String -> AnyTool -> ToolMonad AnyTool
addInputDir' f x = do
  ct <- addInputDir x f
  return $ toAnyTool ct

prepareTransForm' :: Target -> String -> FilePath -> [[FilePath]] -> AnyTool -> ToolMonad ConfiguredTool
prepareTransForm' t tt fp deps x = do
  prepareTransForm x t tt fp deps

---------------------------------------
-- * Instances
---------------------------------------

---------------------------------------
-- ** Tool Instances
---------------------------------------

instance Tool KnownTool where
  getName (KnownTool t) = getName t
  getProgram (KnownTool t) = getProgram t
  supportedFlags (KnownTool t) = supportedFlags t
  supportedArguments (KnownTool t) = supportedArguments t
  optionalArguments (KnownTool t) = optionalArguments t
  requiredArguments (KnownTool t) = requiredArguments t
  invokeTool (KnownTool t) = invokeTool t
  argToString (KnownTool t) = argToString t
  flagToString (KnownTool t) = flagToString t

instance Tool KnownOutputTool where
  getName (KnownOutputTool t) = getName t
  getProgram (KnownOutputTool t) = getProgram t
  supportedFlags (KnownOutputTool t) = supportedFlags t
  supportedArguments (KnownOutputTool t) = supportedArguments t
  optionalArguments (KnownOutputTool t) = optionalArguments t
  requiredArguments (KnownOutputTool t) = requiredArguments t
  invokeTool (KnownOutputTool t) = invokeTool t
  argToString (KnownOutputTool t) = argToString t
  flagToString (KnownOutputTool t) = flagToString t
  
instance Tool KnownPreProcessor where
  getName (KnownPreProcessor t) = getName t
  getProgram (KnownPreProcessor t) = getProgram t
  supportedFlags (KnownPreProcessor t) = supportedFlags t
  supportedArguments (KnownPreProcessor t) = supportedArguments t
  optionalArguments (KnownPreProcessor t) = optionalArguments t
  requiredArguments (KnownPreProcessor t) = requiredArguments t
  invokeTool (KnownPreProcessor t) = invokeTool t
  argToString (KnownPreProcessor t) = argToString t
  flagToString (KnownPreProcessor t) = flagToString t

instance Tool KnownPackageManager where
  getName (KnownPackageManager t) = getName t
  getProgram (KnownPackageManager t) = getProgram t
  supportedFlags (KnownPackageManager t) = supportedFlags t
  supportedArguments (KnownPackageManager t) = supportedArguments t
  optionalArguments (KnownPackageManager t) = optionalArguments t
  requiredArguments (KnownPackageManager t) = requiredArguments t
  invokeTool (KnownPackageManager t) = invokeTool t
  argToString (KnownPackageManager t) = argToString t
  flagToString (KnownPackageManager t) = flagToString t

--instance Tool KnownCompiler where
--  getName (KnownCompiler t) = getName t
--  getProgram (KnownCompiler t) = getProgram t
--  supportedFlags (KnownCompiler t) = supportedFlags t
--  supportedArguments (KnownCompiler t) = supportedArguments t
--  optionalArguments (KnownCompiler t) = optionalArguments t
--  requiredArguments (KnownCompiler t) = requiredArguments t
--  invokeTool (KnownCompiler t) = invokeTool t

---------------------------------------
-- ** OutputTool Instances
---------------------------------------

instance OutputTool KnownOutputTool where
  inputDirFlag (KnownOutputTool t) = inputDirFlag t
  outputDirFlag (KnownOutputTool t) = outputDirFlag t
  outputFileFlag (KnownOutputTool t) = outputFileFlag t
  configureForInvoke (KnownOutputTool t) = configureForInvoke t

instance OutputTool KnownPreProcessor where
  inputDirFlag (KnownPreProcessor t) = inputDirFlag t
  outputDirFlag (KnownPreProcessor t) = outputDirFlag t
  outputFileFlag (KnownPreProcessor t) = outputFileFlag t
  configureForInvoke (KnownPreProcessor t) = configureForInvoke t

instance OutputTool KnownPackageManager where
  inputDirFlag (KnownPackageManager t) = inputDirFlag t
  outputDirFlag (KnownPackageManager t) = outputDirFlag t
  outputFileFlag (KnownPackageManager t) = outputFileFlag t
  configureForInvoke (KnownPackageManager t) = configureForInvoke t

---------------------------------------
-- ** PreProcessor Instances
---------------------------------------

instance PreProcessor KnownPreProcessor where
  supportedTargets (KnownPreProcessor p) = supportedTargets p
  sourceTypes (KnownPreProcessor p) = sourceTypes p
  directDependencies (KnownPreProcessor p) = directDependencies p
  indirectDependencies (KnownPreProcessor p) = indirectDependencies p
  transFormStep (KnownPreProcessor p) = transFormStep p
  preProcess (KnownPreProcessor p) = preProcess p
  inputFileFlagArg (KnownPreProcessor p) = inputFileFlagArg p
  outputFileFlagArg (KnownPreProcessor p) = outputFileFlagArg p

instance PreProcessor KnownPackageManager where
  supportedTargets (KnownPackageManager p) = supportedTargets p
  sourceTypes (KnownPackageManager p) = sourceTypes p
  directDependencies (KnownPackageManager p) = directDependencies p
  indirectDependencies (KnownPackageManager p) = indirectDependencies p
  transFormStep (KnownPackageManager p) = transFormStep p
  preProcess (KnownPackageManager p) = preProcess p
  inputFileFlagArg (KnownPackageManager p) = inputFileFlagArg p
  outputFileFlagArg (KnownPackageManager p) = outputFileFlagArg p

---------------------------------------
-- ** PackageManager Instances
---------------------------------------

instance PackageManager KnownPackageManager where
  init (KnownPackageManager pm) = init pm
  register (KnownPackageManager pm) = register pm
  unregister (KnownPackageManager pm) = unregister pm
  expose (KnownPackageManager pm) = expose pm
  hide (KnownPackageManager pm) = hide pm
  list (KnownPackageManager pm) = list pm
  packageFlag (KnownPackageManager pm) = packageFlag pm
  hideAllPackagesFlag (KnownPackageManager pm) = hideAllPackagesFlag pm


--instance PackageManager KnownCompiler where
--  init (KnownCompiler pm) = init pm
--  register (KnownCompiler pm) = register pm
--  unregister (KnownCompiler pm) = unregister pm
--  expose (KnownCompiler pm) = expose pm
--  hide (KnownCompiler pm) = hide pm
--  list (KnownCompiler pm) = list pm

---------------------------------------
-- ** Compiler Instances
---------------------------------------

--instance Compiler KnownCompiler where
--  supportedBackends (KnownCompiler c) = supportedBackends c
--  supportedFlavours (KnownCompiler c) = supportedFlavours c
--  --supportedExtensions (KnownCompiler c) = supportedExtensions c
--  defaultBackend (KnownCompiler c) = defaultBackend c
--  defaultFlavour (KnownCompiler c) = defaultFlavour c
--  --defaultExtensions (KnownCompiler c) = defaultExtensions c
--  build (KnownCompiler c) = build c

---------------------------------------
-- ** Monad Instances
---------------------------------------

instance StateActions AnyTool where
  configure (AnyTool t) = configure t
  invokeState (AnyTool t) = invokeState t
  toAnyTool t = t
  getPossibleSettings (AnyTool t) = getPossibleSettings t
  setFlag (AnyTool t) f = do
    x <- setFlag t f
    return $ toAnyTool x
  resetFlag (AnyTool t) f = do
    x <- resetFlag t f
    return $ toAnyTool x
  checkFlags (AnyTool t) = checkFlags t
  getCurrentFlags (AnyTool t) = getCurrentFlags t
  setOutputDir (AnyTool t) s = do
    x <- setOutputDir t s
    return $ toAnyTool x
  addInputDir (AnyTool t) s = do
    x <- addInputDir t s
    return $ toAnyTool x
  setOutputFile (AnyTool t) s = do
    x <- setOutputFile t s
    return $ toAnyTool x
  addInputFile (AnyTool t) s = do
    x <- addInputFile t s
    return $ toAnyTool x
  addArgument (AnyTool t) s = do
    x <- addArgument t s
    return $ toAnyTool x
  setBaseDir (AnyTool t) s = do
    x <- setBaseDir t s
    return $ toAnyTool x
  addPreProcess (AnyTool t) p = do
    x <- addPreProcess t p
    return $ toAnyTool x
  prepareTransForm (AnyTool t) = prepareTransForm t
  --setFlag (AnyTool t) = setFlag t
  --setFlag (AnyTool t) = setFlag t
  --setFlag (AnyTool t) = setFlag t

instance StateActions KnownTool where
  configure (KnownTool t) = return $ toAnyTool $ emptyConfigureTool t

instance StateActions KnownOutputTool where
  configure (KnownOutputTool t) = return $ toAnyTool $ emptyConfigureOutputTool t

instance StateActions KnownPreProcessor where
  configure (KnownPreProcessor t) = return $ toAnyTool $ emptyConfigurePreProcessor t

instance StateActions KnownPackageManager where
  configure (KnownPackageManager t) = return $ toAnyTool $ emptyConfigurePackageManager t

--instance StateActions KnownCompiler where
--  configure (KnownCompiler t) = return $ toAnyTool $ emptyConfigureCompiler t


instance StateActions ConfiguredTool where
  invokeState ct = invokeTool (_getTool ct) ct
  getPossibleSettings ct = supportedFlags (_getTool ct) (_configuredProgram ct)
  setFlag ct f = return $ ct & flags %~ (S.insert f)
  resetFlag ct f = return $ ct & flags %~ (S.delete f)
  checkFlags ct = do
    ds <- getPossibleSettings ct
    return $ validFlags (_flags ct) ds
  getCurrentFlags ct = return $ ct ^. flags
  addArgument ct s = return $ ct & arguments %~ (s:)

instance StateActions ConfiguredOutputTool where
  invokeState ct = do 
    ct1 <- configureForInvoke (_getOutputTool ct) ct
    invokeTool t ct1
    where t = _getTool $ _configuredTool ct
  getPossibleSettings = getPossibleSettings . _configuredTool
  setFlag ct f = return $ ct & configuredTool %~ (\x -> x & flags %~ S.insert f)
  resetFlag ct f = return $ ct & configuredTool %~ (\x -> x & flags %~ S.delete f)
  checkFlags ct = do
    ds <- getPossibleSettings ct
    return $ validFlags (ct ^. configuredTool ^. flags) ds
  getCurrentFlags ct = return $ ct ^. configuredTool ^. flags
  addArgument ct s = return $ ct & configuredTool %~ (\x -> x & arguments %~ (s:))
  setOutputDir ct s = return $ ct & outputDir .~ Just s
  addInputDir ct s = return $ ct & inputDirs %~ (s:)
  setOutputFile ct s = return $ ct & outputFile .~ Just s
  addInputFile ct s = return $ ct & inputFiles %~ (s:)

instance StateActions ConfiguredPreProcessor where
  invokeState ct = do 
    cts <- preProcess p ct
    if (length cts == 0)
      then do 
        invokeState $ ct ^. configuredOutputTool
      else do
        ret <- mapM invokeState cts
        return $ intercalate "\n" ret
    where p = ct ^. getPreProcessor
  getPossibleSettings = getPossibleSettings . _configuredOutputTool
  setFlag ct f = do
    nct <- setFlag (_configuredOutputTool ct) f
    return $ ct & configuredOutputTool .~ nct
  resetFlag ct f = do
    nct <- resetFlag (_configuredOutputTool ct) f
    return $ ct & configuredOutputTool .~ nct
  getCurrentFlags = getCurrentFlags . _configuredOutputTool
  addArgument ct s = do
    nct <- addArgument (_configuredOutputTool ct) s
    return $ ct & configuredOutputTool .~ nct
  setOutputDir ct s = do
    nct <- setOutputDir (_configuredOutputTool ct) s
    return $ ct & configuredOutputTool .~ nct
  addInputDir ct s = do
    nct <- addInputDir (_configuredOutputTool ct) s
    return $ ct & configuredOutputTool .~ nct
  setOutputFile ct s = do
    nct <- setOutputFile (_configuredOutputTool ct) s
    return $ ct & configuredOutputTool .~ nct
  addInputFile ct s = do
    nct <- addInputFile (_configuredOutputTool ct) s
    return $ ct & configuredOutputTool .~ nct
  setBaseDir ct s = return $ ct & baseDir .~ s
  addPreProcess ct pp = return $ ct & processFiles %~ (pp:)
  prepareTransForm ct = transFormStep p ct
    where p = ct ^. getPreProcessor
instance StateActions ConfiguredPackageManager where
  invokeState = invokeState . _configuredPreProcessor
  getPossibleSettings = getPossibleSettings . _configuredPreProcessor
  setFlag ct f = do
    nct <- setFlag (_configuredPreProcessor ct) f
    return $ ct & configuredPreProcessor .~ nct
  resetFlag ct f = do
    nct <- resetFlag (_configuredPreProcessor ct) f
    return $ ct & configuredPreProcessor .~ nct
  getCurrentFlags = getCurrentFlags . _configuredPreProcessor
  addArgument ct s = do
    nct <- addArgument (_configuredPreProcessor ct) s
    return $ ct & configuredPreProcessor .~ nct
  setOutputDir ct s = do
    nct <- setOutputDir (_configuredPreProcessor ct) s
    return $ ct & configuredPreProcessor .~ nct
  addInputDir ct s = do
    nct <- addInputDir (_configuredPreProcessor ct) s
    return $ ct & configuredPreProcessor .~ nct
  setOutputFile ct s = do
    nct <- setOutputFile (_configuredPreProcessor ct) s
    return $ ct & configuredPreProcessor .~ nct
  addInputFile ct s = do
    nct <- addInputFile (_configuredPreProcessor ct) s
    return $ ct & configuredPreProcessor .~ nct
  setBaseDir ct s = do
    nct <- setBaseDir (_configuredPreProcessor ct) s
    return $ ct & configuredPreProcessor .~ nct
  addPreProcess ct s = do
    nct <- addPreProcess (_configuredPreProcessor ct) s
    return $ ct & configuredPreProcessor .~ nct
  prepareTransForm = prepareTransForm . _configuredPreProcessor

--instance StateActions ConfiguredCompiler where
--  invokeState ct = do 
--    cts <- return $ build p ct
--    ret <- mapM invokeState cts
--    return $ intercalate "\n" ret
--    where p = ct ^. getCompiler
--  getPossibleSettings = getPossibleSettings . _configuredOutputToolComp
--  setFlag ct f = do
--    nct <- setFlag (_configuredOutputToolComp ct) f
--    return $ ct & configuredOutputToolComp .~ nct
--  resetFlag ct f = do
--    nct <- resetFlag (_configuredOutputToolComp ct) f
--    nct2 <- resetFlag (_configuredPackageManagerComp ct) f
--    return $ ct &~ do
--      configuredOutputToolComp .= nct
--      configuredPackageManagerComp .= nct2
--  getCurrentFlags = getCurrentFlags . _configuredOutputToolComp
--  addArgument ct s = do
--    nct <- addArgument (_configuredOutputToolComp ct) s
--    return $ ct & configuredOutputToolComp .~ nct
--  setOutputDir ct s = do
--    nct <- setOutputDir (_configuredOutputToolComp ct) s
--    return $ ct & configuredOutputToolComp .~ nct
--  addInputDir ct s = do
--    nct <- addInputDir (_configuredOutputToolComp ct) s
--    return $ ct & configuredOutputToolComp .~ nct
--  setOutputFile ct s = do
--    nct <- setOutputFile (_configuredOutputToolComp ct) s
--    return $ ct & configuredOutputToolComp .~ nct
--  addInputFile ct s = do
--    nct <- addInputFile (_configuredOutputToolComp ct) s
--    return $ ct & configuredOutputToolComp .~ nct

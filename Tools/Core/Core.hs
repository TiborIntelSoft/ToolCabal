{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}

module Tools.Core.Core where

import Prelude hiding (init)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import Development.Shake.FilePath (combine, (</>))
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

-- | A backend target for a preprocessor. 
-- The semantics and format of the string
-- depends on the preprocessor.
type Target = String

-- | A type synonym for a file type.
-- It is used to clarify type declarations
type FileType = String

-- | The type for a manual preprocess action.
-- It consists of the source file, the target file
-- and the flags for the preprocessor
type PreProcess = (FilePath, FilePath, Set Flag)

-- | A type synonym for a setting to a program.
-- It is either a flag or an argument (String)
type Setting = Either Flag String

-- | The type synonym for the exception, state and io monad
-- used throughout the system
type ToolMonad a = ExceptT ErrorType (StateT ToolState IO) a

---------------------------------------
-- ** Known tool Data types
---------------------------------------

-- | An existential type for the @Tool@ class.
data KnownTool =
  forall t. Tool t => KnownTool t
deriving instance Show KnownTool

-- | An existential type for the @OutputTool@ class.
data KnownOutputTool =
  forall t. OutputTool t => KnownOutputTool t
deriving instance Show KnownOutputTool

-- | An existential type for the @PreProcessor@ class.
data KnownPreProcessor =
  forall t. PreProcessor t => KnownPreProcessor t
deriving instance Show KnownPreProcessor

-- | An existential type for the @Compiler@ class.
data KnownCompiler =
  forall t. Compiler t => KnownCompiler t
deriving instance Show KnownCompiler

--data KnownCompiler =
--  forall t. Compiler t => KnownCompiler t
--deriving instance Show KnownCompiler

---------------------------------------
-- ** Configured Data types
---------------------------------------

-- | This holds all the data necessary to
-- invoke a tool.
data ConfiguredTool = ConfiguredTool
  {
    -- | Reference to the @Tool@ that belongs to
    -- this ConfigueredTool
    _getTool :: KnownTool,
    -- | The program that will be run during
    -- invocation (only defaultInvoke).
    -- It is supplied here for convenience and
    -- to allow the user to overwrite it, although
    -- the latter should never happen.
    _configuredProgram :: ConfiguredProgram,
    -- | The flags that should be passed on to 
    -- the program during invocation
    _flags :: Set Flag,
    -- | The arguments that should be passed on to
    -- the program during invocation
    _arguments :: [String]
  }
  deriving Show

-- | This holds all the data to invoke an @OutputTool@
data ConfiguredOutputTool = ConfiguredOutputTool
  {
    -- | Reference to the @OutputTool@ that belongs to
    -- this ConfiguredOutputTool
    _getOutputTool :: KnownOutputTool,
    -- | As an @OutputTool@ is also a @Tool@,
    -- all the configured data for a @Tool@ is
    -- also present here
    _configuredTool :: ConfiguredTool,
    -- | A list of directories where source files
    -- (additional files) are located
    _inputDirs :: [FilePath],
    -- | A list of files that are supplied to the
    -- @OutputTool@
    _inputFiles :: [FilePath],
    -- | A directory specifying where output files
    -- should be deposited
    _outputDir :: Maybe FilePath,
    -- | A @FilePath@ specifying the name and location
    -- of the file produced by the @OutputTool@
    _outputFile :: Maybe FilePath
  }
  deriving Show

-- | This holds all the data to invoke a @PreProcessor@
data ConfiguredPreProcessor = ConfiguredPreProcessor
  {
  -- | reference to the @PreProcessor@ that belongs to
  -- this ConfiguredPreProcessor
    _getPreProcessor :: KnownPreProcessor,
    -- | As a @PreProcessor@ is also an @OutputTool@
    -- all configured data is also present here
    _configuredOutputTool :: ConfiguredOutputTool,
    -- | A list of files to be preprocessed.
    -- They are processed in order, so later
    -- files can depend on earlier generated files.
    _processFiles :: [PreProcess],
    -- The directory prefix to be applied
    -- to all @FilePath@s in the files to be preprocessed
    _baseDir :: FilePath
  }
  deriving Show

-- TODO
data ConfiguredCompiler = ConfiguredCompiler
  {
    _getCompiler :: KnownCompiler,
    _configuredPreProcessor :: ConfiguredPreProcessor,
    _packageDb :: [PackageDb]
  }
  deriving Show

--data ConfiguredCompiler = ConfiguredCompiler
--  {
--    _getCompiler :: KnownCompiler,
--    _configuredOutputToolComp :: ConfiguredOutputTool,
--    _configuredCompilerComp :: ConfiguredCompiler,
--    _backends :: Set Setting,
--    _flavours :: Set Setting,
--    --_extensions :: Set String,
--    _executable :: Bool
--  }
--  deriving Show

---------------------------------------
-- ** Monad Data types
---------------------------------------

-- | An existential type for the @StateActions@
-- class so it can be stored inside the state of
-- the @ToolMonad@
data AnyTool = 
  forall t. StateActions t => AnyTool t
deriving instance Show AnyTool

-- | The exception type used by the @ExceptT@
-- monad transformer of the @ToolMonad@
data ErrorType 
  -- | This tells the system invocation
  -- of something that cannot be invoked
  -- happened
  = NotInvokableException String
  -- | This tells the system @configure@
  -- is called on something that cannot be
  -- configured. This happens when trying to
  -- configure a configuration
  | NotConfigurableException String
  -- | Any other type of exception
  | Exception String
  deriving (Show)

-- | The state of the @StateT@ part of the 
-- @ToolMonad@
data ToolState = ToolState {
  -- | A @Int@ variable which intended use is
  -- to be an unique number throughout computation
  _fresh :: Int,
  -- | The @Verbosity@ to use when executing actions
  -- inside the @ToolMonad@
  _verbosity :: Verbosity, 
  -- | Stores a @AnyTool@ for simplified use
  -- of the @ToolMonad@
  _current :: Maybe AnyTool,
  -- | The name of the current @AnyTool@ stored.
  -- This is used when writing the current to the store
  _saveName :: String,
  -- | A store containing @AnyTool@s. This is used
  -- when working with multiple @AnyTool@s.
  _toolStore :: Map String AnyTool}
  deriving (Show)

---------------------------------------
-- ** Other Data types
---------------------------------------

-- | A package database used by @Compiler@s
data PackageDb
  -- | The user package database
  = UserPackageDb
  -- | The global package database
  | GlobalPackageDb
  -- | Any other package database with the 
  -- location
  | PackageDb FilePath
  deriving (Show, Read)

---------------------------------------
-- * Classes
---------------------------------------

---------------------------------------
-- ** Tool Classes
---------------------------------------

-- | The Tool class. It encapsulates a program.
-- Although its intended use is for extern programs,
-- this does not have to be the case
class Show t => Tool t where
  -- | The name of the tool. This should be unique among all instances.
  getName :: t -> String
  getName = show

  -- | The simple program for this tool. Needed to configure the program.
  getProgram :: t -> ConfiguredProgram
  getProgram = (\ x -> simpleConfiguredProgram x (FoundOnSystem x)) . getName

  -- | The supported flags for this @Tool@ and the dependencies
  -- between those flags. This is used to validate the supplied flags.
  -- For most tools the supported flags are so different no generalization
  -- is possible.
  supportedFlags :: t -> ConfiguredProgram -> ToolMonad (DSet Flag)
  supportedFlags _ _ = return emptyDSet
  -- | Arguments are non-named and don't have - or -- prepended.
  -- The list is used for displaying help about a @Tool@
  supportedArguments :: t -> ConfiguredProgram -> ToolMonad [ProgArg]
  supportedArguments t cp = do 
    ra <- requiredArguments t cp 
    oa <- optionalArguments t cp
    return $ ra ++ oa
  -- | Arguments are non-named and don't have - or -- prepended.
  -- The list is used for displaying help about a @Tool@
  optionalArguments :: t -> ConfiguredProgram -> ToolMonad [ProgArg]
  optionalArguments _ _ = return []
  -- | Arguments are non-named and don't have - or -- prepended.
  -- The list is used for displaying help about a @Tool@
  -- and for validation (the number of supplied arguments
  -- should at least be the number of required arguments)
  requiredArguments :: t -> ConfiguredProgram -> ToolMonad [ProgArg]
  requiredArguments _ _ = return []
  -- | Executes the @Tool@ and returns any output
  invokeTool :: t ->  ConfiguredTool -> ToolMonad String
  invokeTool _ = defaultInvoke
  -- | A function to transform an argument to the string
  -- that is supplied to the command line
  argToString :: t -> String -> ToolMonad [String]
  argToString _ a = return [a]
  -- | A function to transform a flag to the string
  -- that is supplied to the command line
  flagToString :: t -> Flag -> ToolMonad [String]
  flagToString _ = return . flagToProgArg

-- | The OutputTool takes input directories or files and puts some
-- output in an output directory or writes to an output file
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
  -- | Prepares a @ConfiguredOutputTool@ and converts it into a @ConfiguredTool@ ready to be invoked.
  -- The idea is that this takes the abstractions in the ConfiguredOutputTool and map them to arguments and
  -- flags that are injected into the already present flags and arguments of the ConfiguredTool
  configureForInvoke :: t -> ConfiguredOutputTool -> ToolMonad ConfiguredTool
  configureForInvoke _ = defaultConfigureForInvoke

-- | This class encapsulates a preprocessor.
-- It supports manual batch preprocessing (via @preProcess@)
-- and automatic preprocessing. The latter is used
-- for compilers
class OutputTool p => PreProcessor p where
  -- | This returns which targets can be build
  -- with this @PreProcessor@. For compilers the
  -- format is ¨flavour target¨ followed by either
  -- ¨library¨ for a library target, ¨executable¨ 
  -- for an executable target or nothing when it is
  -- for either a library or an executable
  supportedTargets :: p -> ConfiguredProgram -> ToolMonad [Target]
  supportedTargets _ _ = return []
  -- | This returns for a given target the file types
  -- that can be produced by this target
  targetTypes :: p -> ConfiguredProgram -> Target -> ToolMonad [(String, FileType)]
  targetTypes _ _ _ = return []
  -- | This returns the source files needed to build the 
  -- target file
  directDependencies :: p -> ConfiguredProgram -> Target -> FilePath -> ToolMonad [FilePath]
  directDependencies _ _ _ _ = return []
  -- | This returns the dependencies of the source files
  -- gotten from @directDependencies@
  indirectDependencies :: p -> ConfiguredProgram -> Target -> FilePath -> [FilePath] -> ToolMonad [FilePath]
  indirectDependencies _ _ _ _ _ = return []
  -- | This performs automatic preprocessing. It is supplied
  -- the target for which to build, the file type to build,
  -- the file name to build (this includes directory) and the dependencies.
  -- The dependencies are layered. Normally this consists of two lists:
  -- the @directDependencies@ and the @indirectDependencies@.
  transFormStep :: p -> ConfiguredPreProcessor -> Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ConfiguredTool
  transFormStep _ _ _ _ _ _ = throwError $ Exception "Unsupported transFormStep"
  -- | This performs manual preprocessing. Inside the @ConfiguredPreProcessor@
  -- is a list of files to preprocess and the flags to use.
  -- Each file is converted into a separate @ConfiguredTool@. If
  -- the preprocessor supports batch processing in a single call,
  -- this function just returns a singleton list configured in the right way
  preProcess :: p -> ConfiguredPreProcessor -> ToolMonad [ConfiguredTool]
  preProcess _ = defaultPreProcess
  -- | Returns the @Flag@ or string to use for an input file
  inputFileFlagArg :: p -> String -> ToolMonad Setting
  inputFileFlagArg _ = return . Right
  -- | This is provided for symmetry with @inputFileFlagArg@
  outputFileFlagArg :: p -> String -> ToolMonad Setting
  outputFileFlagArg p s = do 
    opf <- outputFileFlag p s
    return $ Left opf

-- | The class for a compiler.
-- This provides the interface
-- of how to deal with packages. A compiler is nothing more
-- than a @PreProcessor@ together with packages. 
class PreProcessor pm => Compiler pm where
  --init :: pm -> ConfiguredCompiler -> String -> ToolMonad ConfiguredTool
  --init _ _ _ = throwError $ Exception "This operation is not supported"
  --register :: pm -> ConfiguredCompiler -> String -> ToolMonad ConfiguredTool
  --register _ _ _ = throwError $ Exception "This operation is not supported"
  --unregister :: pm -> ConfiguredCompiler -> String -> ToolMonad ConfiguredTool
  --unregister _ _ _ = throwError $ Exception "This operation is not supported"
  --expose :: pm -> ConfiguredCompiler -> String -> ToolMonad ConfiguredTool
  --expose _ _ _ = throwError $ Exception "This operation is not supported"
  --hide :: pm -> ConfiguredCompiler -> String -> ToolMonad ConfiguredTool
  --hide _ _ _ = throwError $ Exception "This operation is not supported"
  --list :: pm -> ConfiguredCompiler -> String -> ToolMonad ConfiguredTool
  --list _ _ _ = throwError $ Exception "This operation is not supported"
  getLibInstallExtensions :: pm -> ConfiguredProgram -> Target -> ToolMonad [String]
  getLibInstallExtensions _ _ _ = throwError $ Exception "Non existing library install extensions"
  getLibInstallDir :: pm -> ConfiguredProgram -> Target -> PackageDb -> ToolMonad FilePath
  getLibInstallDir _ _ _ _ = throwError $ Exception "Non existing library install directory"
  getExeInstallDir :: pm -> ConfiguredProgram -> Target -> PackageDb -> ToolMonad FilePath
  getExeInstallDir _ _ _ _ = throwError $ Exception "Non existing executable install directory"
  getIncludeInstallDir :: pm -> ConfiguredProgram -> PackageDb -> ToolMonad FilePath
  getIncludeInstallDir _ _ _ = throwError $ Exception "Non existing include data file install directory"
  -- | Returns the flag for using a particular package
  packageFlag :: pm -> ConfiguredCompiler -> String -> ToolMonad Flag
  packageFlag _ _ _ = throwError $ Exception "Non existing package flag"
  -- | Returns the flag for hiding all packages
  hideAllPackagesFlag :: pm -> ConfiguredCompiler -> ToolMonad Flag
  hideAllPackagesFlag _ _ = throwError $ Exception "Non existing hide all packages flag"

--class (Compiler c, OutputTool c) => Compiler c where
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
-- | This class simplifies use of the
-- @ToolMonad@. It is nearly fully typed
-- except for configure
class Show s => StateActions s where
  -- | This turns a class into a configuration.
  -- E.g. a @Tool@ into a @ConfiguredTool@, a 
  -- @OutputTool@ into a @ConfiguredOutputTool@
  -- ...
  configure :: s -> ToolMonad AnyTool
  configure _ = throwError $ NotConfigurableException "Trying to configure a configured program"
  -- | This invokes a configuration. How invocation
  -- happens depends on the configuration
  invokeState :: s ->  ToolMonad String
  invokeState _ = throwError $ NotInvokableException "Trying to invoke a not-configured program"
  -- | Turns it into the existential version of this class
  toAnyTool :: s -> AnyTool
  toAnyTool = AnyTool
  -- | Same as @supportedFlags@. It is called differently to
  -- avoid name clashes
  getPossibleSettings :: s -> ToolMonad (DSet Flag)
  getPossibleSettings _ = throwError $ Exception "Program has no settings"
  -- | Adds a flag to a configuration
  setFlag :: s -> Flag -> ToolMonad s
  setFlag _ _ = throwError $ Exception "Program has no settings"
  -- | Removes a flag from a configuration
  resetFlag :: s -> Flag -> ToolMonad s
  resetFlag _ _ = throwError $ Exception "Program has no settings"
  -- | Validates the flags inside a configuration
  checkFlags :: s -> ToolMonad Bool
  checkFlags _ = throwError $ Exception "Program has no settings"
  -- | Gets the flags inside the configuration
  getCurrentFlags :: s -> ToolMonad (Set Flag)
  getCurrentFlags _ = throwError $ Exception "Program has no settings"
  -- | Sets the output directory. 
  -- Only valid for configurations of an @OutputTool@
  setOutputDir :: s -> String -> ToolMonad s
  setOutputDir _ _ = throwError $ Exception "Program has no output directory"
  -- | Adds an input or search directory.
  -- Only valid for configurations of an @OutputTool@
  addInputDir :: s -> String -> ToolMonad s
  addInputDir _ _ = throwError $ Exception "Program has no input directory"
  -- | Sets the output file.
  -- Only valid for configurations of an @OutputTool@
  setOutputFile :: s -> String -> ToolMonad s
  setOutputFile _ _ = throwError $ Exception "Program has no output files"
  -- | Adds an input or source file
  -- Only valid for configurations of an @OutputTool@
  addInputFile :: s -> String -> ToolMonad s
  addInputFile _ _ = throwError $ Exception "Program has no input files"
  -- | Adds an argument to a configuration
  addArgument :: s -> String -> ToolMonad s
  addArgument _ _ = throwError $ Exception "Program has no arguments"
  -- | Sets the base directory for files to 
  -- be preprocessed.
  -- Only valid for configurations of a @PreProcess@
  setBaseDir :: s -> String -> ToolMonad s
  setBaseDir _ _ = throwError $ Exception "Program has no arguments"
  -- | Adds a file to be preprocessed (for manual preprocessing).
  -- Only valid for configurations of a @PreProcess@
  addPreProcess :: s -> PreProcess -> ToolMonad s
  addPreProcess _ _ = throwError $ Exception "Program has no arguments"
  -- | Execute @TransFormStep@ on a @PreProcess@ configuration.
  -- Different name is to avoid name clashes.
  prepareTransForm :: s -> Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ConfiguredTool
  prepareTransForm _ _ _ _ _ = throwError $ Exception "Program is no preprocessor"

  getHidePkgsFlag :: s -> ToolMonad Flag
  getHidePkgsFlag _ = throwError $ Exception "Program does not have a hide all packages flag" 

  getPackageFlag :: s -> String -> ToolMonad Flag
  getPackageFlag _ _ = throwError $ Exception "Program does not have a package flag" 

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

-- Verifies the flags inside a ConfiguredTool
verifyFlags :: Tool t => t -> ConfiguredTool -> ToolMonad Bool
verifyFlags t ct = do
  sfs <- supportedFlags t $ _configuredProgram ct
  return $ validFlags (_flags ct) sfs

-- | This is the default invoke. It just constructs
-- the command line arguments and runs the program.
-- This version prints first the flags and then the
-- arguments on the commang line.
defaultInvoke :: ConfiguredTool -> ToolMonad String
defaultInvoke = defaultInvoke' False

-- | This is the default invoke. It just constructs
-- the command line arguments and runs the program.
-- It has a boolean to control whether the arguments
-- (@True@) or the flags (@False@) should be printed
-- first on the command line
defaultInvoke' :: Bool -> ConfiguredTool -> ToolMonad String
defaultInvoke' a ct = do
  s <- get
  verb <- return $ _verbosity s
  arg <- sequence argM
  info $ (programId $ _configuredProgram ct) ++ " " ++ (intercalate " " $ concat arg)
  liftIO $ getProgramOutput verb (_configuredProgram ct) $ concat arg
  where argM = if a then args ++ fs else fs ++ args
        args = map (argToString t) $ _arguments ct
        fs = map (flagToString t) $ S.toList $ _flags ct
        t = _getTool ct

-- | Constructs an empty @ConfiguredTool@.
emptyConfigureTool :: Tool t => t -> ConfiguredTool
emptyConfigureTool t = ConfiguredTool (KnownTool t) cp S.empty []
  where cp = getProgram t

-- | This constructs a @ConfiguredTool@ with the supplied flags and arguments
configureTool :: Tool t => t -> Set Flag -> [String] -> ConfiguredTool
configureTool t f a = (emptyConfigureTool t) {_flags = f, _arguments = a}

---------------------------------------
-- * OutputTool
---------------------------------------

-- | The default version to fully configure 
-- a @ConfiguredOutputTool@. It adds input files
-- as arguments and adds the input directories and
-- output file as flags (using the flags defined in
-- the @OutputTool@ instance)
defaultConfigureForInvoke :: ConfiguredOutputTool -> ToolMonad ConfiguredTool
defaultConfigureForInvoke (ConfiguredOutputTool t ct iDirs iFiles oDir oFile) = do 
  iFlags <- if null iDirs then return [] else catchError (mapM (inputDirFlag t) iDirs) (\_ -> return [])
  oFlag <- if isNothing oDir then return [] else catchError (sequence [outputDirFlag t $ fromJust oDir]) (\_ -> return [])
  oFFlag <- if isNothing oFile then return [] else catchError (sequence [outputFileFlag t $ fromJust oFile]) (\_ -> return [])
  return $ ct {_arguments = args, _flags = S.union (S.fromList $ iFlags ++ oFlag ++ oFFlag) $ _flags ct}
  where args = _arguments ct ++ iFiles

-- | Constructs an empty @ConfiguredOutputTool@
emptyConfigureOutputTool :: OutputTool t => t -> ConfiguredOutputTool
emptyConfigureOutputTool t = newConfigureOutputTool t ct
  where ct = emptyConfigureTool t

-- | Contructs an empty @ConfiguredOutputTool@ based
-- on a @ConfiguredTool@
newConfigureOutputTool :: OutputTool t => t -> ConfiguredTool -> ConfiguredOutputTool
newConfigureOutputTool t ct = ConfiguredOutputTool (KnownOutputTool t) ct [] [] Nothing Nothing

-- | Constructs a @ConfiguredOutputTool@ with the supplied
-- input directories, output directory, input files and 
-- output file
configureOutputTool :: OutputTool t => t -> [FilePath] -> Maybe FilePath -> [FilePath] -> Maybe FilePath -> ConfiguredOutputTool
configureOutputTool t = configureFullOutputTool t ct
  where ct = emptyConfigureTool t

-- | Constructs a @ConfiguredOutputTool@ with the supplied
-- input directories, output directory, input files and 
-- output file and @ConfiguredTool@
configureFullOutputTool :: OutputTool t => t -> ConfiguredTool -> [FilePath] -> Maybe FilePath -> [FilePath] -> Maybe FilePath -> ConfiguredOutputTool
configureFullOutputTool t ct iDirs oDir iFiles oFile = (newConfigureOutputTool t ct) {_inputDirs = iDirs, _outputDir = oDir, _inputFiles = iFiles, _outputFile = oFile}

---------------------------------------
-- * PreProcessor
---------------------------------------

-- | The defualt preprocess. For each file
-- to be preprocessed a @ConfiguredTool is constructed.
-- It just maps the input, output file and supplied flags
-- to flags and/or arguments inside the @ConfiguredTool@
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

-- | Constructs an empty @ConfiguredPreProcessor@
emptyConfigurePreProcessor :: PreProcessor t => t -> ConfiguredPreProcessor
emptyConfigurePreProcessor t = newConfigurePreProcessor t ct
  where ct = emptyConfigureOutputTool t

-- | Constructs an empty @ConfiguredPreProcessor@ based upon
-- a @ConfiguredOutputTool@
newConfigurePreProcessor :: PreProcessor t => t -> ConfiguredOutputTool -> ConfiguredPreProcessor
newConfigurePreProcessor t ct = ConfiguredPreProcessor (KnownPreProcessor t) ct [] ""

-- | Constructs a @ConfiguredPreProcessor@ using the
-- supplied files to be preprocessed and the base directory
configurePreProcessor :: PreProcessor t => t -> [(FilePath, FilePath, Set Flag)] -> FilePath -> ConfiguredPreProcessor
configurePreProcessor t = configureFullPreProcessor t ct
  where ct = emptyConfigureOutputTool t

-- | Constructs a @ConfiguredPreProcessor@ using the
-- supplied files to be preprocessed the base directory and
-- a @ConfiguredOutputTool@
configureFullPreProcessor :: PreProcessor t => t -> ConfiguredOutputTool -> [(FilePath, FilePath, Set Flag)] -> FilePath -> ConfiguredPreProcessor
configureFullPreProcessor t ct f bDir = (newConfigurePreProcessor t ct) {_processFiles = f, _baseDir = bDir}

---------------------------------------
-- * Compiler
---------------------------------------

-- | Constructs an empty @ConfiguredCompiler@
emptyConfigureCompiler :: Compiler t => t -> ConfiguredCompiler
emptyConfigureCompiler t = newConfigureCompiler t ct
  where ct = emptyConfigurePreProcessor t

-- | Constructs an empty @ConfiguredCompiler@
-- based upon a @ConfiguredPreProcessor@
newConfigureCompiler :: Compiler t => t -> ConfiguredPreProcessor -> ConfiguredCompiler
newConfigureCompiler t ct = ConfiguredCompiler (KnownCompiler t) ct []

-- | Constructs a @ConfiguredCompiler@ using
-- the supplied packaged databases
configureCompiler :: Compiler t => t -> [PackageDb] -> ConfiguredCompiler
configureCompiler t = configureFullCompiler t ct
  where ct = emptyConfigurePreProcessor t

-- | Constructs a @ConfiguredCompiler@ using
-- the supplied packaged databases and @ConfiguredPreProcessor@
configureFullCompiler :: Compiler t => t -> ConfiguredPreProcessor -> [PackageDb] -> ConfiguredCompiler
configureFullCompiler t ct f = (newConfigureCompiler t ct) {_packageDb = f}

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
--  where ct1 = _configuredToolPM $ _configuredCompilerComp c
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
--        pm = emptyConfigureCompiler t

--newConfigureCompiler :: Compiler t => t -> ConfiguredOutputTool -> ConfiguredCompiler -> ConfiguredCompiler
--newConfigureCompiler t ct pm = ConfiguredCompiler (KnownCompiler t) ct pm S.empty S.empty False

--configureCompiler :: Compiler t => t -> Set Setting -> Set Setting -> Bool -> ConfiguredCompiler
--configureCompiler t = configureFullCompiler t ct pm
--  where ct = emptyConfigureOutputTool t
--        pm = emptyConfigureCompiler t

--configureFullCompiler :: Compiler t => t -> ConfiguredOutputTool -> ConfiguredCompiler -> Set Setting -> Set Setting -> Bool -> ConfiguredCompiler
--configureFullCompiler t ct pm b f e = (newConfigureCompiler t ct pm) {_backends = b, _flavours = f, _executable = e}

---------------------------------------
-- * Messages
---------------------------------------

-- | Retrieve the @Verbosity@ from the
-- @StateT@ inside the @ToolMonad@
getVerbosity :: ToolMonad Verbosity
getVerbosity = do 
  x <- get
  return $ _verbosity x

-- | Prints a warning to the screen.
-- This are shown at normal verbosity
warn :: String -> ToolMonad ()
warn msg = do
  v <- getVerbosity
  liftIO $ printMessage normal v ("Warning: " ++ msg)

-- | Prints a debug message to the screen.
-- This are shown at deafening verbosity
debug :: String -> ToolMonad ()
debug msg = do
  v <- getVerbosity
  liftIO $ printMessage deafening v msg

-- | Print a message to the screen.
-- This are shown at normal verbosity
notice :: String -> ToolMonad ()
notice msg = do
  v <- getVerbosity
  liftIO $ printMessage normal v msg

-- | Print a warning to the screen.
-- This are shown at verbose verbosity
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
makeLenses ''ConfiguredCompiler

---------------------------------------
-- * ToolMonad
---------------------------------------

-- | The basic state of the @ToolMonad@
baseState :: Verbosity -> ToolState
baseState v = ToolState 0 v Nothing "Empty" M.empty

-- | Runs the @ToolMonad@.
-- It returns either the result or the
-- error message if an error was thrown
run :: Verbosity -> ToolMonad a -> IO (Either ErrorType a)
run v m = evalStateT (runExceptT m) (baseState v)

-- | Runs the @ToolMonad@.
-- This will call error if an error occurred
-- during computation.
runError :: Verbosity -> ToolMonad a -> IO a
runError v t = do 
  r <- run v t
  case r of
    Left e -> error $ show e
    Right a -> return a

-- | Do a computation depending on the
-- verbosity set inside the state.
-- It throws away any result
withVerbosity :: Verbosity -> ToolMonad a -> ToolMonad ()
withVerbosity atVerbosity m = do
  v <- use verbosity
  when (v >= atVerbosity) $ m >> return ()

---------------------------------------
-- ** CurrentMonad
---------------------------------------

-- | Saves and restores the current @AnyTool@.
-- It returns the result of the inner computation
withOtherCurrent :: AnyTool -> ToolMonad a -> ToolMonad a
withOtherCurrent a t = do
  x <- use current
  load a
  r <- t
  current .= x
  return r

-- | Adds a @Tool@ to the store inside the
-- state using the specified name
storeTool :: Tool t => String -> t -> ToolMonad ()
storeTool s = store s . KnownTool 

-- | Adds a @OutputTool@ to the store inside the
-- state using the specified name
storeOutputTool :: OutputTool t => String -> t -> ToolMonad ()
storeOutputTool s = store s . KnownOutputTool 

-- | Adds a @PreProcessor@ to the store inside the
-- state using the specified name
storePreProcessor :: PreProcessor t => String -> t -> ToolMonad ()
storePreProcessor s = store s . KnownPreProcessor 

-- | Adds a @Compiler@ to the store inside the
-- state using the specified name
storeCompiler :: Compiler t => String -> t -> ToolMonad ()
storeCompiler s = store s . KnownCompiler

-- | Adds a @StateActions@ to the store inside the
-- state using the specified name
store :: StateActions t => String -> t -> ToolMonad ()
store s t = toolStore %= M.insert s (toAnyTool t)

-- | Adds a @Tool@ to the store inside the
-- state using a fresh name. The name is returned
storeAnyTool :: StateActions t => t -> ToolMonad String
storeAnyTool t = do
  x <- uses fresh show
  store x t
  fresh += 1
  return x

-- | Sets a @StateActions@ as the current
-- @AnyTool@ inside the state
load :: StateActions t => t -> ToolMonad ()
load t = loadAnyTool $ toAnyTool t

-- | Existential version of @load@
loadAnyTool :: AnyTool -> ToolMonad ()
loadAnyTool t = current .= Just t

-- | Sets the specified @AnyTool@ as
-- the current. It throws an error
-- if the name could not be found in the store
loadFromStore :: String -> ToolMonad ()
loadFromStore s = do
  x <- uses toolStore $ M.lookup s
  if (isJust x)
    then (saveName .= s)
    else (throwError $ Exception "AnyTool could not be found in the store")
  current .= x

-- | Write the current @AnyTool@ to the store.
-- The name to use is stored inside the state (@saveName@)
save :: ToolMonad ()
save = do
  s <- use saveName
  saveCurrent s

-- | Write the current @AnyTool@ to the store
-- using the supplied name.
saveCurrent :: String -> ToolMonad ()
saveCurrent s = do
  x <- getCurrent
  store s x

-- | Sets the @saveName@ inside the state
setSaveName :: String -> ToolMonad ()
setSaveName s = saveName .= s

-- | Retrieves the current @AnyTool@ from
-- the state. It throws an error if no
-- current was set.
getCurrent :: ToolMonad AnyTool
getCurrent = do
  x <- use current
  if (isNothing x)
    then throwError $ Exception "No current AnyTool present"
    else return $ fromJust x

-- | Resets the current @AnyTool@ to @Nothing@
resetCurrent :: ToolMonad ()
resetCurrent = current .= Nothing

-- | @configure@ the current @AnyTool@
-- and set the configuration as the current
makeConfiguration :: ToolMonad ()
makeConfiguration = do
  x <- getCurrent
  ct <- configure x
  current .= (Just ct)

-- | Find all supported @Flag@s with the specified name.
-- Throws an error if the flag is not supported
-- by the current @AnyTool@ or if the name can refer
-- to two or more distinct flags
findFlags :: String -> ToolMonad [Flag]
findFlags s = do
  x <- getCurrent
  flags <- getPossibleSettings x
  case getFlags s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambiguous if short or long flag is used" 
    Just x -> return x

-- | Find the supported @Flag@ with the specified name.
-- Throws an error if the flag is not supported
-- by the current @AnyTool@, if the name can refer
-- to two or more distinct flags or the flag requires
-- an argument
findFlag :: String -> ToolMonad Flag
findFlag s = do
  x <- getCurrent
  flags <- getPossibleSettings x
  case getEmptyFlag s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

-- Find all @Flag@s of the given name that are
-- present in the current configuration.
-- Throws an error if the flag is not set
-- in the current configuration or if the name can refer
-- to two or more distinct flags
findSetFlags :: String -> ToolMonad [Flag]
findSetFlags s = do
  x <- getCurrent
  flags <- getCurrentFlags x
  case getFlags s flags of
    Nothing -> throwError $ Exception "Flag not found, or ambiguous if short or long flag is used" 
    Just x -> return x

-- | Find the @Flag@ with the specified name and argument
-- in the current configuration.
-- Throws an error if the flag is not set with the given argument
-- in the current configuration or if the name can refer
-- to two or more distinct flags
findSetFlag :: String -> String -> ToolMonad Flag
findSetFlag s a = do
  fs <- findSetFlags s
  case findFlagWithArg a fs of
    Nothing -> throwError $ Exception "Flag not found" 
    Just x -> return x 

-- | Sets the flag with the given name
-- inside the current configuration.
-- This uses @findFlag@
setSetting :: String -> ToolMonad ()
setSetting s = do
  x <- getCurrent
  f <- findFlag s
  ct <- setFlag x f
  current .= (Just $ toAnyTool ct)

-- | Removes all flags with the given name
-- from the configuration.
-- This uses @findSetFlags@
resetSetting :: String  -> ToolMonad ()
resetSetting s = do
  x <- getCurrent
  f <- findSetFlags s
  ct <- foldM resetFlag x f
  current .= (Just $ toAnyTool ct)

-- | Sets the flag with the given name and argument
-- inside the current configuration.
-- This uses @findFlag@
addSetting :: String -> String -> ToolMonad ()
addSetting s a = do
  x <- getCurrent
  f <- findFlag s
  ct <- setFlag x (f {_defaultArgument = Just a})
  current .= (Just $ toAnyTool ct)

-- | This is a combination or @resetSettting@
-- followed by @addSetting@
replaceSetting :: String -> String -> ToolMonad ()
replaceSetting s a = do
  resetSetting s
  addSetting s a

-- | Singleton version of @resetSetting@. This 
-- removes only the @Flag@ with the specified argument
removeSetting :: String -> String -> ToolMonad ()
removeSetting s a = do
  x <- getCurrent
  f <- findSetFlag s a
  ct <- resetFlag x f
  current .= (Just $ toAnyTool ct)

-- | Validates the flags inside the current
-- configuration
validateFlags :: ToolMonad Bool
validateFlags = do
  x <- getCurrent
  checkFlags x

-- | Adds a @Flag@ to the current
-- configuration
addFlag :: Flag -> ToolMonad ()
addFlag f = do
  x <- getCurrent
  ct <- setFlag x f
  current .= (Just $ toAnyTool ct)

-- | Combination of @removeFlag@ followed
-- by @addFlag@
replaceFlag :: Flag -> Flag -> ToolMonad ()
replaceFlag s f = do
  removeFlag s
  addFlag f

-- | Removes the @Flag@ from the current
-- configuration
removeFlag :: Flag -> ToolMonad ()
removeFlag f = do
  x <- getCurrent
  ct <- resetFlag x f
  current .= (Just $ toAnyTool ct)

-- | Invokes the current configuration
invoke :: ToolMonad String
invoke = do
  x <- getCurrent
  invokeState x

-- | Adds a @PreProcess@ to the current
-- @PreProcessor@ configuration
setPreProcess :: String -> String -> Set Flag -> ToolMonad ()
setPreProcess i o fs = do
  x <- getCurrent
  ct <- addPreProcess x (i, o, fs)
  current .= (Just $ toAnyTool ct)

-- | Adds an argument to the current configuration
setArgument :: String -> ToolMonad ()
setArgument f = do
  x <- getCurrent
  ct <- addArgument x f
  current .= (Just $ toAnyTool ct)

-- | Adds an input file to the current
-- @PreProcessor@ configuration
addInputFileC :: String -> ToolMonad ()
addInputFileC f = do
  x <- getCurrent
  ct <- addInputFile x f
  current .= (Just $ toAnyTool ct)

-- | Sets the output file of the 
-- current @PreProcessor@ configuration
setOutputFileC :: String -> ToolMonad ()
setOutputFileC f = do
  x <- getCurrent
  ct <- setOutputFile x f
  current .= (Just $ toAnyTool ct)

-- | Sets the output directory of the 
-- current @PreProcessor@ configuration
setOutputDirC :: String -> ToolMonad ()
setOutputDirC f = do
  x <- getCurrent
  ct <- setOutputDir x f
  current .= (Just $ toAnyTool ct)

-- | Adds an input or search directory
-- to the current @PreProcessor@ configuration
addInputDirC :: String -> ToolMonad ()
addInputDirC f = do
  x <- getCurrent
  ct <- addInputDir x f
  current .= (Just $ toAnyTool ct)

-- | Prints the current @AnyTool@ to
-- the screen. This should only be used
-- for debugging purposes.
printCurrent :: ToolMonad ()
printCurrent = do
  x <- use current
  liftIO $ print x

-- | Calls @prepareTransForm@ on the current
-- @PreProcessor@ configuration
prepareTransFormC :: Target -> String -> FilePath -> [[FilePath]] -> ToolMonad ()
prepareTransFormC t tt fp deps = do
  x <- getCurrent
  ct <- prepareTransForm x t tt fp deps
  current .= (Just $ toAnyTool ct)

getHidePkgsFlagC :: ToolMonad Flag
getHidePkgsFlagC = do
  x <- getCurrent
  getHidePkgsFlag x

getPackageFlagC :: String -> ToolMonad Flag
getPackageFlagC s = do
  x <- getCurrent
  --printCurrent
  getPackageFlag x s

---------------------------------------
-- ** TypedMonad
---------------------------------------

-- | Retrieve the specified @AnyTool@ from the store.
-- Throws an error if the @AnyTool@ could not be found
loadFromStore' :: String -> ToolMonad AnyTool
loadFromStore' s = do
  x <- uses toolStore $ M.lookup s
  if (isJust x)
    then return $ fromJust x
    else throwError $ Exception "AnyTool could not be found in the store"

-- | Same as @makeConfiguration@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
makeConfiguration' :: StateActions s => s -> ToolMonad AnyTool
makeConfiguration' = configure

-- | Same as @findFlags@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
findFlags' :: StateActions s => s -> String -> ToolMonad [Flag]
findFlags' x s = do
  flags <- getPossibleSettings x
  case getFlags s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

-- | Same as @findFlag@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
findFlag' :: StateActions s => s -> String -> ToolMonad Flag
findFlag' x s = do
  flags <- getPossibleSettings x
  case getEmptyFlag s (dSetToSet flags) of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

-- | Same as @findSetFlags@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
findSetFlags' :: StateActions s => s -> String -> ToolMonad [Flag]
findSetFlags' x s = do
  flags <- getCurrentFlags x
  case getFlags s flags of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x

-- | Same as @findSetFlag@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
findSetFlag' :: StateActions s => s -> String -> String -> ToolMonad Flag
findSetFlag' x s a = do
  flags <- getCurrentFlags x
  fs <- case getFlags s flags of
    Nothing -> throwError $ Exception "Flag not found, or ambigues if short or long flag is used" 
    Just x -> return x
  case findFlagWithArg a fs of
    Nothing -> throwError $ Exception "Flag not found" 
    Just x -> return x 

-- | Same as @setSetting@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
setSetting' :: StateActions s => s -> String -> ToolMonad s
setSetting' x s = do
  f <- findFlag' x s
  setFlag x f

-- | Same as @resetSetting@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
resetSetting' :: StateActions s => s -> String -> ToolMonad s
resetSetting' x s = do
  f <- findSetFlags' x s
  foldM resetFlag x f

-- | Same as @addSetting@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
addSetting' :: StateActions s => s -> String -> String -> ToolMonad s
addSetting' x s a = do
  f <- findFlag' x s
  setFlag x (f {_defaultArgument = Just a})

-- | Same as @replaceSetting@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
replaceSetting' :: StateActions s => s -> String -> String -> ToolMonad s
replaceSetting' ox s a = do
  x <- resetSetting' ox s
  addSetting' x s a

-- | Same as @removeSetting@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
removeSetting' :: StateActions s => s -> String -> String -> ToolMonad s
removeSetting' x s a = do
  f <- findSetFlag' x s a
  resetFlag x f

-- | Same as @validateFlags@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
validateFlags' :: StateActions s => s -> ToolMonad Bool
validateFlags' = checkFlags

-- | Same as @addFlag@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
addFlag' :: StateActions s => s -> Flag -> ToolMonad s
addFlag' = setFlag

-- | Same as @replaceFlag@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
replaceFlag' :: StateActions s => s -> Flag -> Flag -> ToolMonad s
replaceFlag' ox s f = do
  x <- removeFlag' ox s
  addFlag' x f

-- | Same as @removeFlag@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
removeFlag' :: StateActions s => s -> Flag -> ToolMonad s
removeFlag' = resetFlag

-- | Invokes the supplied configuration
invoke' :: StateActions s => s -> ToolMonad String
invoke' = invokeState

-- | Same as @setPreProcess@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
setPreProcess' :: StateActions s => s -> String -> String -> Set Flag -> ToolMonad s
setPreProcess' x i o fs = addPreProcess x (i, o, fs)

-- | Same as @setArgument@, but using
-- the supplied @StateActions@ instead of the current
-- @AnyTool@
setArgument' :: StateActions s => s -> String -> ToolMonad s
setArgument' = addArgument 

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

instance Tool KnownCompiler where
  getName (KnownCompiler t) = getName t
  getProgram (KnownCompiler t) = getProgram t
  supportedFlags (KnownCompiler t) = supportedFlags t
  supportedArguments (KnownCompiler t) = supportedArguments t
  optionalArguments (KnownCompiler t) = optionalArguments t
  requiredArguments (KnownCompiler t) = requiredArguments t
  invokeTool (KnownCompiler t) = invokeTool t
  argToString (KnownCompiler t) = argToString t
  flagToString (KnownCompiler t) = flagToString t

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

instance OutputTool KnownCompiler where
  inputDirFlag (KnownCompiler t) = inputDirFlag t
  outputDirFlag (KnownCompiler t) = outputDirFlag t
  outputFileFlag (KnownCompiler t) = outputFileFlag t
  configureForInvoke (KnownCompiler t) = configureForInvoke t

---------------------------------------
-- ** PreProcessor Instances
---------------------------------------

instance PreProcessor KnownPreProcessor where
  supportedTargets (KnownPreProcessor p) = supportedTargets p
  targetTypes (KnownPreProcessor p) = targetTypes p
  directDependencies (KnownPreProcessor p) = directDependencies p
  indirectDependencies (KnownPreProcessor p) = indirectDependencies p
  transFormStep (KnownPreProcessor p) = transFormStep p
  preProcess (KnownPreProcessor p) = preProcess p
  inputFileFlagArg (KnownPreProcessor p) = inputFileFlagArg p
  outputFileFlagArg (KnownPreProcessor p) = outputFileFlagArg p

instance PreProcessor KnownCompiler where
  supportedTargets (KnownCompiler p) = supportedTargets p
  targetTypes (KnownCompiler p) = targetTypes p
  directDependencies (KnownCompiler p) = directDependencies p
  indirectDependencies (KnownCompiler p) = indirectDependencies p
  transFormStep (KnownCompiler p) = transFormStep p
  preProcess (KnownCompiler p) = preProcess p
  inputFileFlagArg (KnownCompiler p) = inputFileFlagArg p
  outputFileFlagArg (KnownCompiler p) = outputFileFlagArg p

---------------------------------------
-- ** Compiler Instances
---------------------------------------

instance Compiler KnownCompiler where
  --init (KnownCompiler pm) = init pm
  --register (KnownCompiler pm) = register pm
  --unregister (KnownCompiler pm) = unregister pm
  --expose (KnownCompiler pm) = expose pm
  --hide (KnownCompiler pm) = hide pm
  --list (KnownCompiler pm) = list pm
  getLibInstallExtensions (KnownCompiler pm) = getLibInstallExtensions pm
  getLibInstallDir (KnownCompiler pm) = getLibInstallDir pm
  getExeInstallDir (KnownCompiler pm) = getExeInstallDir pm
  getIncludeInstallDir (KnownCompiler pm) = getIncludeInstallDir pm
  packageFlag (KnownCompiler pm) = packageFlag pm
  hideAllPackagesFlag (KnownCompiler pm) = hideAllPackagesFlag pm


--instance Compiler KnownCompiler where
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
  getHidePkgsFlag (AnyTool t) = getHidePkgsFlag t
  getPackageFlag (AnyTool t) = getPackageFlag t

instance StateActions KnownTool where
  configure (KnownTool t) = return $ toAnyTool $ emptyConfigureTool t

instance StateActions KnownOutputTool where
  configure (KnownOutputTool t) = return $ toAnyTool $ emptyConfigureOutputTool t

instance StateActions KnownPreProcessor where
  configure (KnownPreProcessor t) = return $ toAnyTool $ emptyConfigurePreProcessor t

instance StateActions KnownCompiler where
  configure (KnownCompiler t) = return $ toAnyTool $ emptyConfigureCompiler t

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

instance StateActions ConfiguredCompiler where
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
  getHidePkgsFlag ct = hideAllPackagesFlag (ct ^. getCompiler) ct
  getPackageFlag ct = packageFlag (ct ^. getCompiler) ct
module Tools.Cabal.Shake where

import Development.Shake hiding (Verbosity)
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Lens ((^.))

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Control.Monad (when)
import Data.Version (showVersion)
import Data.List (isPrefixOf, elemIndices)

import Cabal
import Tools.Cabal.FileFormat
import Tools.Cabal.BuildInfo
import Tools.BasicTools.Shared
import Tools.Core.Types 
import Tools.Core.ToolMonad
import Tools.Core.TypedMonad (StateActions)
import Tools.Flag

import Debug

--getAllExeNames :: FileFormat -> [String]
--getAllExeNames ff = getAllExeNames' $ ff ^. executables
--  where getAllExeNames' [] = []
--        getAllExeNames' (x:xs) = x ^. exeName : getAllExeNames' xs

-- | This is the main build function.
-- It builds the shake build system based on
-- the supplied @FileFormat@ and @BuildInfo@.
-- Supply ¨build¨, ¨clean¨ or ¨install¨ as
-- command line arguments to
-- execute that action.
mainShake :: FileFormat -> BuildInfo -> IO ()
mainShake ff bi = shakeArgs shakeOptions{shakeFiles=odir ++ "/"} $ do
  want ["build"]

  phony "clean" $ do
    removeFilesAfter odir ["//*"]

  phony "install" $ do
    i <- return $ getInstallIncludeSources bi ff
    s <- return $ getLibInstallSources bi ff
    e <- return $ getAllExeNames bi ff
    t <- return $ getAllToolNames bi ff
    l <- return $ getAllLibNames bi ff
    xs <- return $ map (\(p,f) -> p </> f) $ i ++ s ++ e ++ t ++ l
    need xs

  getInstallRules bi ff
    
  -- TODO only works for default backend
  phony "build" $ do
    s <- return $ getLibInstallSources bi ff
    e <- return $ getAllExeNames bi ff
    t <- return $ getAllToolNames bi ff
    l <- return $ getAllLibNames bi ff
    let g = map (\(_,f) -> odir </> f)
    need $ g t
    need $ g $ s ++ e ++ l

  mapM_ (compilerExeRules bi vers gsi) exes
  
  mapM_ (compilerExeRules bi {_targets = bi ^. toolTargets, _flavours = bi ^. toolFlavours} vers gsi) tls
  
  when (isJust lib) $ do
    compilerLibRules bi vers gsi tLib

  compilerGlobalRules bi vers gsi

  mapM_ (preprocessRules bi) $ getPreprocessSources ff 

  -- Any needed file for which there is no explicit rule
  -- is just copied to the build directory
  -- this allows the other rules to just assume the file exists in
  -- the build directory regardless of whether it was generated or not.
  (odir ++ "//*") *> \out -> do
    let source = dropDirectory1 out
    need [source]
    copyFile' source out
  where exes = ff ^. executables
        tls = ff ^. tools
        lib = ff ^. library
        tLib = fromJust lib
        odir = bi ^. outputDirectory
        gsi = ff ^. pkgSourceInfo
        vers = ff ^. version

getAllExeNames :: BuildInfo -> FileFormat -> [(String, String)]
getAllExeNames bi ff = [(expandDir (ff ^. pkgName) (ff ^. version) $ (bi ^. installExeDir) M.! t, expandName exeTemp (ff ^. version) eName) 
  | t <- ts, eName <- exes, exeTemp <- (bi ^. exeNames) M.! t]
  where ts = map (++ " executable") [y ++ " " ++ x | y <- bi ^. flavours, x <- bi ^. targets]
        exes = [e ^. exeName | e <- ff ^. executables]

getAllToolNames :: BuildInfo -> FileFormat -> [(String, String)]
getAllToolNames bi ff = [(expandDir (ff ^. pkgName) (ff ^. version) $ (bi ^. installExeDir) M.! t, expandName toolTemp (ff ^. version) toolName)
  | t <- ts, toolName <- tls, toolTemp <- (bi ^. exeNames) M.! t]
  where ts = map (++ " executable") [y ++ " " ++ x | y <- bi ^. flavours, x <- bi ^. targets]
        tls = [e ^. exeName | e <- ff ^. tools]

getAllLibNames :: BuildInfo -> FileFormat -> [(String, String)]
getAllLibNames bi ff = [(expandDir (ff ^. pkgName) (ff ^. version) $ (bi ^. installLibDir) M.! t, expandName libTemp (ff ^. version) lName)
  | t <- ts, lName <- lib, libTemp <- (bi ^. libNames) M.! t]
  where ts = map (++ " library") [y ++ " " ++ x | y <- bi ^. flavours, x <- bi ^. targets]
        hasLib = isJust mLib
        mLib = ff ^.library
        lib = maybe [] (\x->[x ^. libName]) mLib

getInstallRules :: BuildInfo -> FileFormat -> Rules ()
getInstallRules bi ff = mapM_ getInstallRule $ (map (toTrip "") inc) ++ (map (toTrip $ bi ^. outputDirectory) s)
  where inc = getInstallIncludeSources bi ff
        s = getLibInstallSources bi ff
        toTrip p (x,y) = (x,p,y)

getInstallRule :: (String, String, String) -> Rules ()
getInstallRule (ip, sp, f) = ip </> f *> \out -> do
  need [sp </> f]
  copyFile' f out

getInstallIncludeSources :: BuildInfo -> FileFormat -> [(String, String)]
getInstallIncludeSources bi ff = 
  if hasLib && bi ^. installLibrary then
    map (\x -> (expandDir (ff ^. pkgName) (ff ^. version) $ bi ^. installIncludeDir ,x)) sources
  else
    []
  where hasLib = isJust mLib
        mLib = ff ^.library
        lib = fromJust mLib
        sources = getAllIncludeSources ff

-- | Retrieve all additional source files. 
getAllIncludeSources :: FileFormat -> [String]
getAllIncludeSources ff = S.toList $ s S.\\ t -- remove the generated sources as they are not
  where (s,t) = getIncludes $ concatMap (\(_,x) -> x) $ getLibPreprocessSources ff ++ ff ^. pkgSourceInfo ^. preprocessSources

getIncludes :: [PreProcess] -> (Set String, Set String)
getIncludes [] = (S.empty, S.empty)
getIncludes ((s,t,_):xs) = (S.insert s ss, S.insert t st)
  where (ss,st) = getIncludes xs

getLibInstallSources :: BuildInfo -> FileFormat -> [(String, String)]
getLibInstallSources bi ff = 
  if hasLib && bi ^. installLibrary then
    concat [map (\x -> (expandDir (ff ^. pkgName) (ff ^. version) $ (bi ^. installLibDir) M.! t, expandName ext (ff ^. version) (dropExtension x))) sources | t <- ts ++ tsg, ext <- (bi ^. installSources) M.! t]
  else
    []
  where hasLib = isJust mLib
        mLib = ff ^.library
        lib = fromJust mLib
        tsg = [y ++ " " ++ x | y <- bi ^. flavours, x <- bi ^. targets]
        ts =  map (++ " library") tsg
        sources = getAllLibSources ff
        comp = bi ^. compiler

getAllLibSources :: FileFormat -> [String]
getAllLibSources ff = if hasLib then 
    lib ^. libSourceInfo ^. sourceFiles ++ globalSources
  else
    []
  where hasLib = isJust mLib
        mLib = ff ^.library
        lib = fromJust mLib
        globalSources = ff ^. pkgSourceInfo ^. sourceFiles

getLibExposedSources :: FileFormat -> [String]
getLibExposedSources ff = if hasLib then lib ^. exposedModules else []
  where hasLib = isJust mLib
        mLib = ff ^.library
        lib = fromJust mLib


-- | The building rules for the library
compilerLibRules :: BuildInfo -> Version -> SourceInfo -> Library -> Rules ()
compilerLibRules bi vers gsi lib = do
  sequence_ [getRule bi vers gsi (lib ^. libSourceInfo) (lib ^. libName) Nothing t ext tt | t <- ts, (ext, tt) <- tTypes t, tt == "lib"]
  sequence_ [getRule bi vers gsi (lib ^. libSourceInfo) sf Nothing t ext tt | t <- ts ++ tsg, sf <- sfs, (ext, tt) <- tTypes t, isFileExt tt]
  where tsg = [y ++ " " ++ x | y <- bi ^. flavours, x <- bi ^. targets]
        ts = map (++ " library") tsg
        tTypes t = (bi ^. compilerTargetTypes) M.! t
        sfs = map dropExtension $ lib ^. libSourceInfo ^. sourceFiles

-- | The building rules for the executables and tools
compilerExeRules :: BuildInfo -> Version -> SourceInfo -> Executable -> Rules ()
compilerExeRules bi vers gsi ex = do
  sequence_ [getRule bi vers gsi (ex ^. exeSourceInfo) (ex ^. exeName) (Just $ ex ^. mainModulePath) t ext tt | t <- ts, (ext, tt) <- tTypes t, tt == "exe"]
  sequence_ [getRule bi vers gsi (ex ^. exeSourceInfo) sf (Just $ ex ^. mainModulePath) t ext tt | t <- ts ++ tsg, sf <- sfs, (ext, tt) <- tTypes t, isFileExt tt]
  where tsg = [y ++ " " ++ x | y <- bi ^. flavours, x <- bi ^. targets]
        ts = map (++ " executable") tsg
        tTypes t = (bi ^. compilerTargetTypes) M.! t
        sfs = map dropExtension $ ex ^. mainModulePath : ex ^. exeSourceInfo ^. sourceFiles

-- | The building rules for the source files in the global section of
-- the @FileFormat@
compilerGlobalRules :: BuildInfo -> Version -> SourceInfo -> Rules ()
compilerGlobalRules bi vers gsi = do
  sequence_ [getRule bi vers gsi emptySourceInfo sf Nothing t ext tt | t <- ts, sf <- sfs, (ext, tt) <- tTypes t, isFileExt tt]
  where ts = [y ++ " " ++ x | y <- bi ^. flavours, x <- bi ^. targets]
        tTypes t = (bi ^. compilerTargetTypes) M.! t
        sfs = map dropExtension $ gsi ^. sourceFiles

-- | Test if it is not an executable or library file
isFileExt :: String -> Bool
isFileExt tt = tt `notElem` ["exe", "lib"]


-- | This is a single build rule for a given target and file extension.
-- The parameters are as follows: 
-- buildInfo, 
-- Global sourceInfo, 
-- local sourceInfo (of the exe or lib),
-- Name of the file to be build, 
-- the name of the main module if applicable
-- Target, 
-- extension, 
-- TargetType
getRule :: BuildInfo -> Version -> SourceInfo -> SourceInfo -> String -> Maybe String -> Target -> String -> String -> Rules ()
getRule bi vers gsi si n m t ext tt = do
  name *> \out -> do
    let fileName = dropDirectory1 $ dropExtension out
    deps' <- runMonad v $ do
      dDeps <- directDependencies comp cp t tt
      return $ concatMap (exPandFileNames allSourceFiles m fileName) dDeps
    deps <- return $ map (odir </>) $ intersectWithSourceFiles allSourceFiles deps'
    need deps
    aDeps' <- runMonad v $ do
      iDeps <- indirectDependencies comp cp t tt deps
      return $ concatMap (exPandFileNames allSourceFiles m fileName) iDeps
    aDeps <- return $ map (odir </>) $ intersectWithSourceFiles allSourceFiles aDeps'
    need aDeps
    execComp bi pkgs $ do
      setOutputFile out
      prepareTransForm t tt out [deps,aDeps]
    return ()
  return ()

  where comp = bi ^. compiler
        odir = bi ^. outputDirectory
        name = odir </> expandFileName n vers ext
        cp = getProgram comp
        v = bi ^. verbosity
        pkgs = si ^. packages ++ gsi ^. packages
        sfs = gsi ^. sourceFiles
        allSourceFiles = maybe [] (:[]) m ++ sfs ++ si ^. sourceFiles        

-- | fileName, format
expandFileName :: String -> Version -> String -> String
expandFileName fileName v name = replaceWithSub "{x}" fileName $ replaceWithSub "{version}" (showVersion v) name

expandName :: String -> Version -> String -> String
expandName l v name = replaceWithSub "{x}" name $ replaceWithSub "{version}" (showVersion v) l

expandDir :: String -> Version -> String -> String
expandDir packageName v = replaceWithSub "{pkg}" packageName . replaceWithSub "{version}" (showVersion v)

replaceWithSub :: Eq a => [a] -> [a] -> [a] -> [a]
replaceWithSub search replace list = foldr f list ixs
  where ixs = indicesSubList search list
        f ix l = take ix l ++ replace ++ drop (ix + length search) l

indicesSubList :: Eq a => [a] -> [a] -> [Int]
indicesSubList [] _ = []
indicesSubList (x:xs) l = filter f i
  where i = elemIndices x l
        f y = (x:xs) `isPrefixOf` (drop y l)

-- | During dependency tracking of files also library files
-- not part of the local package can be detected. This will
-- filter out any such files.
-- It assumes that either all files with the same name but different extension are locally present (or generated)
-- or none are locally present
intersectWithSourceFiles :: [SourceFile] -> [SourceFile] -> [SourceFile]
intersectWithSourceFiles allSources files = filter (\x -> dropExtension x `S.member` extLess allSources) files
  where extLess = S.fromList . map (dropExtension)

-- | Dependency tracking of files can return 
-- dependencies of the form ¨{sources}¨ of ¨{main}¨.
-- These refer to the locally present sources or main module
-- file of an executable. This will replace that values
-- by the actual values. Parameters are as follows:
-- All source files, main module path if existing,
-- the file name of the current file
-- and the current dependency to expand
exPandFileNames :: [SourceFile] -> Maybe String -> FilePath -> String -> [FilePath]
exPandFileNames sfs mPath f fn = case sf of
    "{sources}" -> map (-<.> ext) sfs
    "{main}" -> mModule
    "{x}" -> [f -<.> ext]
    x -> [fn]
  where (sf,ext) = splitExtension fn
        mModule = maybe [] (\x -> [x -<.> ext]) mPath

---- | This generate the name to be concatenated to
---- the name of an executable based on the target.
---- It will generate ¨-backend-flavour¨. If the backend
---- is ¨bytecode¨ or the flavour ¨plain¨ this will not be shown.
--flavourTargetExt :: String -> String
--flavourTargetExt s = (if tb then [] else '-':y) ++ (if fb then [] else '-':x)
--  where (x:y:xs) = words s
--        fb = x == "plain"
--        tb = y == "bytecode"

--getPackages :: FileFormat -> [String]
--getPackages ff = S.toList $ S.fromList $ sourcePackages ++ libPackages ++ exePackages ++ toolPackages
--  where sourcePackages = ff ^. pkgSourceInfo ^. packages
--        libPackages = case ff ^. library of
--          Nothing -> []
--          Just l -> l ^. libSourceInfo ^. packages
--        exePackages = concatMap (\x -> x ^. exeSourceInfo ^. packages) $ ff ^. executables
--        toolPackages = concatMap (\x -> x ^. exeSourceInfo ^. packages) $ ff ^. tools


-- | Using @execTool@ with
-- the compiler stored in @BuildInfo@.
-- It also sets the @hideAllPackagesFlag@
-- and adds the packages used as flags.
execComp :: BuildInfo -> [String] -> ToolMonad a -> Action String
execComp bi pkgs t = execTool bi comp $ do
  mapM (addPkgFlag) pkgs
  hf <- getHidePkgsFlag
  addFlag hf
  t
  where comp = bi ^. compiler
        addPkgFlag s = do
          f <- getPackageFlag s
          addFlag f

-- | Runs a @ToolMonad@ computation
-- using the supplied @PreProcessor@ as the 
-- current @AnyTool@. It will already
-- set the @outputDirectory@ and
-- adds the same directory as an input directory
execPreprocessor :: PreProcessor p => BuildInfo -> p -> ToolMonad a -> Action String
execPreprocessor bi p = execTool bi (KnownPreProcessor p)

execTool :: StateActions p => BuildInfo -> p -> ToolMonad a -> Action String
execTool bi p m =
  traced "run ToolMonad" $ runError (bi ^. verbosity) $ do
    load p
    makeConfiguration
    setOutputDir $ bi ^. outputDirectory
    addInputDir $ bi ^. outputDirectory
    m
    invoke

-- | Runs a @ToolMonad@ computation and lifts
-- it to an @Action@. It uses @runError@ to run
-- the @ToolMonad@
runMonad :: Verbosity -> ToolMonad a -> Action a
runMonad v = liftIO . runError v 

-- | Produce the build rules for a single
-- @PreProcessor@. 
preprocessRules :: BuildInfo -> PreprocessFile -> Rules ()
preprocessRules bi (s, xs) = mapM_ (preprocessSingleRules bi p) xs
  where p = findPreProcessorOrNewPlugin s

-- | Produces the build rules for a single @PreProcess@.
-- It assumes the source is present inside the @outputDirectory@
preprocessSingleRules :: BuildInfo -> KnownPreProcessor -> PreProcess -> Rules ()
preprocessSingleRules bi p (s, t, flags) = 
  target *> \out -> do
    need [source]
    execPreprocessor bi p $ do
      addInputFile source
      setOutputFile $ target
      mapM_ addFlag $ S.toList flags
    return ()
  where odir = bi ^. outputDirectory
        target = odir </> t
        source = odir </> s

--getLibName :: FileFormat -> Maybe String
--getLibName ff = case ff ^. library of
--  Just l -> Just $ l ^. libName
--  Nothing -> Nothing

-- | Retrieve all the @PreprocessFile@s from the
-- @FileFormat@
getPreprocessSources :: FileFormat -> [PreprocessFile]
getPreprocessSources ff = getLibPreprocessSources ff ++ getExePreprocessSources ff ++ ff ^. pkgSourceInfo ^. preprocessSources

-- | Retrieve all the @PreprocessFile@s from the
-- @Library@
getLibPreprocessSources :: FileFormat -> [PreprocessFile]
getLibPreprocessSources ff = case ff ^. library of
    Nothing -> []
    Just l -> l ^. libSourceInfo ^. preprocessSources

-- | Retrieve all the @PreprocessFile@s from the
-- @executables@ and @tools@
getExePreprocessSources :: FileFormat -> [PreprocessFile]
getExePreprocessSources ff = concatMap (\x -> x ^. exeSourceInfo ^. preprocessSources) $ ff ^. executables ++ ff ^. tools


--getHsNonMainSourceFiles :: FileFormat -> String -> [String]
--getHsNonMainSourceFiles ff f = getSourceFiles $ ff ^. executables
--  where getSourceFiles [] = error "Executable has no main file specified!"
--        getSourceFiles (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. exeSourceInfo ^. sourceFiles else getSourceFiles xs

--getHsExeSourceFiles :: FileFormat -> String -> [String]
--getHsExeSourceFiles ff f = getHsMainSourceFile ff f : (getHsNonMainSourceFiles ff f)

--getHsLibSourceFiles :: FileFormat -> [String]
--getHsLibSourceFiles ff = case ff ^. library of
--    Nothing -> []
--    Just l -> l ^. libSourceInfo ^. sourceFiles

--getAllHsNonMainSourceFiles :: FileFormat -> [String]
--getAllHsNonMainSourceFiles ff = concatMap (\x -> x ^. exeSourceInfo ^. sourceFiles) (ff ^. executables) ++ getHsLibSourceFiles ff ++ getAllToolNonMainSources ff

--getAllToolNonMainSources :: FileFormat -> [String]
--getAllToolNonMainSources ff = concatMap (\x -> x ^. exeSourceInfo ^. sourceFiles) (ff ^. tools)

--getHsMainSourceFile :: FileFormat -> String -> String
--getHsMainSourceFile ff f = getMainFile $ ff ^. executables
--  where getMainFile [] = error "Executable has no main file specified!"
--        getMainFile (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. mainModulePath else getMainFile xs


--getAllHsMainSourceFiles :: FileFormat -> [String]
--getAllHsMainSourceFiles ff = map (^. mainModulePath) (ff ^. executables) ++ (map (^. mainModulePath) $ ff ^. tools)

--getHsMainToolSourceFile :: FileFormat -> String -> String
--getHsMainToolSourceFile ff f = getMainFile $ ff ^. tools
--  where getMainFile [] = error "Tool has no main file specified!"
--        getMainFile (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. mainModulePath else getMainFile xs

--getHsNonMainToolSourceFiles :: FileFormat -> String -> [String]
--getHsNonMainToolSourceFiles ff f = getSourceFiles $ ff ^. tools
--  where getSourceFiles [] = error "Tool has no main file specified!"
--        getSourceFiles (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. exeSourceInfo ^. sourceFiles else getSourceFiles xs

--getAllToolNames :: FileFormat -> [String]
--getAllToolNames ff = getAllToolNames' $ ff ^. tools
--  where getAllToolNames' [] = []
--        getAllToolNames' (x:xs) = x ^. exeName : getAllToolNames' xs
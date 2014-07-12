module Tools.Cabal.Shake where

import Development.Shake hiding (Verbosity)
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Lens ((^.))

import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)

import Cabal
import Tools.Cabal.FileFormat
import Tools.Cabal.BuildInfo
import Tools.BasicTools.Shared
import Tools.Core.Types hiding (addInputDir, setOutputFile, setOutputDir, prepareTransForm)
import Tools.Core.ToolMonad
import Tools.Flag

import Debug

getAllExeNames :: FileFormat -> [String]
getAllExeNames ff = getAllExeNames' $ ff ^. executables
  where getAllExeNames' [] = []
        getAllExeNames' (x:xs) = x ^. exeName : getAllExeNames' xs

mainShake :: FileFormat -> BuildInfo -> IO ()
mainShake ff bi = shakeArgs shakeOptions{shakeFiles=odir ++ "/"} $ do
  want [odir ++ "/fake.txt"]

  phony "clean" $ do
    removeFilesAfter odir ["//*"]

  phony (odir ++ "/fake.txt") $ do
    need $ [odir </> c -<.> exe | c <- tools]
    need $ [odir </> c -<.> exe | c <- exes]

  map (\c -> odir ++ "/" ++ c <.> exe) exes |*> \out -> do
    let os = [odir </> c -<.> "o" | c <- getHsMainSourceFile ff out : (getHsNonMainSourceFiles ff out)]
    need os
    execComp bi pkgs $ do
      setOutputFile out
      mapM setInputFile os
    return ()

  map (\c -> odir ++ "/" ++ c <.> exe) tools |*> \out -> do
    let os = [odir </> c -<.> "o" | c <- getHsMainToolSourceFile ff out : (getHsNonMainToolSourceFiles ff out)]
    need os
    execComp bi pkgs $ do
      setOutputFile out
      mapM setInputFile os
    return ()

  map (\c -> odir ++ "/" ++ c -<.> "o") (getAllHsNonMainSourceFiles ff) |*> \out -> do
    let c = out -<.> "hs"
    need [c]
    execComp bi pkgs $ do
      setInputFile c
      addFlag $ newShortFlag "c"
    return ()

  map (\c -> odir ++ "/" ++ c -<.> "o") (getAllHsMainSourceFiles ff) |*> \out -> do
    let c = out -<.> "hs"
    need [c]
    execComp bi pkgs $ do
      setInputFile c
      setOutputFile out
      addFlag $ newShortFlag "c"
    return ()

  sequence_ $ map (preprocessRules bi) $ getPreprocessSources ff 

  -- Any needed file for which there is no explicit rule
  -- is just copied to the build directory
  -- this allows the other rules to just assume the file exists in
  -- the build directory regardless if it was generated or not.
  (odir ++ "//*") *> \out -> do
    let source = dropDirectory1 out
    need [source]
    copyFile' source out
  where exes = ff ^. executables
        tls = ff ^. tools
        lib = ff ^. library
        comp = bi ^. compiler
        odir = bi ^. outputDirectory
        pkgs = getPackages ff

getPackages :: FileFormat -> [String]
getPackages ff = S.toList $ S.fromList $ sourcePackages ++ libPackages ++ exePackages ++ toolPackages
  where sourcePackages = ff ^. pkgSourceInfo ^. packages
        libPackages = case ff ^. library of
          Nothing -> []
          Just l -> l ^. libSourceInfo ^. packages
        exePackages = concatMap (\x -> x ^. exeSourceInfo ^. packages) $ ff ^. executables
        toolPackages = concatMap (\x -> x ^. exeSourceInfo ^. packages) $ ff ^. tools


-- buildinfo + list of used packages
execComp :: BuildInfo -> [String] -> ToolMonad a -> Action String
execComp bi pkgs t = execTool bi comp $ do
  hf <- hidePkgsFlag
  addFlag hf
  mapM (addPkgFlag) pkgs
  t
  where comp = bi ^. compiler
        hidePkgsFlag = hideAllPackagesFlag comp
        addPkgFlag s = do
          f <- packageFlag comp s
          addFlag f


execTool ::PreProcessor p => BuildInfo -> p -> ToolMonad a -> Action String
execTool bi p m = do
  traced "run ToolMonad" $ runError (bi ^. verbosity) $ do
    load $ KnownPreProcessor p
    makeConfiguration
    setOutputDir $ bi ^. outputDirectory
    addInputDir $ bi ^. outputDirectory
    m
    invoke

runMonad :: Verbosity -> ToolMonad a -> Action a
runMonad v = liftIO . runError v 

preprocessRules :: BuildInfo -> PreprocessFile -> Rules ()
preprocessRules bi (s, xs) = mapM_ (preprocessSingleRules bi p) xs
  where p = findPreProcessorOrNewPlugin s

preprocessSingleRules :: BuildInfo -> KnownPreProcessor -> PreProcess -> Rules ()
preprocessSingleRules bi p (s, t, flags) = 
  (target) *> \out -> do
    need [source]
    execTool bi p $ do
      setInputFile source
      setOutputFile $ target
      sequence_ $ map addFlag $ S.toList flags
    return ()
  where odir = bi ^. outputDirectory
        target = odir </> t
        source = odir </> s

getLibName :: FileFormat -> Maybe String
getLibName ff = case ff ^. library of
  Just l -> Just $ l ^. libName
  Nothing -> Nothing

getPreprocessSources :: FileFormat -> [PreprocessFile]
getPreprocessSources ff = getLibPreprocessSources ff ++ getExePreprocessSources ff

getLibPreprocessSources :: FileFormat -> [PreprocessFile]
getLibPreprocessSources ff = case ff ^. library of
    Nothing -> []
    Just l -> l ^. libSourceInfo ^. preprocessSources

getExePreprocessSources :: FileFormat -> [PreprocessFile]
getExePreprocessSources ff = concatMap (\x -> x ^. exeSourceInfo ^. preprocessSources) $ ff ^. executables


getHsNonMainSourceFiles :: FileFormat -> String -> [String]
getHsNonMainSourceFiles ff f = getSourceFiles $ ff ^. executables
  where getSourceFiles [] = error "Executable has no main file specified!"
        getSourceFiles (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. exeSourceInfo ^. sourceFiles else getSourceFiles xs

getHsExeSourceFiles :: FileFormat -> String -> [String]
getHsExeSourceFiles ff f = getHsMainSourceFile ff f : (getHsNonMainSourceFiles ff f)

getHsLibSourceFiles :: FileFormat -> [String]
getHsLibSourceFiles ff = case ff ^. library of
    Nothing -> []
    Just l -> l ^. libSourceInfo ^. sourceFiles

getAllHsNonMainSourceFiles :: FileFormat -> [String]
getAllHsNonMainSourceFiles ff = concatMap (\x -> x ^. exeSourceInfo ^. sourceFiles) (ff ^. executables) ++ getHsLibSourceFiles ff ++ getAllToolNonMainSources ff

getAllToolNonMainSources :: FileFormat -> [String]
getAllToolNonMainSources ff = concatMap (\x -> x ^. exeSourceInfo ^. sourceFiles) (ff ^. tools)

getHsMainSourceFile :: FileFormat -> String -> String
getHsMainSourceFile ff f = getMainFile $ ff ^. executables
  where getMainFile [] = error "Executable has no main file specified!"
        getMainFile (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. mainModulePath else getMainFile xs


getAllHsMainSourceFiles :: FileFormat -> [String]
getAllHsMainSourceFiles ff = map (^. mainModulePath) (ff ^. executables) ++ (map (^. mainModulePath) $ ff ^. tools)

getHsMainToolSourceFile :: FileFormat -> String -> String
getHsMainToolSourceFile ff f = getMainFile $ ff ^. tools
  where getMainFile [] = error "Tool has no main file specified!"
        getMainFile (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. mainModulePath else getMainFile xs

getHsNonMainToolSourceFiles :: FileFormat -> String -> [String]
getHsNonMainToolSourceFiles ff f = getSourceFiles $ ff ^. tools
  where getSourceFiles [] = error "Tool has no main file specified!"
        getSourceFiles (x:xs) = if x ^. exeName == dropDirectory1 f then x ^. exeSourceInfo ^. sourceFiles else getSourceFiles xs

getAllToolNames :: FileFormat -> [String]
getAllToolNames ff = getAllToolNames' $ ff ^. tools
  where getAllToolNames' [] = []
        getAllToolNames' (x:xs) = x ^. exeName : getAllToolNames' xs
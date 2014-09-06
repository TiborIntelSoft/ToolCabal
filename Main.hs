{-# LANGUAGE TemplateHaskell #-}
module Main where

import Text.ParserCombinators.UU.Utils
import Development.Shake.FilePath ((</>))
import System.Environment (getArgs, withArgs)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe
import Data.List
import Control.Lens

import Tools.Core.Types
import Tools.Core.Utils
import Tools.BasicTools.Types
import Tools.BasicTools.Shared
import Tools.Cabal.BuildInfo

import Tools.Cabal.Shake
import Tools.Cabal.FileFormatParser
import Tools.Flag

import Cabal

import Debug

data FlagInfo = FlagInfo
  {
    _doBuild :: Bool,
    _shakeArgs :: [String],
    _doInstall :: Bool,
    _showVersion :: Bool,
    _showHelp :: Bool,
    _doClean :: Bool
  }
makeLenses ''FlagInfo

emptyFlagInfo = FlagInfo True [] False False False False

main :: IO ()
main = do
  src <- readFile "pkg.toolpkg"
  ff <- return $ runParser "Parse FileFormat" pFileFormat src
  args <- getArgs
  (bi, shakeArgs) <- parseArgs args
  withArgs shakeArgs $ mainShake ff bi

parseArgs :: [String] -> IO (BuildInfo, [String])
parseArgs args = do
  bi <- return $ getBasicBuildInfo GHC
  (bi2, fi) <- return $ f bi
  bi3 <- addBasicInfo bi2
  return (bi3, sArg fi)
  --return $ g bi
  where flags = runParser "Parse arguments" pFlags $ intercalate " " args
        f bi = foldr addFlag (bi, emptyFlagInfo) flags
        sArg fi = fi ^. shakeArgs ++ extraShake fi
        extraShake fi = b ++ i ++ c
          where b = if fi ^. doBuild && (not $ fi ^.doClean) then ["build"] else []
                i = if fi ^. doInstall && (not $ fi ^.doClean) then ["install"] else []
                c = if fi ^. doClean then ["clean"] else []



addFlag :: Flag -> (BuildInfo, FlagInfo) -> (BuildInfo, FlagInfo)
addFlag f (bi, fi) =
  if s == "v" || l == "version" then
    (bi, fi & showVersion .~ True)
  else if s == "h" || l == "help" then
    (bi, fi & showHelp .~ True)
  else if s == "c" || l == "compiler" then
    let c = findCompilerOrNewPlugin $ fromJust $ f ^. defaultArgument
    in (bi & compiler .~ c, fi)
  else if "plugin" `isPrefixOf` l then
    undefined
  else if "path" `isPrefixOf` l then
    undefined
  else if s == "o" || l == "outputDir" then
    undefined
  else if s == "V" || l == "verbosity" then
    undefined
  else if s == "cf" || l == "compiler-flavour" then
    (bi & flavours %~ (fromJust (f ^. defaultArgument) :), fi)
  else if s == "ct" || l == "compiler-target" then
    (bi & targets %~ (fromJust (f ^. defaultArgument) :), fi)
  else if s == "tf" || l == "tool-flavour" then
    undefined
  else if s == "tt" || l == "tool-target" then
    undefined
  else if l == "install-packageDB" then
    undefined
  else if l == "use-packageDB" then
    undefined
  else if l == "installTools" then
    undefined
  else if l == "installTool" then
    undefined
  else if l == "doNotInstallTool" then
    undefined
  else if l == "doNotInstallLibrary" then
    undefined
  else if l == "doNotInstallExecutables" then
    undefined
  else if l == "installExecutable" then
    undefined
  else if l == "doNotInstallExecutable" then
    undefined
  else if s == "s" || l == "shake" then
    (bi, fi & shakeArgs %~ (fromJust (f ^. defaultArgument) :))
  else if s == "b" || l == "build" then
    (bi, fi & doBuild .~ True)
  else if s == "i" || l == "install" then
    (bi, fi & doInstall .~ True)
  else if s == "clean" || l == "clean" then
    (bi, fi & doClean .~ True)
  else error $ "Unknown flag " ++ show f ++ "given"
  where s = f ^. shortName 
        l = f ^. longName
        arg = f ^. defaultArgument
        a = fromJust arg
        n = isNothing arg 

getBasicBuildInfo :: Compiler c => c -> BuildInfo
getBasicBuildInfo c = BuildInfo "_build" (KnownCompiler c) M.empty
  normal [] [] [] [] M.empty M.empty M.empty "" M.empty M.empty
  False M.empty True M.empty True UserPackageDb [] M.empty M.empty
  M.empty M.empty False

addBasicInfo :: BuildInfo -> IO BuildInfo
addBasicInfo bi = do
  t <- runError v $ supportedTargets pm p
  tfs <- mapM (runError v . targetTypes pm p) t
  tfsMap <- return $ M.fromList $ zip t tfs
  installLibDirs <- mapM (\x -> runError v $
      getLibInstallDir pm p x UserPackageDb
    ) t
  installLibDirMap <- return $ M.fromList $ zip t installLibDirs
  installExeDirs <- mapM (\x -> runError v $
      getExeInstallDir pm p x UserPackageDb
    ) t
  installExeDirMap <- return $ M.fromList $ zip t installExeDirs
  installSourceStrings <- mapM (\x -> runError v $
      getLibInstallExtensions pm p x
    ) t
  installSourceMap <- return $ M.fromList $ zip t installSourceStrings
  installIncludeDirectory <- runError v $ getIncludeInstallDir pm p UserPackageDb
  exeNameMap <- return $ f "exe" tfsMap
  libNameMap <- return $ f "lib" tfsMap
  return $ bi 
    & compilerTargetTypes .~ tfsMap
    & targets %~ (ae "bytecode") 
    & flavours %~ (ae "plain")
    & toolTargets %~ (ae "bytecode") 
    & toolFlavours %~ (ae "plain")
    & exeNames .~ exeNameMap
    & libNames .~ libNameMap
    & installLibDir .~ installLibDirMap
    & installIncludeDir .~ installIncludeDirectory
    & installExeDir .~ installExeDirMap
    & installSources .~ installSourceMap 
  where ae s xs = if xs == [] then [s] else xs
        pm = bi ^. compiler
        p = getProgram pm
        v = normal
        f :: String -> Map Target [(String, FileType)] -> Map Target [String]
        f s = M.map g
          where g :: [(String, FilePath)] -> [String]
                g = map fst . filter (\x -> snd x == s)
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Cabal.Utils where

import Cabal.Verbosity
import Cabal.Exception
         ( tryIO, catchIO, catchExit )
import Control.Monad
    ( join, when, unless, filterM )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Data.List
  ( nub, unfoldr, isPrefixOf, tails, intercalate )
import Data.Char as Char
    ( toLower, chr, ord )
import Data.Bits
    ( Bits((.|.), (.&.), shiftL, shiftR) )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

import System.Directory
    ( Permissions(executable), getDirectoryContents, getPermissions
    , doesDirectoryExist, doesFileExist, removeFile, findExecutable
    , getModificationTime )
import System.Environment
    ( getProgName )
import System.Exit
    ( exitWith, ExitCode(..) )
import Development.Shake.FilePath
    ( normalise, (</>), (<.>)
    , getSearchPath, takeDirectory, splitFileName
    , splitExtension, splitExtensions, splitDirectories )
import System.Directory
    ( createDirectory, renameFile, removeDirectoryRecursive )
import System.IO
    ( Handle, openFile, openBinaryFile, openBinaryTempFile
    , IOMode(ReadMode), hSetBinaryMode
    , hGetContents, stdin, stderr, stdout, hPutStr, hFlush, hClose )
import System.IO.Error as IO.Error
    ( isDoesNotExistError, isAlreadyExistsError
    , ioeSetFileName, ioeGetFileName, ioeGetErrorString )
import System.IO.Error
    ( ioeSetLocation, ioeGetLocation )
import System.IO.Unsafe
    ( unsafeInterleaveIO )
import qualified Control.Exception as Exception

import Cabal.Text
    ( display, simpleParse )
import Cabal.Version
    (Version(..))

import Control.Exception (IOException, evaluate, throwIO)
import System.Process (rawSystem)
import qualified System.Process as Process (CreateProcess(..))

import Control.Concurrent (forkIO)
import System.Process (runInteractiveProcess, waitForProcess, proc,
                       StdStream(..))

import System.Process (showCommandForUser)

import System.Process (createProcess)


import Debug
-------------------
-- File permissions

-- | Like 'doesFileExist', but also checks that the file is executable.
doesExecutableExist :: FilePath -> IO Bool
doesExecutableExist f = do
  exists <- doesFileExist f
  if exists
    then do perms <- getPermissions f
            return (executable perms)
    else return False

wrapText :: String -> String
wrapText = unlines
         . map (intercalate "\n"
              . map unwords
              . wrapLine 79
              . words)
         . lines

-- | Wraps a list of words to a list of lines of words of a particular width.
wrapLine :: Int -> [String] -> [[String]]
wrapLine width = wrap 0 []
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

-- Exit with the same exitcode if the subcommand fails
rawSystemExit :: Verbosity -> FilePath -> [String] -> IO ()
rawSystemExit verbosity path args = do
  printRawCommandAndArgs verbosity path args
  hFlush stdout
  exitcode <- rawSystem path args
  unless (exitcode == ExitSuccess) $ do
    debug verbosity $ path ++ " returned " ++ show exitcode
    exitWith exitcode

-- Closes the passed in handles before returning.
rawSystemIOWithEnv :: Verbosity
                   -> FilePath
                   -> [String]
                   -> Maybe FilePath           -- ^ New working dir or inherit
                   -> Maybe [(String, String)] -- ^ New environment or inherit
                   -> Maybe Handle  -- ^ stdin
                   -> Maybe Handle  -- ^ stdout
                   -> Maybe Handle  -- ^ stderr
                   -> IO ExitCode
rawSystemIOWithEnv verbosity path args mcwd menv inp out err = do
    maybe (printRawCommandAndArgs       verbosity path args)
          (printRawCommandAndArgsAndEnv verbosity path args) menv
    hFlush stdout
    exitcode <- syncProcess "rawSystemIOWithEnv" (proc path args)
                { Process.cwd     = mcwd
                , Process.env     = menv
                , Process.std_in  = mbToStd inp
                , Process.std_out = mbToStd out
                , Process.std_err = mbToStd err }
                `Exception.finally` (mapM_ maybeClose [inp, out, err])
    unless (exitcode == ExitSuccess) $ do
      debug verbosity $ path ++ " returned " ++ show exitcode
    return exitcode
  where
  -- Also taken from System.Process
  maybeClose :: Maybe Handle -> IO ()
  maybeClose (Just  hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeClose _ = return ()

  mbToStd :: Maybe Handle -> StdStream
  mbToStd Nothing    = Inherit
  mbToStd (Just hdl) = UseHandle hdl

rawSystemStdout :: Verbosity -> FilePath -> [String] -> IO String
rawSystemStdout verbosity path args = do
  (output, errors, exitCode) <- rawSystemStdInOut verbosity path args
                                                  Nothing Nothing
                                                  Nothing False
  when (exitCode /= ExitSuccess) $
    die errors
  return output

-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old osx
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

printRawCommandAndArgs :: Verbosity -> FilePath -> [String] -> IO ()
printRawCommandAndArgs verbosity path args
 | verbosity >= deafening = print (path, args)
 | verbosity >= verbose   =
                            putStrLn $ showCommandForUser path args

 | otherwise              = return ()

rawSystemStdInOut :: Verbosity
                  -> FilePath                 -- ^ Program location
                  -> [String]                 -- ^ Arguments
                  -> Maybe FilePath           -- ^ New working dir or inherit
                  -> Maybe [(String, String)] -- ^ New environment or inherit
                  -> Maybe (String, Bool)     -- ^ input text and binary mode
                  -> Bool                     -- ^ output in binary mode
                  -> IO (String, String, ExitCode) -- ^ output, errors, exit
rawSystemStdInOut verbosity path args mcwd menv input outputBinary = do
  printRawCommandAndArgs verbosity path args

  Exception.bracket
     (runInteractiveProcess path args mcwd menv)
     (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
    $ \(inh,outh,errh,pid) -> do

      -- output mode depends on what the caller wants
      hSetBinaryMode outh outputBinary
      -- but the errors are always assumed to be text (in the current locale)
      hSetBinaryMode errh False

      -- fork off a couple threads to pull on the stderr and stdout
      -- so if the process writes to stderr we do not block.

      err <- hGetContents errh
      out <- hGetContents outh

      mv <- newEmptyMVar
      let force str = (evaluate (length str) >> return ())
            `Exception.finally` putMVar mv ()
          --TODO: handle exceptions like text decoding.
      _ <- forkIO $ force out
      _ <- forkIO $ force err

      -- push all the input, if any
      case input of
        Nothing -> return ()
        Just (inputStr, inputBinary) -> do
                -- input mode depends on what the caller wants
          hSetBinaryMode inh inputBinary
          hPutStr inh inputStr
          hClose inh
          --TODO: this probably fails if the process refuses to consume
          -- or if it closes stdin (eg if it exits)

      -- wait for both to finish, in either order
      takeMVar mv
      takeMVar mv

      -- wait for the program to terminate
      exitcode <- waitForProcess pid
      unless (exitcode == ExitSuccess) $
        debug verbosity $ path ++ " returned " ++ show exitcode
                       ++ if null err then "" else
                          " with error message:\n" ++ err
                       ++ case input of
                            Nothing       -> ""
                            Just ("",  _) -> ""
                            Just (inp, _) -> "\nstdin input:\n" ++ inp

      return $ (out, err, exitcode)

printRawCommandAndArgsAndEnv :: Verbosity
                             -> FilePath
                             -> [String]
                             -> [(String, String)]
                             -> IO ()
printRawCommandAndArgsAndEnv verbosity path args env
 | verbosity >= deafening = do putStrLn ("Environment: " ++ show env)
                               print (path, args)
 | verbosity >= verbose   = putStrLn $ unwords (path : args)
 | otherwise              = return ()

-- This is taken directly from the process package.
-- The reason we need it is that runProcess doesn't handle ^C in the same
-- way that rawSystem handles it, but rawSystem doesn't allow us to pass
-- an environment.
syncProcess :: String -> Process.CreateProcess -> IO ExitCode
syncProcess _fun c = do
  (_,_,_,p) <- createProcess c
  waitForProcess p

die :: String -> IO a
die msg = ioError (userError msg)

fromUTF8 :: String -> String
fromUTF8 []     = []
fromUTF8 (c:cs)
  | c <= '\x7F' = c : fromUTF8 cs
  | c <= '\xBF' = replacementChar : fromUTF8 cs
  | c <= '\xDF' = twoBytes c cs
  | c <= '\xEF' = moreBytes 3 0x800     cs (ord c .&. 0xF)
  | c <= '\xF7' = moreBytes 4 0x10000   cs (ord c .&. 0x7)
  | c <= '\xFB' = moreBytes 5 0x200000  cs (ord c .&. 0x3)
  | c <= '\xFD' = moreBytes 6 0x4000000 cs (ord c .&. 0x1)
  | otherwise   = replacementChar : fromUTF8 cs
  where
    twoBytes c0 (c1:cs')
      | ord c1 .&. 0xC0 == 0x80
      = let d = ((ord c0 .&. 0x1F) `shiftL` 6)
             .|. (ord c1 .&. 0x3F)
         in if d >= 0x80
               then  chr d           : fromUTF8 cs'
               else  replacementChar : fromUTF8 cs'
    twoBytes _ cs' = replacementChar : fromUTF8 cs'

    moreBytes :: Int -> Int -> [Char] -> Int -> [Char]
    moreBytes 1 overlong cs' acc
      | overlong <= acc && acc <= 0x10FFFF
     && (acc < 0xD800 || 0xDFFF < acc)
     && (acc < 0xFFFE || 0xFFFF < acc)
      = chr acc : fromUTF8 cs'

      | otherwise
      = replacementChar : fromUTF8 cs'

    moreBytes byteCount overlong (cn:cs') acc
      | ord cn .&. 0xC0 == 0x80
      = moreBytes (byteCount-1) overlong cs'
          ((acc `shiftL` 6) .|. ord cn .&. 0x3F)

    moreBytes _ _ cs' _
      = replacementChar : fromUTF8 cs'

    replacementChar = '\xfffd'

toUTF8 :: String -> String
toUTF8 []        = []
toUTF8 (c:cs)
  | c <= '\x07F' = c
                 : toUTF8 cs
  | c <= '\x7FF' = chr (0xC0 .|. (w `shiftR` 6))
                 : chr (0x80 .|. (w .&. 0x3F))
                 : toUTF8 cs
  | c <= '\xFFFF'= chr (0xE0 .|.  (w `shiftR` 12))
                 : chr (0x80 .|. ((w `shiftR` 6)  .&. 0x3F))
                 : chr (0x80 .|.  (w .&. 0x3F))
                 : toUTF8 cs
  | otherwise    = chr (0xf0 .|.  (w `shiftR` 18))
                 : chr (0x80 .|. ((w `shiftR` 12)  .&. 0x3F))
                 : chr (0x80 .|. ((w `shiftR` 6)  .&. 0x3F))
                 : chr (0x80 .|.  (w .&. 0x3F))
                 : toUTF8 cs
  where w = ord c

-- | Look for a program on the path.
findProgramLocation :: Verbosity -> FilePath -> IO (Maybe FilePath)
findProgramLocation verbosity prog = do
  debug verbosity $ "searching for " ++ prog ++ " in path."
  res <- findExecutable prog
  case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just path -> debug verbosity ("found " ++ prog ++ " at "++ path)
  return res


-- | Look for a program and try to find it's version number. It can accept
-- either an absolute path or the name of a program binary, in which case we
-- will look for the program on the path.
--
findProgramVersion :: String             -- ^ version args
                   -> (String -> String) -- ^ function to select version
                                         --   number from program output
                   -> Verbosity
                   -> FilePath           -- ^ location
                   -> IO (Maybe Version)
findProgramVersion versionArg selectVersion verbosity path = do
  str <- rawSystemStdout verbosity path [versionArg]
         `catchIO`   (\_ -> return "")
         `catchExit` (\_ -> return "")
  let version :: Maybe Version
      version = simpleParse (selectVersion str)
  case version of
      Nothing -> warn verbosity $ "cannot determine version of " ++ path
                               ++ " :\n" ++ show str
      Just v  -> debug verbosity $ path ++ " is version " ++ display v
  return version

-- | Non fatal conditions that may be indicative of an error or problem.
--
-- We display these at the 'normal' verbosity level.
--
warn :: Verbosity -> String -> IO ()
warn verbosity msg =
  when (verbosity >= normal) $ do
    hFlush stdout
    hPutStr stderr (wrapText ("Warning: " ++ msg))

-- | Detailed internal debugging information
--
-- We display these messages when the verbosity level is 'deafening'
--
debug :: Verbosity -> String -> IO ()
debug verbosity msg =
  when (verbosity >= deafening) $ do
    putStr (wrapText msg)
    hFlush stdout

-- | Useful status messages.
--
-- We display these at the 'normal' verbosity level.
--
-- This is for the ordinary helpful status messages that users see. Just
-- enough information to know that things are working but not floods of detail.
--
notice :: Verbosity -> String -> IO ()
notice verbosity msg =
  when (verbosity >= normal) $
    putStr (wrapText msg)

-- | More detail on the operation of some action.
--
-- We display these messages when the verbosity level is 'verbose'
--
info :: Verbosity -> String -> IO ()
info verbosity msg =
  when (verbosity >= verbose) $
    putStr (wrapText msg)

printMessage :: Verbosity -> Verbosity -> String -> IO()
printMessage atVerbosity v msg = when (v >= atVerbosity) $
    putStr (wrapText msg)
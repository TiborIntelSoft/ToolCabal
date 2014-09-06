-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program
-- Copyright   :  Isaac Jones 2006, Duncan Coutts 2007-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This provides an abstraction which deals with configuring and running
-- programs. A 'Program' is a static notion of a known program. A
-- 'ConfiguredProgram' is a 'Program' that has been found on the current
-- machine and is ready to be run (possibly with some user-supplied default
-- args). Configuring a program involves finding its location and if necessary
-- finding its version. There is also a 'ProgramConfiguration' type which holds
-- configured and not-yet configured programs. It is the parameter to lots of
-- actions elsewhere in Cabal that need to look up and run programs. If we had
-- a Cabal monad, the 'ProgramConfiguration' would probably be a reader or
-- state component of it. 
--
-- The module also defines all the known built-in 'Program's and the
-- 'defaultProgramConfiguration' which contains them all.
--
-- One nice thing about using it is that any program that is
-- registered with Cabal will get some \"configure\" and \".cabal\"
-- helpers like --with-foo-args --foo-path= and extra-foo-args.
--
-- There's also good default behavior for trying to find \"foo\" in
-- PATH, being able to override its location, etc.
--
-- There's also a hook for adding programs in a Setup.lhs script.  See
-- hookedPrograms in 'Distribution.Simple.UserHooks'.  This gives a
-- hook user the ability to get the above flags and such so that they
-- don't have to write all the PATH logic inside Setup.lhs.

module Cabal.Program where

import Cabal.Types
import Cabal.Run
--import Distribution.Simple.Program.Db
--import Distribution.Simple.Program.Builtin

import Cabal.Utils
         ( die, findProgramLocation, findProgramVersion )
import Cabal.Verbosity
         ( Verbosity )

import Debug.Trace


-- | Runs the given configured program.
--
runProgram :: Verbosity            -- ^Verbosity
           -> ConfiguredProgram  -- ^The program to run
           -> [ProgArg]            -- ^Any /extra/ arguments to add
           -> IO ()
runProgram verbosity prog args =
  runProgramInvocation verbosity (programInvocation prog args)


-- | Runs the given configured program and gets the output.
--
getProgramOutput :: Verbosity           -- ^Verbosity
                 -> ConfiguredProgram -- ^The program to run
                 -> [ProgArg]           -- ^Any /extra/ arguments to add
                 -> IO String
getProgramOutput verbosity prog args = 
  getProgramInvocationOutput verbosity (programInvocation prog args)


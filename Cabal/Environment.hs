{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Cabal.Environment (getEnvironment)
       where

import qualified System.Environment as System

getEnvironment :: IO [(String, String)]

getEnvironment = System.getEnvironment

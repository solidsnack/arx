#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString
import System.Environment
import System.Exit
import System.IO
import System.Process

import Data.FileEmbed

import qualified System.Posix.ARX.CLI


main                        ::  IO ()
main                         =  do
  flags                     <-  (,) <$> checkHelp <*> checkVersion
  case flags of (True, _)   ->  Data.ByteString.putStr usage >> exitSuccess
                (_, True)   ->  Data.ByteString.putStr version >> exitSuccess
                (_,    _)   ->  System.Posix.ARX.CLI.main


checkHelp                    =  checkOne ["-h", "-?", "--help"]
checkVersion                 =  checkOne ["-v", "--version"]
checkOne list                =  any (`elem` list) . take 1 <$> getArgs

usage                        =  $(embedFile "./docs/blessed/arx.txt")
version                      =  $(embedFile "./version")


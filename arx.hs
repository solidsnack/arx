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
  helpFlag                  <-  checkHelp
  if helpFlag then sendHelp
              else System.Posix.ARX.CLI.main


checkHelp = any (`elem` ["-h", "-?", "--help"]) <$> getArgs


txt                          =  $(embedFile "./docs/blessed/arx.txt")

sendHelp                     =  do Data.ByteString.putStr txt
                                   exitSuccess


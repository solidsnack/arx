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
import System.Unix.Directory

import qualified System.Posix.ARX.CLI


main                        ::  IO ()
main                         =  do
  helpFlag                  <-  checkHelp
  if helpFlag then sendHelp
              else System.Posix.ARX.CLI.main


checkHelp = any (`elem` ["-h", "-?", "--help"]) <$> getArgs


man                          =  $(embedFile "./docs/blessed/arx.man")

txt                          =  $(embedFile "./docs/blessed/arx.txt")

sendHelp                     =  do
  manResult                 <-  throughMan man
  case manResult of
    Just exitCode           ->  exitWith exitCode
    Nothing                 ->  do Data.ByteString.putStr txt
                                   exitSuccess

{-| Runs man interactively on the input file.
 -}
throughMan                  ::  ByteString -> IO (Maybe ExitCode)
throughMan manText           =  do
  exit                      <-  system "which man"
  if exit /= ExitSuccess
    then  return Nothing
    else  Just <$> withTemporaryDirectory "arx.man." runManInTMP
 where
  runManInTMP dir = do Data.ByteString.writeFile (dir ++ "/arx.man") manText
                       system (unwords ["cd", dir, "&&", "man ./arx.man"])


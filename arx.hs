#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as LazyB
import qualified Data.ByteString as StrictB
import Data.Monoid
import System.Environment
import System.Exit

import qualified Blaze.ByteString.Builder as Blaze

import qualified System.Posix.ARX as ARX
import qualified System.Posix.ARX.Sh as ARX (setEU)


main                         =  do
  ["shdat"]                 <-  getArgs
  input                     <-  LazyB.getContents
  let output                 =  ARX.interpret (ARX.SHDAT chunkSize) input
  (LazyB.putStr . Blaze.toLazyByteString) output
  exitSuccess
 where
  chunkSize                  =  0x20000 -- 128K
  -- chunkSize                  =  0x40000 -- 256K
  -- chunkSize                  =  0x80000 -- 512K
  -- chunkSize                  =  0x100000 -- 1M
  -- chunkSize                  =  0x400000 -- 4M


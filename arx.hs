#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as LazyB
import qualified Data.ByteString as StrictB
import Data.Monoid
import System.Environment
import System.Exit

import qualified Blaze.ByteString.Builder as Blaze

import System.Posix.ARX.HEREDat


main                         =  do
  ["shbin"]                 <-  getArgs
  input                     <-  LazyB.getContents
  let chunks                 =  chunked input
  mapM_ (LazyB.putStr . Blaze.toLazyByteString) chunks
  exitSuccess
 where
  chunkSize                  =   0x400000 -- 4M
  -- chunkSize                  =  0x20000 -- 128K
  chunked input              =  case LazyB.splitAt chunkSize input of
    ("", "")                ->  []
    (a , "")                ->  [chunkIt a]
    (a ,  b)                ->  chunkIt a : chunked b
   where  
    chunkIt                  =  script . chunk . mconcat . LazyB.toChunks


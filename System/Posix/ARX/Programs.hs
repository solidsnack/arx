{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , FlexibleInstances
           , MultiParamTypeClasses
           , FunctionalDependencies #-}

module System.Posix.ARX.Programs where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.ByteString.Lazy as LazyB
import Data.Monoid
import Data.Word

import qualified Blaze.ByteString.Builder as Blaze

import System.Posix.ARX.HEREDat
import qualified System.Posix.ARX.Sh as Sh
import qualified System.Posix.ARX.TMPXTools as TMPXTools
import System.Posix.ARX.Tar


{-| ARX subprograms process some input to produce a script.
 -}
class ARX program input | program -> input where
  interpret                 ::  program -> input -> Blaze.Builder


{-| An 'SHDAT' program processes byte streams with the specified chunking to
    produce a script.
 -}
newtype SHDAT                =  SHDAT Word  -- Chunk size.
instance ARX SHDAT LazyB.ByteString where
  interpret (SHDAT w)        =  localeC . mconcat . chunked
   where
    localeC b                =  "( export LC_ALL=C\n" `mappend` b `mappend` ")"
    chunkSize                =  min (fromIntegral w) maxBound
    chunked input            =  case LazyB.splitAt chunkSize input of
      ("", "")              ->  []
      (a , "")              ->  [chunkIt a]
      (a ,  b)              ->  chunkIt a : chunked b
     where  
      chunkIt                =  script . chunk . mconcat . LazyB.toChunks


{-| A 'TMPX' program archives streams to produce a script that unpacks the
    file data in a temporary location and runs the command with the attached
    environment information in that location. The command may be any
    executable file contents, modulo architectural compatibility. It is
    written along side the temporary work location, to ensure it does not
    collide with any files in the archive. The two boolean flags determine
    when to delete the temporary directory. The first flag determines whether
    or not to delete successful (exit code zero) runs; the second determines
    whether or not to delete failed (exit code non-zero) runs.
 -}
data TMPX = TMPX SHDAT LazyB.ByteString -- Code of task to run.
                       [(Sh.Var, Sh.Val)] -- Environment mapping.
                       Bool -- Destroy tmp if task runs successfully.
                       Bool -- Destroy tmp if task exits with an error code.
                       Bool -- Reuse tmp dir if available.
instance ARX TMPX [(Tar, LazyB.ByteString)] where
  interpret (TMPX encoder run env rm0 rm1 rm2) stuff = TMPXTools.render
    (TMPXTools.Template rm0 rm1 rm2 env' run' archives)
   where
    archives                 =  mconcat (uncurry archive <$> stuff)
    archive tar bytes        =  mconcat
      [shdat bytes, " | tar ", flags tar, "\n"]
    flags TAR                =  "-x"
    flags TGZ                =  "-x -z"
    flags TBZ                =  "-x -j"
    flags TXZ                =  "-x -J"
    run' = case run of ""   ->  ""
                       _    ->  shdat run
    env' = case env of _:_  ->  (shdat . unblz . Sh.render) env
                       [ ]  ->  ""
    shdat                    =  interpret encoder
    unblz                    =  Blaze.toLazyByteString



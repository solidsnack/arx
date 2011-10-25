{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           , StandaloneDeriving #-}

module System.Posix.ARX where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.ByteString.Lazy as LazyB
import Data.Monoid
import Data.Word

import qualified Blaze.ByteString.Builder as Blaze
import qualified Text.ShellEscape as Sh (sh, Sh, Escape(bytes))

import System.Posix.ARX.BlazeIsString -- Most string literals are builders.
import System.Posix.ARX.HEREDat
import qualified System.Posix.ARX.Sh as Sh


{-| ARX subprograms process some input to produce a script.
 -}
class ARX program where
  type Input program        ::  *
  interpret                 ::  program -> Input program -> Blaze.Builder


{-| An 'SHDAT' program processes byte streams with the specified chunking to
    produce a script. 
 -}
newtype SHDAT                =  SHDAT Word  -- ^ Chunk size.
instance ARX SHDAT where
  type Input SHDAT           =  LazyB.ByteString
  interpret (SHDAT w) bytes  =  mconcat (chunked bytes)
   where
    chunkSize                =  min (fromIntegral w) maxBound
    chunked input            =  case LazyB.splitAt chunkSize input of
      ("", "")              ->  []
      (a , "")              ->  [chunkIt a]
      (a ,  b)              ->  chunkIt a : chunked b
     where  
      chunkIt                =  script . chunk . mconcat . LazyB.toChunks


{-| A 'TMPX' program archive streams to produce a script that unpacks the file
    data in a temporary location and runs the command with the attached
    environment information.
 -}
data TMPX                    =  TMPX SHDAT [Sh.Val] [(Sh.Var, Sh.Val)]
instance ARX TMPX where
  type Input TMPX            =  [(Tar, LazyB.ByteString)]
  interpret (TMPX encoder cmd env) stuff = mconcat
    [ "d=/tmp/tmpx.`date -u +%FT%TZ`.$$\n",
      "trap \"rm -rf $d\" EXIT\n",
      "rm -rf $d\n",
      "mkdir $d\n",
      "cd $d\n",
      mconcat (archives stuff),
      "[ $# != 0 ] || set -- " `mappend` Sh.render cmd `mappend` "\n",
      "( # User may trap/set whatever they like...\n",
      Sh.render env,
      "\"$@\"\n",
      ") # ...in their own shell.\n",
      "exit $?\n" ]
   where
    archives stuff           =  case stuff of
      [            ]        ->  []
      (tar, bytes):t        ->  archive tar bytes : archives t
     where
      archive tar bytes      =  mconcat
        ["{\n", interpret encoder bytes, "} | tar ", flags tar, "\n"]
      flags TAR              =  "-x"
      flags TGZ              =  "-x -z"
      flags TBZ              =  "-x -j"


{-| Handled styles of Tar archive.
 -}
data Tar                     =  TAR | TGZ | TBZ
deriving instance Eq Tar
deriving instance Ord Tar
deriving instance Show Tar


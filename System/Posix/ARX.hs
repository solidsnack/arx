
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

{-| An 'SHBIN' program processes byte streams with the specified chunking to
    produce a script. 
 -}
newtype SHBIN                =  SHBIN Word  -- ^ Chunk size.
instance ARX SHBIN where
  type Input SHBIN           =  LazyB.ByteString
  interpret (SHBIN w) bytes  =  mconcat ["{\n", mconcat (chunked bytes), "}"]
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
data TMPX                    =  TMPX SHBIN [Sh.Val] [(Sh.Var, Sh.Val)]
instance ARX TMPX where
  type Input TMPX            =  [(Tar, LazyB.ByteString)]
  interpret (TMPX encoder cmd env) stuff = (mconcat . map (`mappend` "\n"))
    [ "d=/tmp/tmpx.`date -u +%FT%TZ`.$$",
      "trap \"rm -rf $d\" EXIT",
      "rm -rf $d",
      "mkdir $d",
      "cd $d",
      mconcat (archives stuff),
      "[ $# != 0 ] || set -- " `mappend` Sh.render cmd,
      "( \"$@\" )",
      "exit $?" ]
   where
    archives stuff           =  case stuff of
      [            ]        ->  []
      (tag, bytes):t        ->  interpret encoder bytes : tar tag : archives t
    tar TAR                  =  " | tar x\n"
    tar TGZ                  =  " | tar xz\n"
    tar TBZ                  =  " | tar xj\n"


{-| Handled styles of Tar archive.
 -}
data Tar                     =  TAR | TGZ | TBZ
deriving instance Eq Tar
deriving instance Ord Tar
deriving instance Show Tar


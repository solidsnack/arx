{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , StandaloneDeriving #-}
{-| Utilities for working with shell script.
 -}
module System.Posix.ARX.Sh where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import Data.Monoid

import qualified Blaze.ByteString.Builder as Blaze
import qualified Text.ShellEscape as Esc

import System.Posix.ARX.BlazeIsString


setEU                       ::  Blaze.Builder
setEU                        =  "set -e -u\n"

{-| Valid shell string values contain any byte but null. 
 -}
newtype Val                  =  Val ByteString
deriving instance Eq Val
deriving instance Ord Val
deriving instance Show Val
instance Render Val where
  render (Val bytes) = (Blaze.fromByteString . Esc.bytes . Esc.sh) bytes

val                         ::  ByteString -> Maybe Val
val bytes = guard (Bytes.all (/= '\0') bytes) >> Just (Val bytes)

{-| Valid shell variable names consist of a leading letter or underscore and
    then any number of letters, underscores or digits. 
 -}
newtype Var                  =  Var ByteString
deriving instance Eq Var
deriving instance Ord Var
deriving instance Show Var
instance Render Var where
  render (Var bytes) = (Blaze.fromByteString . Esc.bytes . Esc.sh) bytes

var                         ::  ByteString -> Maybe Var
var ""                       =  Nothing
var bytes = guard (leading h && Bytes.all body t) >> Just (Var bytes)
 where
  (h, t)                     =  (Bytes.head bytes, Bytes.tail bytes)
  body c                     =  leading c || (c >= '0' && c <= '9')
  leading c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_'

instance Render [(Var, Val)] where
  render [       ]           =  mempty
  render ((k,v):t)           =  exportStatement `mappend` render t
   where
    exportStatement = mconcat ["export ", render k, "=", render v, "\n"]

instance Render [Val] where
  render                     =  mconcat . map (mappend " " . render)

class Render t where
  render                    ::  t -> Blaze.Builder


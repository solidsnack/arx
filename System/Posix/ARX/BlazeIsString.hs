
module System.Posix.ARX.BlazeIsString where

import Data.String

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze


instance IsString Blaze.Builder where
  fromString                 =  Blaze.fromString


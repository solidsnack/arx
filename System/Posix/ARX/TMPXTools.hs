{-# LANGUAGE TemplateHaskell
           , RecordWildCards
           , PatternGuards   #-}

module System.Posix.ARX.TMPXTools where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import Data.List
import Data.Maybe
import Data.Monoid

import qualified Blaze.ByteString.Builder as Blaze
import Data.FileEmbed

import System.Posix.ARX.BlazeIsString


data Template = Template { rm0 :: Bool, {-^ Remove tmp on task success?   -}
                           rm1 :: Bool, {-^ Remove tmp on task error?     -}
                           tsk :: Blaze.Builder, {-^ Task unpacking text. -}
                           dat :: Blaze.Builder  {-^ Data unpacking text. -} }
instance Show Template where
  show Template{..} =
    "Template { rm0=" ++ tf rm0 ++ " rm1=" ++ tf rm1 ++ " tsk=... dat=... }"
   where
    tf True                  =  "true"
    tf False                 =  "false"

toBuilder                   ::  Template -> Blaze.Builder
toBuilder Template{..}       =  mconcat [ blaze a,
                                          flags, 
                                          blaze b,
                                          tsk,
                                          blaze c,
                                          dat,
                                          blaze d ]
 where
  flags                      =  mconcat ["rm0=",tf rm0," ; ","rm1=",tf rm1,"\n"]
  blaze                      =  Blaze.fromByteString
  (a, b, c, d)               =  template
  tf True                    =  "true"
  tf False                   =  "false"

template :: (ByteString, ByteString, ByteString, ByteString)
template                     =  (a, b, c, d)
 where
  a : b : c : d : [] = findChunks $(embedFile "./model-scripts/tmpx.sh")

findChunks                  ::  ByteString -> [ByteString]
findChunks                   =  coalesce . markHoles

coalesce                    ::  [Maybe ByteString] -> [ByteString]
coalesce                     =  reverse . catMaybes . foldl' f []
 where
  f [           ] item       =  [item]
  f (Just a  : t) (Just b)   =  Just (Bytes.append a b) : t
  f (Nothing : t) (Just b)   =  Just b : Nothing : t
  f (Just a  : t) (Nothing)  =  Nothing : Just a : t
  f (Nothing : t) (Nothing)  =  Nothing : t

markHoles                   ::  ByteString -> [Maybe ByteString]
markHoles                    =  map f . Bytes.lines
 where
  f l | isHole l             =  Nothing
      | otherwise            =  Just (l `Bytes.snoc` '\n')

isHole                      ::  ByteString -> Bool
isHole line                  =  "# To be set by tool." `Bytes.isSuffixOf` line


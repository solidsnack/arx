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


data Template = Template { rm0 :: Bool, {-^ Remove tmp on run success?    -}
                           rm1 :: Bool, {-^ Remove tmp on run error?      -}
                           env :: Blaze.Builder, {-^ Stream for env text. -}
                           run :: Blaze.Builder, {-^ Stream for run text. -}
                           dat :: Blaze.Builder  {-^ Data unpacking text. -} }
instance Show Template where
  show Template{..} =
    "Template { rm0=" ++ tf rm0 ++ " rm1=" ++ tf rm1 ++ " ... }"
   where
    tf True                  =  "true"
    tf False                 =  "false"

render                      ::  Template -> Blaze.Builder
render Template{..}          =  mconcat [ blaze a,
                                          flags, 
                                          blaze b,
                                          env,
                                          blaze c,
                                          run,
                                          blaze d,
                                          dat,
                                          blaze e ]
 where
  flags                      =  mconcat ["rm0=",tf rm0," ; ","rm1=",tf rm1,"\n"]
  blaze                      =  Blaze.fromByteString
  tf True                    =  "true"
  tf False                   =  "false"
  a : b : c : d : e : [] = findChunks $(embedFile "./model-scripts/tmpx.sh")

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


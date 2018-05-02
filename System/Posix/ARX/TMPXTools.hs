{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
           , RecordWildCards
           , PatternGuards   #-}

module System.Posix.ARX.TMPXTools where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Numeric


import qualified Blaze.ByteString.Builder as Blaze
import Data.FileEmbed
import Data.Hashable


data Template = Template { rm0    :: Bool, {-^ Remove tmp on run success?    -}
                           rm1    :: Bool, {-^ Remove tmp on run error?      -}
                           shared :: Bool, {-^ Share directory across runs?  -}
                           tmpdir :: String, {-^ Location to store tmp files.-}
                           env    :: Blaze.Builder, {-^ Stream for env text. -}
                           run    :: Blaze.Builder, {-^ Stream for run text. -}
                           dat    :: Blaze.Builder  {-^ Data text. -} }
instance Show Template where
  show Template{..} =
    "Template { tmpdir=" tmpdir ++ " rm0=" ++ tf rm0 ++ " rm1=" ++ tf rm1 ++
              " shared=" ++ tf shared ++  " ... }"
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
  flags                      =  mconcat [ "rm0=", tf rm0, " ; ",
                                          "rm1=", tf rm1, " ; ",
                                          "shared=", tf shared, " ; ",
                                          "hash=", (hexStr . hash) dat,
                                          "tmpdir=", tmpdir, "\n" ]
  hash                       =  abs . Data.Hashable.hash . Blaze.toByteString
  hexStr                     =  blaze . Bytes.pack . hex
  hex i = Numeric.showIntAtBase 16 Data.Char.intToDigit i ""
  blaze                      =  Blaze.fromByteString
  tf True                    =  "true"
  tf False                   =  "false"
  a : b : c : d : e : f : [] = findChunks $(embedFile "./model-scripts/tmpx.sh")

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


{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

module System.Posix.ARX.Tar where

import Prelude hiding (drop, take)
import Data.ByteString.Lazy.Char8


{-| Handled styles of Tar archive.
 -}
data Tar                     =  TAR | TBZ | TGZ | TXZ
deriving instance Eq Tar
deriving instance Ord Tar
deriving instance Show Tar


{-| Scan a lazy ByteString for file magic.
 -}
magic                       ::  ByteString -> Maybe Tar
magic b | bzMagic b          =  Just TBZ
        | gzMagic b          =  Just TGZ
        | tarMagic b         =  Just TAR
        | xzMagic b          =  Just TXZ
        | otherwise          =  Nothing

bzMagic                      =  (== "BZh")      . take 3

gzMagic                      =  (== "\x1F\x8b") . take 2

tarMagic                     =  (== "ustar")    . take 5 . drop 257

xzMagic                      =  (== xzBytes)    . take 6
 where
  xzBytes                    = '\xFD' `cons` "7zXZ\0"

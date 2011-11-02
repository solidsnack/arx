{-# LANGUAGE OverloadedStrings
           , TupleSections
           , StandaloneDeriving #-}

module System.Posix.ARX.CLI where

import Control.Applicative hiding (many)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as Char8
import Data.Either
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import Text.Parsec hiding (satisfy, (<|>))

import System.Posix.ARX.CLI.CLTokens (Class(..))
import qualified System.Posix.ARX.CLI.CLTokens as CLTokens
import System.Posix.ARX.CLI.Options
import System.Posix.ARX.Programs
import qualified System.Posix.ARX.Sh as Sh
import System.Posix.ARX.Tar


--parseArgs args = do


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


arx                          =  do
  parsed                    <-  Left <$> shdat <|> Right <$> tmpx
  case parsed of
    Left shdatStuff         ->  undefined
    Right tmpxStuff         ->  undefined

shdatResolve                ::  ([Word], [IOStream], [IOStream])
                            ->  (SHDAT, IOStream, [IOStream])
shdatResolve                 =  undefined

tmpxResolve :: ( [Word], [IOStream], [IOStream], [(Tar, IOStream)],
                 [(Sh.Var, Sh.Val)], [(Bool, Bool)], [ByteSource]  )
            -> (TMPX, IOStream, [IOStream], [(Tar, IOStream)])
tmpxResolve                  =  undefined


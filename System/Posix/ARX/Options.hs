{-# LANGUAGE OverloadedStrings
           , TupleSections
           , StandaloneDeriving #-}

module System.Posix.ARX.Options where

import Control.Applicative hiding (many)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as Char8
import Data.Either
import Data.List
import Data.Word
import Text.Parsec hiding (satisfy, (<|>))

import qualified Data.Attoparsec

import System.Posix.ARX.CLTokens (Class(..))
import qualified System.Posix.ARX.CLTokens as CLTokens
import qualified System.Posix.ARX.Sh as Sh
import System.Posix.ARX.Tar


{-| Finds a run of two or more slashes; if there is more than one such run,
    returns the longest one.
 -}
longestSlashRun             ::  [ByteString] -> Maybe ByteString
longestSlashRun              =  foldl' f Nothing
 where
  f Nothing b  | slashes b   =  guard (Bytes.length b > 1) >> Just b
               | otherwise   =  Nothing
  f (Just a) b | slashes b   =  Just (longest a b)
               | otherwise   =  Just a
  slashes                    =  Char8.all (== '/')
  longest a b                =  if Bytes.length b > Bytes.length a then b
                                                                   else a


blockSize                   ::  ArgsParser Word
blockSize                    =  do arg "-b"
                                   CLTokens.sizeBounded <@> tokCL Size

outputFile                  ::  ArgsParser IOStream
outputFile                   =  arg "-o" >> ioStream

ioStream                    ::  ArgsParser IOStream
ioStream                     =  STDIO <$  tokCL Dash
                            <|> Path  <$> tokCL QualifiedPath

tar                         ::  ArgsParser (Tar, IOStream)
tar = (,) <$> (TAR <$ arg "-tar" <|> TBZ <$ arg "-tbz" <|> TGZ <$ arg "-tgz")
          <*> ioStream

rm                          ::  ArgsParser (Bool, Bool)
rm  =   (True,  False) <$ arg "-rm0"  <|>  (False, True) <$ arg "-rm1"
   <|>  (False, False) <$ arg "-rm!"  <|>  (True,  True) <$ arg "-rm_"

env                         ::  ArgsParser (Sh.Var, Sh.Val)
env                          =  do
  (var, assignment)         <-  Char8.break (== '=') <$> tokCL EnvBinding
  case (,) <$> Sh.var var <*> Sh.val (Bytes.drop 1 assignment) of
    Nothing                 ->  mzero
    Just x                  ->  return x

run                         ::  ArgsParser ByteSource
run                          =  arg "-run" >> IOStream <$> ioStream

cmd                         ::  ByteString -> ArgsParser ByteSource
cmd bars = ByteString . Char8.unwords <$> bracketed bars bars anyArg
 where
  bracketed start end p      =  arg start >> manyTill p (eof <|> () <$ arg end)


{-| A byte-oriented store that can be read from or written to in a streaming
    fashion.
 -}
data IOStream                =  STDIO | Path !ByteString
deriving instance Eq IOStream
deriving instance Ord IOStream
deriving instance Show IOStream

{-| A source of bytes (no writing, only reading).
 -}
data ByteSource              =  ByteString !ByteString | IOStream !IOStream
deriving instance Eq ByteSource
deriving instance Ord ByteSource
deriving instance Show ByteSource


type ArgsParser              =  Parsec [ByteString] ()

satisfy                     ::  (ByteString -> Bool) -> ArgsParser ByteString
satisfy p                    =  argPrim test
 where
  test b                     =  guard (p b) >> Just b

anyArg                      ::  ArgsParser ByteString
anyArg                       =  argPrim Just

arg                         ::  ByteString -> ArgsParser ByteString
arg b                        =  satisfy (== b)

argPrim                     ::  (ByteString -> Maybe t) -> ArgsParser t
argPrim                      =  tokenPrim show next
 where
  next pos _ _               =  incSourceLine pos 1

(<@>) :: Data.Attoparsec.Parser t -> ArgsParser ByteString -> ArgsParser t
atto <@> parsec              =  do
  res                       <-  Data.Attoparsec.parseOnly atto <$> parsec
  case res of Left _        ->  mzero
              Right x       ->  return x
infixl 4 <@>

tokCL                       ::  Class -> ArgsParser ByteString
tokCL tokenClass             =  satisfy (CLTokens.match tokenClass)

interleaveMany a b = partitionEithers <$> many (Left <$> a <|> Right <$> b)


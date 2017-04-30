{-# LANGUAGE OverloadedStrings
           , TupleSections
           , StandaloneDeriving #-}

module System.Posix.ARX.CLI where

import Control.Applicative hiding (many)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyB
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Word
import System.Environment
import System.Exit (exitFailure)
import System.IO

import qualified Blaze.ByteString.Builder as Blaze
import Text.Parsec hiding (satisfy, (<|>))

import System.Posix.ARX.CLI.CLTokens (Class(..))
import qualified System.Posix.ARX.CLI.CLTokens as CLTokens
import System.Posix.ARX.CLI.Options
import System.Posix.ARX


{-| Run CLI tool, processing arguments and options.
 -}
main                        ::  IO ()
main                         =  do
  args                      <-  (Char8.pack <$>) <$> getArgs
  case parse arx "<args>" args of
    Left _                  ->  die "Argument error."
    Right (Left shdatArgs)  ->  do
      let (size, out, ins)   =  shdatResolve shdatArgs
      case shdatCheckStreams ins of Nothing  -> return ()
                                    Just msg -> do die msg
      let apply i            =  interpret (SHDAT size) <$> inIOStream i
      mapM_ ((send out =<<) . apply) ins
    Right (Right tmpxArgs)  ->  do
      let (size, out, tars, env, (rm0, rm1, rm2), cmd) = tmpxResolve tmpxArgs
      case tmpxCheckStreams tars cmd of Nothing  -> return ()
                                        Just msg -> do die msg
      cmd'                  <-  openByteSource cmd
      let tmpx               =  TMPX (SHDAT size) cmd' env rm0 rm1 rm2
      (badAr, goodAr)       <-  partitionEithers <$> mapM openArchive tars
      (badAr /= []) `when` do (((die .) .) . blockMessage)
                                "The file magic of some archives:"
                                badAr
                                "could not be interpreted."
      send out (interpret tmpx goodAr)
 where
  arx                        =  Left <$> shdat <|> Right <$> tmpx
  name STDIO                 =  "-"
  name (Path b)              =  b
  send o b                   =  (outIOStream o . Blaze.toLazyByteString) b
  openArchive io             =  do r <- arIOStream io
                                   return $ case r of Nothing -> Left (name io)
                                                      Just x  -> Right x

{-| Apply defaulting and overrides appropriate to 'SHDAT' programs.
 -}
shdatResolve                ::  ([Word], [IOStream], [IOStream])
                            ->  (Word, IOStream, [IOStream])
shdatResolve (sizes, outs, ins) = (size, out, ins')
 where
  size                       =  last (defaultBlock:sizes)
  out                        =  last (STDIO:outs)
  ins' | ins == []           =  [STDIO]
       | otherwise           =  ins

shdatCheckStreams           ::  [IOStream] -> Maybe ByteString
shdatCheckStreams ins        =  streamsMessage [ins']
 where
  ins'                       =  case [ x | x <- ins, x == STDIO ] of
      []                    ->  Zero
      [_]                   ->  One "as a file input"
      _:_:_                 ->  Many ["more than once as a file input"]


{-| Apply defaulting and overrides appropriate to 'TMPX' programs.
 -}
tmpxResolve                 ::  ( [Word], [IOStream], [IOStream],
                                  [(Var, Val)], [(Bool, Bool, Bool)], [ByteSource] )
                            ->  ( Word, IOStream, [IOStream],
                                  [(Var, Val)], (Bool, Bool, Bool), ByteSource )
tmpxResolve (sizes, outs, tars, env, rms, cmds) =
  (size, out, tars, env, rm, cmd)
 where
  size                       =  last (defaultBlock:sizes)
  out                        =  last (STDIO:outs)
  rm                         =  last ((True,True,False):rms)
  cmd                        =  last (defaultTask:cmds)

tmpxCheckStreams            ::  [IOStream] -> ByteSource -> Maybe ByteString
tmpxCheckStreams tars cmd    =  streamsMessage [tars', cmd']
 where
  tars'                      =  case [ x | x <- tars, x == STDIO ] of
      []                    ->  Zero
      [_]                   ->  One "as an archive input"
      _:_:_                 ->  Many ["more than once as an archive input"]
  cmd'
    | cmd == IOStream STDIO  =  One "as a command input"
    | otherwise              =  Zero

tmpxOpen :: Word -> [(Var, Val)] -> (Bool, Bool, Bool) -> ByteSource -> IO TMPX
tmpxOpen size env (rm0, rm1, rm2) cmd = do
  text                      <-  case cmd of
    ByteString b            ->  return (LazyB.fromChunks [b])
    IOStream STDIO          ->  LazyB.getContents
    IOStream (Path b)       ->  LazyB.readFile (Char8.unpack b)
  return (TMPX (SHDAT size) text env rm0 rm1 rm2)


openByteSource              ::  ByteSource -> IO LazyB.ByteString
openByteSource source        =  case source of
    ByteString b            ->  return (LazyB.fromChunks [b])
    IOStream STDIO          ->  LazyB.getContents
    IOStream (Path b)       ->  LazyB.readFile (Char8.unpack b)

inIOStream STDIO             =  LazyB.getContents
inIOStream (Path b)          =  LazyB.readFile (Char8.unpack b)

outIOStream STDIO            =  LazyB.putStr
outIOStream (Path b)         =  LazyB.writeFile (Char8.unpack b)

arIOStream                  ::  IOStream -> IO (Maybe (Tar, LazyB.ByteString))
arIOStream io                =  do opened <- inIOStream io
                                   return ((,opened) <$> magic opened)


{-| By default, we encode binary data to HERE docs 4MiB at a time. (The
    encoded result may be up to 10% larger, though 1% is more likely.)
 -}
defaultBlock                ::  Word
defaultBlock                 =  0x400000

{-| The default task is a no-op call to @\/bin\/true@.
 -}
defaultTask                 ::  ByteSource
defaultTask                  =  ByteString "/bin/true"


data ZOM                     =  Zero | One !ByteString | Many ![ByteString]
instance Monoid ZOM where
  mempty                     =  Zero
  Zero    `mappend` x        =  x
  x       `mappend` Zero     =  x
  One m   `mappend` One m'   =  Many [m, m']
  One m   `mappend` Many ms  =  Many (mappend [m] ms)
  Many ms `mappend` One m    =  Many (mappend ms  [m])
  Many ms `mappend` Many ms' =  Many (mappend ms  ms')

streamsMessage filtered      =  case foldl' mappend Zero filtered of
  Many messages             ->  Just (template messages)
  _                         ->  Nothing
 where
  template clauses           =  blockMessage
                                  "STDIN is specified multiple times:"
                                  clauses
                                  "but restreaming STDIN is not supported."

blockMessage a bs c          =  Char8.unlines
  [a, Bytes.intercalate ",\n" (mappend "  " <$> bs), c]

err ""                       =  return ()
err b | Char8.last b == '\n' =  Char8.hPutStr stderr b
      | otherwise            =  Char8.hPutStr stderr (b `Char8.snoc` '\n')

die msg                      =  err msg >> exitFailure


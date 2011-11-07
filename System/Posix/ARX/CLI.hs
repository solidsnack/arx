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
import System.Exit
import Text.Parsec hiding (satisfy, (<|>))

import System.Posix.ARX.CLI.CLTokens (Class(..))
import qualified System.Posix.ARX.CLI.CLTokens as CLTokens
import System.Posix.ARX.CLI.Options
import System.Posix.ARX.Programs
import qualified System.Posix.ARX.Sh as Sh
import System.Posix.ARX.Tar


main                         =  do
  args                      <-  Char8.pack <$> getArgs
  case parse "<args>" arx args of
    Left _                  ->  do
      putStrLn "Argument error."
      exitSuccess
    Right (Left shdatArgs)  ->  do
      let (size, out, ins)   =  shdatResolve args
      case shdatCheckStreams ins of
        Nothing             ->  return ()
        Just err            ->  do Char8.putStrLn err
                                   exitFailure
      let apply i            =  interpret (SHDAT size) <$> inIOStream i
      mapM_ (send out =<< apply) ins
    Right (Right tmpxArgs)  ->  do
      let (size, out, ins, tars, env, (rm0, rm1), cmd) = tmpxResolve args
      case tmpxCheckStreams ins tars cmd of
        Nothing             ->  return ()
        Just err            ->  do Char8.putStrLn err
                                   exitFailure
      cmd'                  <-  openByteSource cmd
      let tmpx               =  TMPX (SHDAT size) cmd' env rm0 rm1
      opened                 =  mapM (second inIOStream) tars
      send out =<< interpret tmpx opened
 where
  arx                        =  Left <$> shdat <|> Right <$> tmpx
  send o builder             =  do
    (outIOStream o . Blaze.toLazyByteString) builder

{-| Apply defaulting and overrides appropriate to 'SHDAT' programs.
 -}
shdatResolve                ::  ([Word], [IOStream], [IOStream])
                            ->  (Word, IOStream, [IOStream])
shdatResolve (sizes, outs, ins) = (size, out, ins)
 where
  size                       =  last (defaultBlock:sizes)
  out                        =  last (STDIO:outs)

shdatCheckStreams           ::  [IOStream] -> Maybe ByteString
shdatCheckStreams ins        =  streamsMessage [ins']
 where
  ins'                       =  case [ x == STDIO | x <- ins ] of
      []                    ->  Zero
      [_]                   ->  One "as a file input"
      _:_:_                 ->  Many ["more than once as a file input"]


{-| Apply defaulting and overrides appropriate to 'TMPX' programs.
 -}
tmpxResolve :: ( [Word], [IOStream], [IOStream], [(Tar, IOStream)],
                 [(Sh.Var, Sh.Val)], [(Bool, Bool)], [ByteSource]  )
            -> ( Word, IOStream, [IOStream], [(Tar, IOStream)],
                 [(Sh.Var, Sh.Val)], (Bool, Bool), ByteSource  )
tmpxResolve (sizes, outs, ins, tars, env, rms, cmds) =
  (size, out, ins, tarsWithDefaulting, env, rm, cmd)
 where
  size                       =  last (defaultBlock:sizes)
  out                        =  last (STDIO:outs)
  rm                         =  last ((True,True):rms)
  cmd                        =  last (defaultTask:cmds)
  tarsWithDefaulting
    | ins == [] && tars == [] = [(TAR, STDIO)]
    | otherwise              =  tars

tmpxCheckStreams            ::  [IOStream] -> [(Tar, IOStream)] -> ByteSource
                            ->  Maybe ByteString
tmpxCheckStreams ins tars cmd = streamsMessage [tars', ins', cmd']
 where
  tars'                      =  case [ x == STDIO | x <- (snd <$> tars) ] of
      []                    ->  Zero
      [_]                   ->  One "as a tar input"
      _:_:_                 ->  Many ["more than once as a tar input"]
  ins'                       =  case [ x == STDIO | x <- ins ] of
      []                    ->  Zero
      [_]                   ->  One "as a file input"
      _:_:_                 ->  Many ["more than once as a file input"]
  cmd'
    | cmd == IOStream STDIO  =  One "as a command input"
    | otherwise              =  Zero

tmpxOpen :: Word -> [(Sh.Var, Sh.Val)] -> (Bool, Bool) -> ByteSource -> IO TMPX
tmpxOpen size env (rm0, rm1) cmd = do
  text                      <-  case cmd of
    ByteString b            ->  return (LazyB.fromChunks [b])
    IOStream STDIO          ->  LazyB.getContents
    IOStream (Path b)       ->  LazyB.readFile (Char8.unpack b)
  return (TMPX (SHDAT size) text env rm0 rm1)


openByteSource              ::  ByteSource -> IO LazyB.ByteString
openByteSource source        =  case source of
    ByteString b            ->  return (LazyB.fromChunks [b])
    IOStream STDIO          ->  LazyB.getContents
    IOStream (Path b)       ->  LazyB.readFile (Char8.unpack b)

inIOStream STDIO             =  LazyB.getContents
inIOStream (Path b)          =  LazyB.readFile (Char8.unpack b)

outIOStream STDIO            =  LazyB.putStr
outIOStream (Path b)         =  LazyB.writeFile (Char8.unpack b)


{-| By default, we encode binary data to HERE docs 4MiB at a time. (The
    encoded result may be up to 10% larger, though 1% is more likely.)
 -}
defaultBlock                ::  Word
defaultBlock                 =  0x400000

{-| The default task is a no-op call to @/bin/true@.
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
  template clauses           =  Char8.unlines
    [ "STDIN is specified multiple times:",
      Bytes.intercalate ",\n" (mappend "  " <$> clauses),
      "but restreaming STDIN is not supported."           ]


{-# LANGUAGE OverloadedStrings
           , TupleSections
           , StandaloneDeriving #-}

{-| Utilities for encoding arbitrary data as Bourne shell fragments that
    stream the data to standard output, using HERE documents and simple shell
    decoders.
 -}
module System.Posix.ARX.HEREDat where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Internal as Bytes (c2w)
import qualified Data.List as List
import Data.Monoid
import Data.Ord
import Data.String
import Data.Word
import Numeric (showOct, showHex)

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString.Nums.Careless as Bytes

import System.Posix.ARX.BlazeIsString


{-| A chunk describes a block of binary data ready for inclusion in a shell
    script. For many data blocks, no encoding or decoding is necessary; these
    are stored in a 'SafeChunk'. Those blocks needing byte-translation are
    stored in an 'EncodedChunk'.
 -}
data Chunk                   =  SafeChunk !ByteString
                             |  EncodedChunk !ByteString -- Encoded data.
                                             !Int        -- Original length.
                                             !EscapeChar -- Null replacer.
                                             !EscapeChar -- Escaper.
deriving instance Show Chunk
instance IsString Chunk where
  fromString                 =  chunk . Data.ByteString.Char8.pack

{-| Converts a 'ByteString' into a string safe for inclusion in a shell HERE
    document and annotates with information to construct a shell decoder for
    that document, if necessary.

    A 'ByteString' with nulls is rewritten in a complicated way. Two escape
    characters are chosen from a class of ASCII printable characters that look
    like reasonable escape characters; the two that show up least frequently
    in the document (including 0 times) become the null replacer and the
    escaper. All instances of these two characters are rewritten to escape
    sequences formed with the escaper, while nulls are rewritten to the null
    replacer. Given the two characters thus chosen, a command line with @tr@
    and @sed@ in sequence can be constructed to decode the document.

    This encoding doubles the amount of space consumed by the escape
    characters. In the worst case, where the data is made of all 20 potential
    escapes, evenly distributed, and one null (so we can't punt on escaping),
    the data will grow in size by 10 percent. For data that is more evenly
    distributed over the bytes -- as we might expect of compressed tarballs --
    we expect a size growth of two 256ths, or less than 0.8 percent.
 -}
chunk                       ::  ByteString -> Chunk
chunk block                  =  EncodedChunk (encode nW eW block)
                                             (Bytes.length block) nEsc eEsc
--  | safeForHereDoc block     =  SafeChunk block
--  | otherwise                =  EncodedChunk (encode nW eW block)
--                                             (Bytes.length block) nEsc eEsc
 where
  nEsc@(EscapeChar nW _ _ _) :
    eEsc@(EscapeChar eW _ _ _) : _ = snd <$> List.sortBy (comparing cmp) counts
  cmp (count, EscapeChar w _ _ _) = (count, w)
  counts                     =  countAndBundle <$> escapes
   where
    countAndBundle e@(EscapeChar w _ _ _) = (Bytes.count w block, e)

{-| Given a byte to replace nulls and an escape byte, rewrites the data such
    that nulls are mapped to the replace byte, replace bytes are mapped to a
    pair of escape bytes and the escape byte is is mapped to an escape byte
    followed by an underscore. For example, if the null replace byte is @!@
    and the escape byte is @\#@ then all nulls become @!@, any @!@ become
    @\#\#@ and all @\#@ become @\#_@.

    This escaping scheme is dictated by the needs of our Sed decoder, which is
    just two global substitions, one after another. If the escaping were such
    that, with our characters above, @\#@ escaped to @\#\#@ and @!@ to @\#_@,
    then @\#_@ in the input becomes @\#\#_@. We want to run the subsitution
    for @\#@ first, to catch this; it produces @\#_@; then Sed feeds the input
    to the second substitution which unfortunately renders @!@. In the
    alternate scheme, the input is encoded @\#__@, the @!@ decoder runs first
    and ignores it, then the @\#@ decoder runs and catches it. When using a
    pipeline of stream processors to interpret escape sequences, it seems best
    to ensure that only the very last processor inserts escape characters, to
    prevent their further interpretation.
 -}
encode                      ::  Word8 -> Word8 -> ByteString -> ByteString
encode nullReplaceByte escapeByte bytes =
  fst $ Bytes.unfoldrN len f (Nothing, bytes)
 where
  --  The encoding should introduce at most 10% overhead; we allocate a little
  --  more just to be safe. This allows us to make use of the somewhat faster
  --  unfoldrN function (which probably pre-allocates).
  len = ceiling (fromIntegral (Bytes.length bytes) * 1.25)
  -- The worker sometimes floats up a byte, sometimes escapes a byte and
  -- introduces a byte to be 'carried' (like carryies in arithmetic) and
  -- sometimes floats up the carried byte.
  f (Just carried, bytes)    =  Just (carried, (Nothing, bytes))
  f (Nothing     , bytes)    =  do
    ((b, carry), t)         <-  first rewrite <$> Bytes.uncons bytes
    Just (b, (carry, t))
  rewrite b
    | b == 0x00              =  (nullReplaceByte, Nothing)
    | b == escapeByte        =  (escapeByte     , Just underscore)
    | b == nullReplaceByte   =  (escapeByte     , Just escapeByte)
    | otherwise              =  (b              , Nothing)
  underscore                 =  Bytes.c2w '_'

{-| Given the byte used to replace nulls and the escape byte, undoes the result
    of the encode operation -- rewriting null replacers to literal nulls and
    escape patterns to the original bytes. This function is not intended to be
    used in practice -- it will be shell commands that unpack the data -- but
    serves to document the ideas behind decoding as well as offering a way to
    check the encoder.
 -}
decode                      ::  Word8 -> Word8 -> ByteString -> ByteString
decode nullReplaceByte escapeByte = (unEscape . Bytes.map unReplace)
 where
  unReplace b
    | b == nullReplaceByte   =  0x00
    | otherwise              =  b
  unEscape                   =  Bytes.concat . List.reverse . fst
                             .  List.foldl' f ([], False)
                             .  Bytes.split escapeByte
   where
    nS                       =  Bytes.singleton nullReplaceByte
    f (strings, True)  ""    =  (nS:strings , False)
    f (strings, False) ""    =  (strings    , True)
    f (strings, False) s     =  (s:strings  , True)
    f (strings, True)  s
      | underscore           =  (eSt:strings, True)
      | otherwise            =  (s:strings  , True)
     where
      underscore             =  Bytes.head s == Bytes.c2w '_'
      eSt                    =  Bytes.cons escapeByte (Bytes.tail s)
   {- The second field of the tuple is the "escaped" flag and the reasoning
    - behind it's setting and unsetting is tricky. We start unescaped. If a
    - string follows another string in the list of splits, there must have
    - been an escape character to make us split it; therefore, seeing a string
    - makes us set escaping to True. However, if we see an empty string, it
    - means there were two escape characters next to one another. We
    - interpret the double escape sequence and unset the escape flag.
    -}

data EscapeChar = EscapeChar !Word8 !ByteString -- For @tr@ char list.
                                    !ByteString -- For @sed@ pattern.
                                    !ByteString -- For @sed@ replacement.
deriving instance Show EscapeChar

{-| The candidate escape characters, with the forms to be used in constructed
    @tr@ and @sed@ commands.
 -}
escapes                     ::  [EscapeChar]
escapes                      =  [EscapeChar  0x21  "!"    "!"    "!",
                                 EscapeChar  0x22  "\""   "\""   "\"",
                                 EscapeChar  0x23  "#"    "#"    "#",
                                 EscapeChar  0x24  "$"    "[$]"  "$",
                                 EscapeChar  0x25  "%"    "%"    "%",
                                 EscapeChar  0x26  "&"    "&"    "\\&",

                                 EscapeChar  0x2a  "*"    "[*]"  "*",
                                 EscapeChar  0x2b  "+"    "[+]"  "+",
                                 EscapeChar  0x2c  ","    ","    ",",
                                 EscapeChar  0x2d  "-"    "-"    "-",
                                 EscapeChar  0x2e  "."    "[.]"  ".",
                                 EscapeChar  0x2f  "/"    "/"    "/",

                                 EscapeChar  0x3a  ":"    ":"    ":",
                                 EscapeChar  0x3b  ";"    ";"    ";",

                                 EscapeChar  0x3d  "="    "="    "=",

                                 EscapeChar  0x3f  "?"    "[?]"  "?",
                                 EscapeChar  0x40  "@"    "@"    "@",

                                 EscapeChar  0x5c  "\\\\" "\\\\" "\\\\",

                                 EscapeChar  0x60  "`"    "`"    "`",

                                 EscapeChar  0x7e  "~"    "~"    "~"]
{- We use character classes instead of \ for many characters on the pattern
 - side because \ turns special behaviour on in basic mode and off in extended
 - mode, an ambiguity that, I feel, is best not to have to think about.
 -}

{-| Many binary strings can be embedded as-is in a HEREDOC, without escaping.
 -}
safeForHereDoc              ::  ByteString -> Bool
safeForHereDoc               =  not . Bytes.any (== 0x00)

{-| Predicate to determine whether data is represented as an encoded chunk or
    is unencoded.
 -}
encoded                     ::  Chunk -> Bool
encoded (SafeChunk _)        =  False
encoded (EncodedChunk _ _ _ _) = True

{-|  
 -}
script block                 =  mconcat $ case block of
  SafeChunk bytes           ->  [script (chunk bytes)] -- Convert to Encoded
  EncodedChunk bytes len
               (EscapeChar _ trN _ sedRN) (EscapeChar b _ sedPE sedRE) ->
    [ "{ ", mconcat tr, " | ", mconcat sed, " | ", clip len, " ;}",
      dataSection (Blaze.fromWord8 b) bytes ]
   where
    tr                       =  ["tr '", blz trN, "' '\\000'"]
    (e, e', n)               =  (blz sedPE, blz sedRE, blz sedRN)
    sed                      =  ["sed '","s|",e, e, "|",n, "|g",
                                   " ; ","s|",e,"_","|",e',"|g","'"]
 where
  blz                        =  Blaze.fromByteString
  nl                         =  Blaze.fromChar '\n'
  dataSection eof bytes = mconcat [" <<\\", eof, nl, blz bytes, nl, eof, nl]
  clip len                   =  "head -c " `mappend` Blaze.fromShow len


 {- Catting a tarball escaped this way to a shell behind a TTY won't work very
  - well: a ^C or ^Z is passed literally and would cause the TTY to kill or
  - suspend the shell.
  -
  - One reason users might care about this is the 'requiretty' option in
  - sudoers, an option set by default on many systems. It prevents one from
  - running `sudo ...' over SSH without a TTY (enabled through the -t flag to
  - SSH).
  -
  - There are 33 control characters, counting delete with the leading 32. Some
  - don't need to be escaped at all -- for example, newline -- whereas for
  - others, it's unclear (like carriage return). We can trust, I think, that
  - bytes higher than 127 don't need to be escaped. In principle, we have a
  - base-222 alphabet in which to encode the data so it should still be more
  - more compact than base 64; but whether shell decoders can effectively
  - realize this efficiency is another matter.
  -}


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
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Vector
import qualified Data.Vector.Algorithms.Intro as Vector


{-| A chunk describes a block of binary data ready for inclusion in a shell
    script. For many data blocks, no encoding or decoding is necessary; these
    are stored in a 'SafeChunk'. Those blocks needing byte-translation are
    stored in an 'EncodedChunk'.
 -}
data Chunk                   =  SafeChunk !ByteString
                             |  EncodedChunk !ByteString -- ^ Encoded data.
                                             !Int        -- ^ Original length.
                                             !EscapeChar -- ^ Null replacer.
                                             !EscapeChar -- ^ Escaper.
deriving instance Show Chunk
instance IsString Chunk where
  fromString                 =  chunk . Data.ByteString.Char8.pack

{-| Converts a 'ByteString' into a string safe for inclusion in a shell HERE
    document and annotates with information to construct a shell decoder for
    that document, if necessary.

    When a 'ByteString' contains no nulls, no escaping is needed: HERE
    documents with a "quoted delimiter" do not undergo any variable
    interpolation or escape sequence interpretation whatsoever (such HERE
    documents happen to print much faster than the unquoted delimiter kind).

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
chunk block
  | safeForHereDoc block     =  SafeChunk block
  | otherwise                =  EncodedChunk (encode nW eW block)
                                             (Bytes.length block)
                                             (EscapeChar nW (octal nW) nSed)
                                             (EscapeChar eW (octal eW) eSed)
 where
  (nW, nSed):(eW, eSed):_    =  snd <$> List.sortBy (comparing cmp) counts
  cmp (count, (w, s))        =  (count, w)
  counts                     =  countAndBundle <$> escapes
   where
    countAndBundle (w, s)    =  (Bytes.count w block, (w, s))

{-| Given a byte to replace nulls and an escape byte, rewrites the data such
    that nulls are mapped to the replace byte, replace bytes are mapped to a
    pair of escape bytes and the escape byte is is mapped to an escape byte
    followed by an underscore. For example, if the null replace byte is @!@
    and the escape byte is @#@ then all nulls become @!@, any @!@ become @##@
    and all @#@ become @#_@.

    This escaping scheme is dictated by the needs of our Sed decoder, which is
    just two global substitions, one after another. If the escaping were such
    that, with our characters above, @#@ escaped to @##@ and @!@ to @#_@, then
    @#_@ in the input becomes @##_@. We want to run the subsitution for @#@
    first, to catch this; it produces @#_@; then Sed feeds the input to the
    second substitution which unfortunately renders @!@. In the alternate
    scheme, the input is encoded @#__@, the @!@ decoder runs first and ignores
    it, then the @#@ runs and catches it. When using a pipeline of stream
    processors to interpret escape sequences, it seems best to ensure that
    only the very last processor inserts escape characters, to prevent their
    further interpretation.
 -}
encode                      ::  Word8 -> Word8 -> ByteString -> ByteString
encode nullReplaceByte escapeByte = Bytes.concatMap rewrite
 where
  rewrite b
    | b == 0x00              =  Bytes.singleton nullReplaceByte
    | b == escapeByte        =  Bytes.pack      [escapeByte, Bytes.c2w '_']
    | b == nullReplaceByte   =  Bytes.pack      [escapeByte, escapeByte]
    | otherwise              =  Bytes.singleton b

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

--  ! " # $ % & '    * + , - . /    : ;    ? @    \    `    ~
{-| The escape candidate characters, paired with forms suitable for inclusion
    in Sed scripts.
 -}
escapes                     ::  [(Word8, ByteString)]
escapes                      =  first Bytes.c2w <$>
                                  [ ('!',"!"),  ('"',"\""),
                                    ('#',"#"),  ('$',"\\$"),
                                    ('%',"%"),  ('&',"\\&"),
                                    ('\'',"'"), ('*',"*"),
                                    ('+',"+"),  (',',","),
                                    ('-',"-"),  ('.',"\\."),
                                    ('/',"/"),  (':',":"),
                                    (';',";"),  ('?',"\\?"),
                                    ('@',"@"),  ('\\',"\\\\"),
                                    ('`',"`"),  ('~',"~")      ]

{-| Present a 'Word8' as an octal literal, suitable for use with @tr@. 
 -}
octal                       ::  Word8 -> ByteString
octal w = Data.ByteString.Char8.pack $ case s of []    -> "\\000"
                                                 [_]   -> "\\00" ++ s
                                                 [_,_] -> "\\0" ++ s
                                                 _     -> "\\" ++ s
 where
  s                          =  showOct w ""

{-| Many binary strings can be embedded as-is in a HEREDOC, without escaping.
 -}
safeForHereDoc              ::  ByteString -> Bool
safeForHereDoc               =  not . Bytes.any (== 0x00)

{-| Predicate to determine whether data is represented as an encoded chunk or
    or is unencoded.
 -}
encoded                     ::  Chunk -> Bool
encoded (SafeChunk _)        =  False
encoded (EncodedChunk _ _ _ _) = True

data EscapeChar              =  EscapeChar !Word8      -- ^ ASCII value.
                                           !ByteString -- ^ Form for @tr@.
                                           !ByteString -- ^ Form for @sed@.
deriving instance Show EscapeChar

{-|  
 -}
script chunk                 =  mconcat $ case chunk of
  SafeChunk bytes           ->  [clip len, dataSection eof bytes]
   where
    len                      =  Bytes.length bytes
    eof                      =  blz (leastStringNotIn bytes)
  EncodedChunk bytes len (EscapeChar _ trN sedN) (EscapeChar b _ sedE) ->
    [ blz "{ ",       blaze tr,
      blz " | ",      blaze sed,
      blz " | ",      clip len,
      blz " ;}",      dataSection (Blaze.fromWord8 b) bytes ]
   where
    tr                       =  ["tr '", trN, "' '\\000'"]
    sed                      =  ["sed '","s|",sedE,sedE,"|",sedN,"|g", " ; ",
                                         "s|",sedE,"_", "|",sedE,"|g", "'"    ]
 where
  blaze                      =  mconcat . (Blaze.fromByteString <$>)
  blz                        =  Blaze.fromByteString
  nl                         =  Blaze.fromChar '\n'
  dataSection eof bytes      =  mconcat
    [blz " <<\\", eof, nl, blz bytes, nl, eof, nl]
  clip len                   =  blz "head -c " `mappend` Blaze.fromShow len

{-| Finds a short hexadecimal string that is not in the input.

    A string of length @n@ has at most @n - (k - 1)@ substrings of some fixed,
    positive length @k@ -- the substring starting at the first byte and
    extending for @k@, the substring starting at the second byte and extending
    for @k@ and so on, on until the end where we have to stop @k - 1@ short of
    the last byte. We choose @k@ such that it contains enough hexadecimal
    digits to enumerate all the substrings; for a 4M input, we want @k = 6@.

    We can take all the hex substrings of length @k@ in the input, sort them,
    and then find the gaps. We take the least substring in the first gap for
    our chosen substring. This gives us an O(n log n) algorithm.

    The measurable length of a 'ByteString' is at most the maximum 'Word'
    (since the length function results in an 'Int'); this is one less than 2
    to the bit width of a 'Word' (because there is a 0 'Word'). Thus a 'Word'
    suffices to enumerate all the possible substrings in a 'ByteString'; and
    one more. (Substrings are zero-indexed and the length is 1-indexed.) We
    can leverage this fact to translate all substrings to 'Word' and store
    them in an unboxed vector, using integer operations to find the least
    subtring in the first gap. Space usage is linear in the length of the
    input string; for a 4M string, the sorted vector could consume 32M on 64
    bit machines.
 -}
leastStringNotIn            ::  ByteString -> ByteString
leastStringNotIn bytes       =  hex
 where
  len                        =  Bytes.length bytes
  digits                     =  1 + floor (logBase 16 (fromIntegral len))
  substrings = [ s | s <- Bytes.take digits <$> Bytes.tails bytes, isHex s ]
  sortedWords               ::  Vector Word
  sortedWords                =  Vector.create $ do
    v                       <-  Vector.new len
    zipWithM_ (Vector.write v) [0..] (Bytes.hex <$> substrings)
    Vector.sort v
    return v
  isHex ""                   =  False
  isHex s                    =  Bytes.all (`Bytes.elem` "0123456789ABCDEF") s
  -- Find the smallest number not in the list, assuming it is sorted.
  minW                       =  f 0 (Vector.toList sortedWords)
   where
    f candidate l            =  case l of [ ]                 -> candidate
                                          h:t | candidate < h -> candidate
                                              | otherwise     -> f (h+1) t
  padded                     =  "0000000000000000" `mappend`
                                Data.ByteString.Char8.pack (showHex minW "")
  (_, hex) = Bytes.splitAt (Bytes.length padded - digits) padded


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


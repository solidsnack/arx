{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving  #-}
{-| The CLTokens module describes non-overlapping classes of strings that are
    useful for disambiguating arguments to command line programs. Many common
    string formats -- environment variable assignments, URLs, option strings --
    are recognized by this module's utilities.
 -}
module System.Posix.ARX.CLI.CLTokens where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Attoparsec.Char8 ( char8, choice, decimal, endOfInput, inClass,
                               isDigit, parseOnly, Parser, satisfy, string,
                               takeWhile, takeWhile1, try )


{-| Non-overlapping classes of command line argument strings.
 -}
data Class = EnvBinding    -- ^ An 'EnvBinding' has the form
                           --   @<shell var name>=<string>@. For example,
                           --   @SENDIN=the_clowns@.
           | QualifiedPath -- ^ A 'QualifiedPath' is a file path starting with
                           --   @/@, @./@, or @../@.
           | DashDash      -- ^ A 'DashDash' is a string of two dashes, @--@,
                           --   commonly used to indicate the end of options
                           --   processing.
           | LongOption    -- ^ A 'LongOption' is a string beginning with two
                           --   dashes and then at least one non-dash.
           | Dash          -- ^ A 'Dash' is a single dash, @-@, commonly used
                           --   to indicate input from @stdin@ or output to
                           --   @stdout@.
           | ShortOption   -- ^ A 'ShortOption' is a beginning with a dash and
                           --   then at least one non-dash.
           | URL           -- ^ A 'URL' is a scheme, separated from the
                           --   resource, represented as an arbitrary string,
                           --   by @://@. The scheme consists of ASCII,
                           --   lower-case letters and digits, and may be
                           --   multi-part, with each part separated by a @+@
                           --   or @/@ (for example, @git+ssh@). An example
                           --   URL: @http://example.com/?q=special@.
           | HexNum        -- ^ A 'HexNum' is a sequence of hexadecimal
                           --   digits, upper or lower case, beginning with
                           --   @0x@; for example: @0x01a3@.
           | DecimalNum    -- ^ A 'DecimalNum' is a string of decimal digits:
                           --   @123123@.
           | Size          -- ^ A 'Size' is a decimal number followed by a
                           --   multiplicative suffix, in the manner of @dd@
                           --   or @head@. Note that counts in terms of bytes
                           --   require @B@ (unlike @dd@ or @head@). For a
                           --   full list of suffixes, see 'sizes' below.
deriving instance Eq Class
deriving instance Ord Class
deriving instance Show Class


{-| Determine if a particular 'ByteString' matches the given 'Class' of token.
 -}
match                       ::  Class -> ByteString -> Bool
match                        =  (e2b .) . parseOnly . recognizer
 where
  e2b (Left _)               =  False
  e2b (Right _)              =  True


{-| Determine if a particular 'ByteString' matches any 'Class' of token.
 -}
recognize                   ::  ByteString -> Maybe Class
recognize                    =  e2m . parseOnly (choice recognizers)
 where
  e2m (Left _)               =  Nothing
  e2m (Right x)              =  Just x
  recognizeIt x              =  x <$ recognizer x
  recognizers                =  recognizeIt <$> [ EnvBinding,
                                                  QualifiedPath,
                                                  DashDash,
                                                  LongOption,
                                                  Dash,
                                                  ShortOption,
                                                  URL,
                                                  HexNum,
                                                  DecimalNum ]


{-| A ByteString stand-in that demoes each token class.
 -}
exemplar                    ::  Class -> ByteString
exemplar cls                 =  case cls of
  EnvBinding                ->  "VAR=value"
  QualifiedPath             ->  "./qualified/path"
  DashDash                  ->  "--"
  LongOption                ->  "--long-option"
  Dash                      ->  "-"
  ShortOption               ->  "-shortopt"
  URL                       ->  "scheme://url-to-resource"
  HexNum                    ->  "0xA12FE"
  DecimalNum                ->  "0123456789"
  Size                      ->  "4MiB"


{-| The recognizer appropriate to each token class. Parses successfully if a
    the token class is recognized, returning '()'. Most token types are
    defined in terms of a prefix of the input -- for example, 'QualifiedPath'
    -- and the parsers for these tokens naturally return as soon as the prefix
    is recognized.
 -}
recognizer                  ::  Class -> Parser ()
recognizer cls               =  case cls of
  EnvBinding                ->  () <$ do satisfy varFirst
                                         takeWhile varBody
                                         char8 '='
  QualifiedPath             ->  () <$ do string "/" <|> string "./"
                                                    <|> string "../"
  DashDash                  ->  string "--" *> endOfInput
  LongOption                ->  () <$ (string "--" >> satisfy (/= '-'))
  Dash                      ->  char8 '-' *> endOfInput
  ShortOption               ->  () <$ (char8 '-' >> satisfy (/= '-'))
  URL                       ->  () <$ do takeWhile1 isURLSchemeChar
                                         many $ do char8 '+' <|> char8 '/'
                                                   takeWhile1 isURLSchemeChar
                                         string "://"
  HexNum                    ->  string "0x" >> takeWhile1 isHexDigit
                                            *> endOfInput
  DecimalNum                ->  takeWhile1 isDigit *> endOfInput
  Size                      ->  () <$ size

schemeSeparator              =  char8 '+' <|> char8 '/'

varFirst                     =  inClass "a-zA-Z_"

varBody                      =  inClass "a-zA-Z_0-9"

isLongOptionChar             =  inClass "a-zA-Z0-9-"

isShortOptionChar            =  inClass "a-zA-Z0-9!?"

isSchemeChar                 =  inClass "a-z0-9"

isHexDigit                   =  inClass "0-9a-fA-F"

isURLSchemeChar              =  inClass "a-z0-9"


{-| A map from suffixes to sizes, following the conventions of command line
    tools (GNU @dd@ or @head@ and many others) as well as the standard for
    binary sizes established by the IEC.
@
  B       =    1
  K = KiB = 1024B   kB = 1000B
  M = MiB = 1024K   MB = 1000kB
  G = GiB = 1024M   GB = 1000MB
  T = TiB = 1024G   TB = 1000GB
  P = PiB = 1024T   PB = 1000TB
  E = EiB = 1024P   EB = 1000PB
  Z = ZiB = 1024E   ZB = 1000EB
  Y = YiB = 1024Z   YB = 1000ZB
@
 -}
sizes                       ::  Map ByteString Integer
sizes                        =  Map.fromList
                                 [ ("B", 1),
                                   ("K", 2^10), ("KiB", 2^10), ("kB", 10^03),
                                   ("M", 2^20), ("MiB", 2^20), ("MB", 10^06),
                                   ("G", 2^30), ("GiB", 2^30), ("GB", 10^09),
                                   ("T", 2^40), ("TiB", 2^40), ("TB", 10^12),
                                   ("P", 2^50), ("PiB", 2^50), ("PB", 10^15),
                                   ("E", 2^60), ("EiB", 2^60), ("EB", 10^18),
                                   ("Z", 2^70), ("ZiB", 2^70), ("ZB", 10^21),
                                   ("Y", 2^80), ("YiB", 2^80), ("YB", 10^24) ]

{-| Parse a size, consuming the entire input string.
 -}
size                        ::  Parser Integer
size                         =  (*) <$> decimal <*> suffix
 where
  asSuffix (k, v)            =  v <$ try (string k <* endOfInput)
  suffix                     =  choice (asSuffix <$> Map.toList sizes)

{-| Parse a size, consuming the entire input string, with the final result
    bounded by the maximum of a 'Bounded' type.
 -}
sizeBounded :: forall b . (Bounded b, Integral b) => Parser b
sizeBounded = fromInteger . min (toInteger (maxBound :: b)) <$> size


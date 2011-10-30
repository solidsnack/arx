{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}
{-| The CLTokens module describes non-overlapping classes of strings that are
    useful for disambiguating arguments to command line programs. Many common
    string formats -- environment variable assignments, URLs, option strings --
    are recognized by this module's utilities.
 -}
module System.Posix.ARX.CLTokens where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

import Data.Attoparsec.Char8
import Data.Attoparsec.FastSet


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
                           --   URL: @http://example.com/?q=special
           | HexNum        -- ^ A 'HexNum' is a sequence of hexadecimal
                           --   digits, upper or lower case, beginning with
                           --   @0x@; for example: @0x01a3@.
           | DecimalNum    -- ^ A 'DecimalNum' is a string of decimal digits:
                           --   @123123@.
deriving instance Eq Class
deriving instance Ord Class
deriving instance Show Class


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

{-| The recognizer appropriate to each token class. Parses successfully if a
    the token class is recognized, returning '()'. Most token types are
    defined in terms of a prefix of the input -- for example, 'QualifiedPath'
    -- and the parsers for these tokens naturally return as soon as the prefix
    is recognized.
 -}
recognizer                  ::  Class -> Parser ()
recognizer EnvBinding        =  do satisfy varFirst
                                   takeWhile varBody
                                   char8 '='
                                   return ()
recognizer QualifiedPath     =  do string "/" <|> string "./" <|> string "../"
                                   return ()
recognizer DashDash          =  do string "--"
                                   endOfInput
recognizer LongOption        =  do string "--"
                                   satisfy (/= '-')
                                   return ()
recognizer Dash              =  do char8 '-'
                                   endOfInput
recognizer ShortOption       =  do char8 '-'
                                   satisfy (/= '-')
                                   return ()
recognizer URL               =  do takeWhile1 isURLSchemeChar
                                   many $ do char8 '+' <|> char8 '/'
                                             takeWhile1 isURLSchemeChar
                                   string "://"
                                   return ()
recognizer HexNum            =  do string "0x"
                                   takeWhile1 isHexDigit
                                   endOfInput
recognizer DecimalNum        =  do takeWhile1 isDigit
                                   endOfInput

schemeSeparator              =  char8 '+' <|> char8 '/'

varFirst                     =  inClass "a-zA-Z_"

varBody                      =  inClass "a-zA-Z_0-9"

isLongOptionChar             =  inClass "a-zA-Z0-9-"

isShortOptionChar            =  inClass "a-zA-Z0-9!?"

isSchemeChar                 =  inClass "a-z0-9"

isHexDigit                   =  inClass "0-9a-fA-F"

isURLSchemeChar              =  inClass "a-z0-9"


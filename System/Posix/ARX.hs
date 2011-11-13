module System.Posix.ARX (
    -- * Interface and implementation of subcommands.
    ARX(..), SHDAT(..), TMPX(..),
    -- * Creation of environment bindings for 'TMPX'.
    Val, val, Var, var,
    -- * Tar archive types and magic detection.
    Tar(..), magic
  ) where

import System.Posix.ARX.Programs
import System.Posix.ARX.Sh
import System.Posix.ARX.Tar


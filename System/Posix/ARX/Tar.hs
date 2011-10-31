
module System.Posix.ARX.Tar where


{-| Handled styles of Tar archive.
 -}
data Tar                     =  TAR | TGZ | TBZ
deriving instance Eq Tar
deriving instance Ord Tar
deriving instance Show Tar


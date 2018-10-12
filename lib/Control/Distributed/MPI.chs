{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

#include <mpi.h>
#include <mpihs.h>

module Control.Distributed.MPI
  ( Comm(..)
  , ComparisonResult(..)
  , Datatype(..)
  , Rank(..)
  , fromRank
  , rootRank
  , toRank
  , Request(..)
  , Status(..)
  --, statusError
  , statusSource
  , statusTag
  , Tag(..)
  , fromTag
  , toTag
  , unitTag
  , ThreadSupport(..)

  , commSelf
  , commWorld
  , byte
  , char
  , double
  , float
  , int
  , long
  , longDouble
  , longLongInt
  , short
  , unsigned
  , unsignedChar
  , unsignedLong
  , unsignedShort
  , anySource
  , anyTag

  , barrier
  , bcast
  , commCompare
  , commRank
  , commSize
  , finalize
  , finalized
  , getCount
  , getLibraryVersion
  , getProcessorName
  , getVersion
  , ibarrier
  , ibcast
  , init
  , initThread
  , initialized
  , iprobe
  , irecv
  , isend
  , probe
  , recv
  , send
  , test
  , wait
  ) where

import Prelude hiding (fromEnum, init, toEnum)
import qualified Prelude

import Control.Monad (liftM)
import Data.Coerce
import Data.Ix
import Data.Version
import Foreign (Ptr, Storable(..), alloca, castPtr, toBool)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import GHC.Arr (indexError)
import System.IO.Unsafe (unsafePerformIO)

default (Int)

{#context prefix = "MPI" #}



--------------------------------------------------------------------------------

-- Arguments

fromEnum :: (Enum e, Integral i) => e -> i
fromEnum  = fromIntegral . Prelude.fromEnum

toEnum :: (Integral i, Enum e) => i -> e
toEnum  = Prelude.toEnum . fromIntegral

-- Return values

bool2maybe :: (Bool, a) -> Maybe a
bool2maybe (False, _) = Nothing
bool2maybe (True, x) = Just x

discard :: a -> IO ()
discard _ = return ()

-- a Bool, probably represented as CInt
peekBool :: (Integral a, Storable a) => Ptr a -> IO Bool
peekBool  = liftM toBool . peek

-- a (Ptr a) argument, probably represented either as (Ptr a) or (Ptr ())
peekCast :: Storable b => Ptr a -> IO b
peekCast ptr = peek (castPtr ptr :: Ptr b)

-- a type that we wrapped, e.g. CInt and Rank
peekCoerce :: (Storable a, Coercible a b) => Ptr a -> IO b
peekCoerce = liftM coerce . peek

peekEnum :: (Integral i, Storable i, Enum e) => Ptr i -> IO e
peekEnum = liftM toEnum . peek

peekInt :: (Integral i, Storable i) => Ptr i -> IO Int
peekInt = liftM fromIntegral . peek



--------------------------------------------------------------------------------

type MPIComm = {#type MPI_Comm #}
newtype Comm = Comm { getComm :: MPIComm } deriving (Eq, Ord, Show)



{#enum ComparisonResult {underscoreToCase} deriving (Eq, Ord, Read, Show) #}



type MPIDatatype = {#type MPI_Datatype #}
newtype Datatype = Datatype { getDatatype :: MPIDatatype }
  deriving (Eq, Ord, Show)



newtype Rank = Rank { getRank :: CInt }
  deriving (Eq, Ord, Read, Show, Num, Storable)

instance Ix Rank where
  range (Rank rmin, Rank rmax) = Rank <$> [rmin..rmax]
  {-# INLINE index #-}
  index b@(Rank rmin, _) i@(Rank r)
    | inRange b i = fromIntegral (r - rmin)
    | otherwise   = indexError b i "MPI.Rank"
  inRange (Rank rmin, Rank rmax) (Rank r) = rmin <= r && r <= rmax

toRank :: Enum e => e -> Rank
toRank = Rank . fromIntegral . fromEnum

fromRank :: Enum e => Rank -> e
fromRank = toEnum . fromIntegral . getRank

rootRank :: Rank
rootRank = toRank 0



type MPIRequest = {#type MPI_Request #}
newtype Request = Request { getRequest :: MPIRequest }
  deriving (Eq, Ord, Show, Storable)



type MPIStatus = {#type MPI_Status #}
newtype Status = Status { getStatus :: MPIStatus }
  deriving (Eq, Ord, Show, Storable)

-- statusError :: Status -> Error
-- statusError (Status mst) =
--   toError $ unsafePerformIO ({#get MPI_Status->MPI_ERROR #} mst)

statusSource :: Status -> Rank
statusSource (Status mst) =
  toRank $ unsafePerformIO ({#get MPI_Status->MPI_SOURCE #} mst)

statusTag :: Status -> Tag
statusTag (Status mst) =
  toTag $ unsafePerformIO ({#get MPI_Status->MPI_TAG #} mst)



newtype Tag = Tag { getTag :: CInt }
  deriving (Eq, Ord, Read, Show, Num, Storable)

toTag :: Enum e => e -> Tag
toTag = Tag . fromIntegral . fromEnum

fromTag :: Enum e => Tag -> e
fromTag = toEnum . fromIntegral . getTag

unitTag :: Tag
unitTag = toTag ()



{#enum ThreadSupport {underscoreToCase} deriving (Eq, Ord, Read, Show) #}



--------------------------------------------------------------------------------

foreign import ccall "&mpi_comm_self" mpiCommSelf :: Ptr MPIComm
commSelf :: Comm
commSelf = Comm <$> unsafePerformIO $ peek mpiCommSelf

foreign import ccall "&mpi_comm_world" mpiCommWorld :: Ptr MPIComm
commWorld :: Comm
commWorld = Comm <$> unsafePerformIO $ peek mpiCommWorld



foreign import ccall "&mpi_byte" mpiByte :: Ptr MPIDatatype
byte :: Datatype
byte = Datatype <$> unsafePerformIO $ peek mpiByte

foreign import ccall "&mpi_char" mpiChar :: Ptr MPIDatatype
char :: Datatype
char = Datatype <$> unsafePerformIO $ peek mpiChar

foreign import ccall "&mpi_double" mpiDouble :: Ptr MPIDatatype
double :: Datatype
double = Datatype <$> unsafePerformIO $ peek mpiDouble

foreign import ccall "&mpi_float" mpiFloat :: Ptr MPIDatatype
float :: Datatype
float = Datatype <$> unsafePerformIO $ peek mpiFloat

foreign import ccall "&mpi_int" mpiInt :: Ptr MPIDatatype
int :: Datatype
int = Datatype <$> unsafePerformIO $ peek mpiInt

foreign import ccall "&mpi_long" mpiLong :: Ptr MPIDatatype
long :: Datatype
long = Datatype <$> unsafePerformIO $ peek mpiLong

foreign import ccall "&mpi_long_double" mpiLongDouble :: Ptr MPIDatatype
longDouble :: Datatype
longDouble = Datatype <$> unsafePerformIO $ peek mpiLongDouble

foreign import ccall "&mpi_long_long_int" mpiLongLong_Int :: Ptr MPIDatatype
longLongInt :: Datatype
longLongInt = Datatype <$> unsafePerformIO $ peek mpiLongLong_Int

foreign import ccall "&mpi_short" mpiShort :: Ptr MPIDatatype
short :: Datatype
short = Datatype <$> unsafePerformIO $ peek mpiShort

foreign import ccall "&mpi_unsigned" mpiUnsigned :: Ptr MPIDatatype
unsigned :: Datatype
unsigned = Datatype <$> unsafePerformIO $ peek mpiUnsigned

foreign import ccall "&mpi_unsigned_char" mpiUnsignedChar :: Ptr MPIDatatype
unsignedChar :: Datatype
unsignedChar = Datatype <$> unsafePerformIO $ peek mpiUnsignedChar

foreign import ccall "&mpi_unsigned_long" mpiUnsignedLong :: Ptr MPIDatatype
unsignedLong :: Datatype
unsignedLong = Datatype <$> unsafePerformIO $ peek mpiUnsignedLong

foreign import ccall "&mpi_unsigned_short" mpiUnsignedShort :: Ptr MPIDatatype
unsignedShort :: Datatype
unsignedShort = Datatype <$> unsafePerformIO $ peek mpiUnsignedShort



foreign import ccall "&mpi_any_source" mpiAnySource :: Ptr CInt
anySource :: Rank
anySource = Rank <$> unsafePerformIO $ peek mpiAnySource



foreign import ccall "&mpi_any_tag" mpiAnyTag :: Ptr CInt
anyTag :: Tag
anyTag = Tag <$> unsafePerformIO $ peek mpiAnyTag



--------------------------------------------------------------------------------

{#fun unsafe Barrier as ^ {getComm `Comm'} -> `()' discard*- #}

{#fun unsafe Bcast as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getComm `Comm'
    } -> `()' discard*- #}

{#fun unsafe Comm_compare as ^
    { getComm `Comm'
    , getComm `Comm'
    , alloca- `ComparisonResult' peekEnum*
    } -> `()' discard*- #}

{#fun unsafe Comm_rank as ^
    { getComm `Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' discard*- #}

{#fun unsafe Comm_size as ^
    { getComm `Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' discard*- #}

{#fun unsafe Finalize as ^ {} -> `()' discard*- #}

{#fun unsafe Finalized as ^ {alloca- `Bool' peekBool*} -> `()' discard*- #}

{#fun unsafe Get_count as ^
    { getStatus `Status'
    , getDatatype `Datatype'
    , alloca- `Int' peekInt*
    } -> `()' discard*- #}

{#fun unsafe Get_library_version as getLibraryVersion_
    { id `CString'
    , alloca- `Int' peekInt*
    } -> `()' discard*- #}

getLibraryVersion :: IO String
getLibraryVersion =
  do buf <- mallocArray {#const MPI_MAX_LIBRARY_VERSION_STRING #}
     len <- getLibraryVersion_ buf
     str <- peekCStringLen (buf, len)
     return str

{#fun unsafe Get_processor_name as getProcessorName_
    { id `CString'
    , alloca- `Int' peekInt*
    } -> `()' discard*- #}

getProcessorName :: IO String
getProcessorName =
  do buf <- mallocArray {#const MPI_MAX_PROCESSOR_NAME #}
     len <- getProcessorName_ buf
     str <- peekCStringLen (buf, len)
     return str

{#fun unsafe Get_version as getVersion_
    { alloca- `Int' peekInt*
    , alloca- `Int' peekInt*
    } -> `()' discard*- #}

getVersion :: IO Version
getVersion =
  do (major, minor) <- getVersion_
     return (makeVersion [major, minor])

{#fun unsafe Ibarrier as ^
    { getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' discard*- #}

{#fun unsafe Ibcast as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' discard*- #}

{#fun unsafe Initialized as ^ {alloca- `Bool' peekBool*} -> `()' discard*- #}

{#fun unsafe mpihs_init as init {} -> `()' discard*- #}

{#fun unsafe mpihs_init_thread as initThread
    { fromEnum `ThreadSupport'
    , alloca- `ThreadSupport' peekEnum*
    } -> `()' discard*- #}

{#fun unsafe Iprobe as iprobe_
    { getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Bool' peekBool*
    , alloca- `Status' peekCast*
    } -> `()' discard*- #}

iprobe :: Rank -> Tag -> Comm -> IO (Maybe Status)
iprobe rank tag comm = bool2maybe <$> iprobe_ rank tag comm

{#fun unsafe Irecv as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' discard*- #}

{#fun unsafe Isend as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' discard*- #}

{#fun unsafe Recv as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Status' peekCast*
    } -> `()' discard*- #}

{#fun unsafe Probe as ^
    { getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Status' peekCast*
    } -> `()' discard*- #}

{#fun unsafe Send as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    } -> `()' discard*- #}

{#fun unsafe Wait as ^
    { castPtr `Ptr Request'
    , alloca- `Status' peekCast*
    } -> `()' discard*- #}

{#fun unsafe Test as test_
    { castPtr `Ptr Request'
    , alloca- `Bool' peekBool*
    , alloca- `Status' peekCast*
    } -> `()' discard*- #}

test :: Ptr Request -> IO (Maybe Status)
test req = bool2maybe <$> test_ req

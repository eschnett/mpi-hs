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
  , Op(..)
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
  -- TODO: use a module for this namespace
  , datatypeByte
  , datatypeChar
  , datatypeDouble
  , datatypeFloat
  , datatypeInt
  , datatypeLong
  , datatypeLongDouble
  , datatypeLongLongInt
  , datatypeShort
  , datatypeUnsigned
  , datatypeUnsignedChar
  , datatypeUnsignedLong
  , datatypeUnsignedShort
  -- TODO: use a module for this namespace
  , opBand
  , opBor
  , opBxor
  , opLand
  , opLor
  , opLxor
  , opMax
  , opMaxloc
  , opMin
  , opMinloc
  , opProd
  , opSum
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
  , ireduce
  , isend
  , probe
  , recv
  , reduce
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

newtype Comm = Comm { getComm :: {#type MPI_Comm #} }
  deriving (Eq, Ord, Show, Storable)



{#enum ComparisonResult {underscoreToCase} deriving (Eq, Ord, Read, Show) #}



newtype Datatype = Datatype { getDatatype :: {#type MPI_Datatype #} }
  deriving (Eq, Ord, Show, Storable)



newtype Op = Op { getOp :: {#type MPI_Op #} }
  deriving (Eq, Ord, Show, Storable)



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



newtype Request = Request { getRequest :: {#type MPI_Request #} }
  deriving (Eq, Ord, Show, Storable)



newtype Status = Status { getStatus :: {#type MPI_Status #} }
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

foreign import ccall "&mpi_comm_self" mpiCommSelf :: Ptr Comm
commSelf :: Comm
commSelf = unsafePerformIO $ peek mpiCommSelf

foreign import ccall "&mpi_comm_world" mpiCommWorld :: Ptr Comm
commWorld :: Comm
commWorld = unsafePerformIO $ peek mpiCommWorld



foreign import ccall "&mpi_byte" mpiByte :: Ptr Datatype
datatypeByte :: Datatype
datatypeByte = unsafePerformIO $ peek mpiByte

foreign import ccall "&mpi_char" mpiChar :: Ptr Datatype
datatypeChar :: Datatype
datatypeChar = unsafePerformIO $ peek mpiChar

foreign import ccall "&mpi_double" mpiDouble :: Ptr Datatype
datatypeDouble :: Datatype
datatypeDouble = unsafePerformIO $ peek mpiDouble

foreign import ccall "&mpi_float" mpiFloat :: Ptr Datatype
datatypeFloat :: Datatype
datatypeFloat = unsafePerformIO $ peek mpiFloat

foreign import ccall "&mpi_int" mpiInt :: Ptr Datatype
datatypeInt :: Datatype
datatypeInt = unsafePerformIO $ peek mpiInt

foreign import ccall "&mpi_long" mpiLong :: Ptr Datatype
datatypeLong :: Datatype
datatypeLong = unsafePerformIO $ peek mpiLong

foreign import ccall "&mpi_long_double" mpiLongDouble :: Ptr Datatype
datatypeLongDouble :: Datatype
datatypeLongDouble = unsafePerformIO $ peek mpiLongDouble

foreign import ccall "&mpi_long_long_int" mpiLongLong_Int :: Ptr Datatype
datatypeLongLongInt :: Datatype
datatypeLongLongInt = unsafePerformIO $ peek mpiLongLong_Int

foreign import ccall "&mpi_short" mpiShort :: Ptr Datatype
datatypeShort :: Datatype
datatypeShort = unsafePerformIO $ peek mpiShort

foreign import ccall "&mpi_unsigned" mpiUnsigned :: Ptr Datatype
datatypeUnsigned :: Datatype
datatypeUnsigned = unsafePerformIO $ peek mpiUnsigned

foreign import ccall "&mpi_unsigned_char" mpiUnsignedChar :: Ptr Datatype
datatypeUnsignedChar :: Datatype
datatypeUnsignedChar = unsafePerformIO $ peek mpiUnsignedChar

foreign import ccall "&mpi_unsigned_long" mpiUnsignedLong :: Ptr Datatype
datatypeUnsignedLong :: Datatype
datatypeUnsignedLong = unsafePerformIO $ peek mpiUnsignedLong

foreign import ccall "&mpi_unsigned_short" mpiUnsignedShort :: Ptr Datatype
datatypeUnsignedShort :: Datatype
datatypeUnsignedShort = unsafePerformIO $ peek mpiUnsignedShort



foreign import ccall "&mpi_band" mpiBand :: Ptr Op
opBand :: Op
opBand = unsafePerformIO $ peek mpiBand

foreign import ccall "&mpi_bor" mpiBor :: Ptr Op
opBor :: Op
opBor = unsafePerformIO $ peek mpiBor

foreign import ccall "&mpi_bxor" mpiBxor :: Ptr Op
opBxor :: Op
opBxor = unsafePerformIO $ peek mpiBxor

foreign import ccall "&mpi_land" mpiLand :: Ptr Op
opLand :: Op
opLand = unsafePerformIO $ peek mpiLand

foreign import ccall "&mpi_lor" mpiLor :: Ptr Op
opLor :: Op
opLor = unsafePerformIO $ peek mpiLor

foreign import ccall "&mpi_lxor" mpiLxor :: Ptr Op
opLxor :: Op
opLxor = unsafePerformIO $ peek mpiLxor

foreign import ccall "&mpi_max" mpiMax :: Ptr Op
opMax :: Op
opMax = unsafePerformIO $ peek mpiMax

foreign import ccall "&mpi_maxloc" mpiMaxloc :: Ptr Op
opMaxloc :: Op
opMaxloc = unsafePerformIO $ peek mpiMaxloc

foreign import ccall "&mpi_min" mpiMin :: Ptr Op
opMin :: Op
opMin = unsafePerformIO $ peek mpiMin

foreign import ccall "&mpi_minloc" mpiMinloc :: Ptr Op
opMinloc :: Op
opMinloc = unsafePerformIO $ peek mpiMinloc

foreign import ccall "&mpi_prod" mpiProd :: Ptr Op
opProd :: Op
opProd = unsafePerformIO $ peek mpiProd

foreign import ccall "&mpi_sum" mpiSum :: Ptr Op
opSum :: Op
opSum = unsafePerformIO $ peek mpiSum



foreign import ccall "&mpi_any_source" mpiAnySource :: Ptr Rank
anySource :: Rank
anySource = unsafePerformIO $ peek mpiAnySource



foreign import ccall "&mpi_any_tag" mpiAnyTag :: Ptr Tag
anyTag :: Tag
anyTag = unsafePerformIO $ peek mpiAnyTag



--------------------------------------------------------------------------------

{#fun unsafe Barrier as ^ {getComm `Comm'} -> `()' return*- #}

{#fun unsafe Bcast as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getComm `Comm'
    } -> `()' return*- #}

{#fun unsafe Comm_compare as ^
    { getComm `Comm'
    , getComm `Comm'
    , alloca- `ComparisonResult' peekEnum*
    } -> `()' return*- #}

{#fun unsafe Comm_rank as ^
    { getComm `Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' return*- #}

{#fun unsafe Comm_size as ^
    { getComm `Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' return*- #}

{#fun unsafe Finalize as ^ {} -> `()' return*- #}

{#fun unsafe Finalized as ^ {alloca- `Bool' peekBool*} -> `()' return*- #}

{#fun unsafe Get_count as ^
    { getStatus `Status'
    , getDatatype `Datatype'
    , alloca- `Int' peekInt*
    } -> `()' return*- #}

{#fun unsafe Get_library_version as getLibraryVersion_
    { id `CString'
    , alloca- `Int' peekInt*
    } -> `()' return*- #}

getLibraryVersion :: IO String
getLibraryVersion =
  do buf <- mallocArray {#const MPI_MAX_LIBRARY_VERSION_STRING #}
     len <- getLibraryVersion_ buf
     str <- peekCStringLen (buf, len)
     return str

{#fun unsafe Get_processor_name as getProcessorName_
    { id `CString'
    , alloca- `Int' peekInt*
    } -> `()' return*- #}

getProcessorName :: IO String
getProcessorName =
  do buf <- mallocArray {#const MPI_MAX_PROCESSOR_NAME #}
     len <- getProcessorName_ buf
     str <- peekCStringLen (buf, len)
     return str

{#fun unsafe Get_version as getVersion_
    { alloca- `Int' peekInt*
    , alloca- `Int' peekInt*
    } -> `()' return*- #}

getVersion :: IO Version
getVersion =
  do (major, minor) <- getVersion_
     return (makeVersion [major, minor])

{#fun unsafe Ibarrier as ^
    { getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' return*- #}

{#fun unsafe Ibcast as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' return*- #}

{#fun unsafe Initialized as ^ {alloca- `Bool' peekBool*} -> `()' return*- #}

{#fun unsafe mpihs_init as init {} -> `()' return*- #}

{#fun unsafe mpihs_init_thread as initThread
    { fromEnum `ThreadSupport'
    , alloca- `ThreadSupport' peekEnum*
    } -> `()' return*- #}

{#fun unsafe Iprobe as iprobe_
    { getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Bool' peekBool*
    , alloca- `Status' peekCast*
    } -> `()' return*- #}

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
    } -> `()' return*- #}

{#fun unsafe Ireduce as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getRank `Rank'
    , getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' return*- #}

{#fun unsafe Isend as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Request' peekCast*
    } -> `()' return*- #}

{#fun unsafe Probe as ^
    { getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Status' peekCast*
    } -> `()' return*- #}

{#fun unsafe Recv as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , alloca- `Status' peekCast*
    } -> `()' return*- #}

{#fun unsafe Reduce as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getRank `Rank'
    , getComm `Comm'
    } -> `()' return*- #}

{#fun unsafe Send as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    } -> `()' return*- #}

{#fun unsafe Wait as ^
    { castPtr `Ptr Request'
    , alloca- `Status' peekCast*
    } -> `()' return*- #}

{#fun unsafe Test as test_
    { castPtr `Ptr Request'
    , alloca- `Bool' peekBool*
    , alloca- `Status' peekCast*
    } -> `()' return*- #}

test :: Ptr Request -> IO (Maybe Status)
test req = bool2maybe <$> test_ req

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
  , HasDatatype(..)
  , datatypeOf
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
  , HasOp(..)
  , anySource
  , anyTag

  , abort
  , allgather
  , allreduce
  , alltoall
  , barrier
  , bcast
  , commCompare
  , commRank
  , commSize
  , exscan
  , finalize
  , finalized
  , gather
  , getCount
  , getLibraryVersion
  , getProcessorName
  , getVersion
  , iallgather
  , iallreduce
  , ialltoall
  , ibarrier
  , ibcast
  , iexscan
  , igather
  , init
  , initThread
  , initialized
  , iprobe
  , irecv
  , ireduce
  , iscan
  , iscatter
  , isend
  , probe
  , recv
  , reduce
  , scan
  , scatter
  , send
  , sendrecv
  , test
  , wait
  ) where

import Prelude hiding (fromEnum, fst, init, toEnum)
import qualified Prelude

import Control.Monad (liftM)
import Data.Coerce
import Data.Ix
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Data.Version
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Arr (indexError)
import System.IO.Unsafe (unsafePerformIO)

default (Int)

{#context prefix = "MPI"#}



--------------------------------------------------------------------------------

-- See GHC's includes/rts/Flags.h
foreign import ccall "&rts_argc" rtsArgc :: Ptr CInt
foreign import ccall "&rts_argv" rtsArgv :: Ptr (Ptr CString)
argc :: CInt
argv :: Ptr CString
argc = unsafePerformIO $ peek rtsArgc
argv = unsafePerformIO $ peek rtsArgv



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

-- a type that we wrapped, e.g. CInt and Rank
peekCoerce :: (Storable a, Coercible a b) => Ptr a -> IO b
peekCoerce = liftM coerce . peek

peekEnum :: (Integral i, Storable i, Enum e) => Ptr i -> IO e
peekEnum = liftM toEnum . peek

peekInt :: (Integral i, Storable i) => Ptr i -> IO Int
peekInt = liftM fromIntegral . peek



--------------------------------------------------------------------------------

newtype Comm = Comm { getComm :: {#type MPI_Comm#} }
  deriving (Eq, Ord, Show, Storable)



{#enum ComparisonResult {underscoreToCase} deriving (Eq, Ord, Read, Show)#}



newtype Datatype = Datatype { getDatatype :: {#type MPI_Datatype#} }
  deriving (Eq, Ord, Show, Storable)



newtype Op = Op { getOp :: {#type MPI_Op#} }
  deriving (Eq, Ord, Show, Storable)



newtype Rank = Rank { getRank :: CInt }
  deriving (Eq, Ord, Num, Storable)

instance Read Rank where
  readsPrec p = map (\(r, s) -> (Rank r, s)) . readsPrec p

instance Show Rank where
  showsPrec p = showsPrec p . getRank

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



{#pointer *MPI_Request as Request foreign newtype#}

deriving instance Eq Request
deriving instance Ord Request
deriving instance Show Request



{#pointer *MPI_Status as Status foreign newtype#}

deriving instance Eq Status
deriving instance Ord Status
deriving instance Show Status

-- statusError :: Status -> IO Error
-- statusError (Status mst) =
--   Error $ {#get MPI_Status.MPI_ERROR#} mst

statusSource :: Status -> IO Rank
statusSource (Status fst) =
  withForeignPtr fst (\pst -> Rank <$> {#get MPI_Status->MPI_SOURCE#} pst)

statusTag :: Status -> IO Tag
statusTag (Status fst) =
  withForeignPtr fst (\pst -> Tag <$> {#get MPI_Status->MPI_TAG#} pst)



newtype Tag = Tag { getTag :: CInt }
  deriving (Eq, Ord, Read, Show, Num, Storable)

toTag :: Enum e => e -> Tag
toTag = Tag . fromIntegral . fromEnum

fromTag :: Enum e => Tag -> e
fromTag = toEnum . fromIntegral . getTag

unitTag :: Tag
unitTag = toTag ()



{#enum ThreadSupport {underscoreToCase} deriving (Eq, Ord, Read, Show)#}



--------------------------------------------------------------------------------

foreign import ccall "&hsmpi_comm_self" mpiCommSelf :: Ptr Comm
commSelf :: Comm
commSelf = unsafePerformIO $ peek mpiCommSelf

foreign import ccall "&hsmpi_comm_world" mpiCommWorld :: Ptr Comm
commWorld :: Comm
commWorld = unsafePerformIO $ peek mpiCommWorld



foreign import ccall "&hsmpi_byte" mpiByte :: Ptr Datatype
datatypeByte :: Datatype
datatypeByte = unsafePerformIO $ peek mpiByte

foreign import ccall "&hsmpi_char" mpiChar :: Ptr Datatype
datatypeChar :: Datatype
datatypeChar = unsafePerformIO $ peek mpiChar

foreign import ccall "&hsmpi_double" mpiDouble :: Ptr Datatype
datatypeDouble :: Datatype
datatypeDouble = unsafePerformIO $ peek mpiDouble

foreign import ccall "&hsmpi_float" mpiFloat :: Ptr Datatype
datatypeFloat :: Datatype
datatypeFloat = unsafePerformIO $ peek mpiFloat

foreign import ccall "&hsmpi_int" mpiInt :: Ptr Datatype
datatypeInt :: Datatype
datatypeInt = unsafePerformIO $ peek mpiInt

foreign import ccall "&hsmpi_long" mpiLong :: Ptr Datatype
datatypeLong :: Datatype
datatypeLong = unsafePerformIO $ peek mpiLong

foreign import ccall "&hsmpi_long_double" mpiLongDouble :: Ptr Datatype
datatypeLongDouble :: Datatype
datatypeLongDouble = unsafePerformIO $ peek mpiLongDouble

foreign import ccall "&hsmpi_long_long_int" mpiLongLong_Int :: Ptr Datatype
datatypeLongLongInt :: Datatype
datatypeLongLongInt = unsafePerformIO $ peek mpiLongLong_Int

foreign import ccall "&hsmpi_short" mpiShort :: Ptr Datatype
datatypeShort :: Datatype
datatypeShort = unsafePerformIO $ peek mpiShort

foreign import ccall "&hsmpi_unsigned" mpiUnsigned :: Ptr Datatype
datatypeUnsigned :: Datatype
datatypeUnsigned = unsafePerformIO $ peek mpiUnsigned

foreign import ccall "&hsmpi_unsigned_char" mpiUnsignedChar :: Ptr Datatype
datatypeUnsignedChar :: Datatype
datatypeUnsignedChar = unsafePerformIO $ peek mpiUnsignedChar

foreign import ccall "&hsmpi_unsigned_long" mpiUnsignedLong :: Ptr Datatype
datatypeUnsignedLong :: Datatype
datatypeUnsignedLong = unsafePerformIO $ peek mpiUnsignedLong

foreign import ccall "&hsmpi_unsigned_short" mpiUnsignedShort :: Ptr Datatype
datatypeUnsignedShort :: Datatype
datatypeUnsignedShort = unsafePerformIO $ peek mpiUnsignedShort

class HasDatatype a where datatype :: Datatype
instance HasDatatype CChar where datatype = datatypeChar
instance HasDatatype CDouble where datatype = datatypeDouble
instance HasDatatype CFloat where datatype = datatypeFloat
instance HasDatatype CInt where datatype = datatypeInt
instance HasDatatype CLLong where datatype = datatypeLongLongInt
instance HasDatatype CLong where datatype = datatypeLong
instance HasDatatype CShort where datatype = datatypeShort
instance HasDatatype CUChar where datatype = datatypeUnsignedChar
instance HasDatatype CUInt where datatype = datatypeUnsigned
instance HasDatatype CULong where datatype = datatypeUnsignedLong
instance HasDatatype CUShort where datatype = datatypeUnsignedShort

datatypeOf :: forall a p. HasDatatype a => p a -> Datatype
datatypeOf _ = datatype @a



foreign import ccall "&hsmpi_band" mpiBand :: Ptr Op
opBand :: Op
opBand = unsafePerformIO $ peek mpiBand

foreign import ccall "&hsmpi_bor" mpiBor :: Ptr Op
opBor :: Op
opBor = unsafePerformIO $ peek mpiBor

foreign import ccall "&hsmpi_bxor" mpiBxor :: Ptr Op
opBxor :: Op
opBxor = unsafePerformIO $ peek mpiBxor

foreign import ccall "&hsmpi_land" mpiLand :: Ptr Op
opLand :: Op
opLand = unsafePerformIO $ peek mpiLand

foreign import ccall "&hsmpi_lor" mpiLor :: Ptr Op
opLor :: Op
opLor = unsafePerformIO $ peek mpiLor

foreign import ccall "&hsmpi_lxor" mpiLxor :: Ptr Op
opLxor :: Op
opLxor = unsafePerformIO $ peek mpiLxor

foreign import ccall "&hsmpi_max" mpiMax :: Ptr Op
opMax :: Op
opMax = unsafePerformIO $ peek mpiMax

foreign import ccall "&hsmpi_maxloc" mpiMaxloc :: Ptr Op
opMaxloc :: Op
opMaxloc = unsafePerformIO $ peek mpiMaxloc

foreign import ccall "&hsmpi_min" mpiMin :: Ptr Op
opMin :: Op
opMin = unsafePerformIO $ peek mpiMin

foreign import ccall "&hsmpi_minloc" mpiMinloc :: Ptr Op
opMinloc :: Op
opMinloc = unsafePerformIO $ peek mpiMinloc

foreign import ccall "&hsmpi_prod" mpiProd :: Ptr Op
opProd :: Op
opProd = unsafePerformIO $ peek mpiProd

foreign import ccall "&hsmpi_sum" mpiSum :: Ptr Op
opSum :: Op
opSum = unsafePerformIO $ peek mpiSum

instance HasDatatype a => HasDatatype (Monoid.Product a) where
  datatype = datatype @a
instance HasDatatype a => HasDatatype (Monoid.Sum a) where
  datatype = datatype @a
instance HasDatatype a => HasDatatype (Semigroup.Max a) where
  datatype = datatype @a
instance HasDatatype a => HasDatatype (Semigroup.Min a) where
  datatype = datatype @a

class (Monoid a, HasDatatype a) => HasOp a where op :: Op
instance (Num a, HasDatatype a) => HasOp (Monoid.Product a) where
  op = opProd
instance (Num a, HasDatatype a) => HasOp (Monoid.Sum a) where
  op = opSum
instance (Bounded a, Ord a, HasDatatype a) => HasOp (Semigroup.Max a) where
  op = opMax
instance (Bounded a, Ord a, HasDatatype a) => HasOp (Semigroup.Min a) where
  op = opMin



foreign import ccall "&hsmpi_any_source" mpiAnySource :: Ptr Rank
anySource :: Rank
anySource = unsafePerformIO $ peek mpiAnySource



foreign import ccall "&hsmpi_any_tag" mpiAnyTag :: Ptr Tag
anyTag :: Tag
anyTag = unsafePerformIO $ peek mpiAnyTag



--------------------------------------------------------------------------------

{#fun Abort as ^
    { getComm `Comm'
    , fromIntegral `Int'
    } -> `()' return*-#}

{#fun Allgather as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Allreduce as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Alltoall as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Barrier as ^ {getComm `Comm'} -> `()' return*-#}

{#fun Bcast as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun unsafe Comm_compare as ^
    { getComm `Comm'
    , getComm `Comm'
    , alloca- `ComparisonResult' peekEnum*
    } -> `()' return*-#}

{#fun unsafe Comm_rank as ^
    { getComm `Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' return*-#}

{#fun unsafe Comm_size as ^
    { getComm `Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' return*-#}

{#fun Exscan as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Finalize as ^ {} -> `()' return*-#}

{#fun Finalized as ^ {alloca- `Bool' peekBool*} -> `()' return*-#}

{#fun Gather as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , fromRank `Rank'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun unsafe Get_count as ^
    { withStatus* `Status'
    , getDatatype `Datatype'
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

{#fun unsafe Get_library_version as getLibraryVersion_
    { id `CString'
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

getLibraryVersion :: IO String
getLibraryVersion =
  do buf <- mallocForeignPtrBytes {#const MPI_MAX_LIBRARY_VERSION_STRING#}
     withForeignPtr buf $ \ptr ->
       do len <- getLibraryVersion_ ptr
          str <- peekCStringLen (ptr, len)
          return str

{#fun unsafe Get_processor_name as getProcessorName_
    { id `CString'
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

getProcessorName :: IO String
getProcessorName =
  do buf <- mallocForeignPtrBytes {#const MPI_MAX_PROCESSOR_NAME#}
     withForeignPtr buf $ \ptr ->
       do len <- getProcessorName_ ptr
          str <- peekCStringLen (ptr, len)
          return str

{#fun unsafe Get_version as getVersion_
    { alloca- `Int' peekInt*
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

getVersion :: IO Version
getVersion =
  do (major, minor) <- getVersion_
     return (makeVersion [major, minor])

{#fun Iallgather as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Iallreduce as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Ialltoall as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Ibarrier as ^
    { getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Ibcast as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Iexscan as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Igather as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , fromRank `Rank'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun unsafe Initialized as ^ {alloca- `Bool' peekBool*} -> `()' return*-#}

{#fun Init as init_
    { with* `CInt'
    , with* `Ptr CString'
    } -> `()' return*-#}

init :: IO ()
init = init_ argc argv

{#fun Init_thread as initThread_
    { with* `CInt'
    , with* `Ptr CString'
    , fromEnum `ThreadSupport'
    , alloca- `ThreadSupport' peekEnum*
    } -> `()' return*-#}

initThread :: ThreadSupport -> IO ThreadSupport
initThread ts = initThread_ argc argv ts

iprobe_ :: Rank -> Tag -> Comm -> IO (Bool, Status)
iprobe_ rank tag comm =
  do fst <- mallocForeignPtrBytes {#sizeof MPI_Status#}
     withForeignPtr fst $ \st ->
       do alloca $ \flag ->
            do _ <- {#call Iprobe as iprobe__#}
                    (getRank rank) (getTag tag) (getComm comm) flag st
               x <- peekBool flag
               let y = Status fst
               return (x, y)

iprobe :: Rank -> Tag -> Comm -> IO (Maybe Status)
iprobe rank tag comm = bool2maybe <$> iprobe_ rank tag comm

{#fun Irecv as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Ireduce as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getRank `Rank'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Iscan as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Iscatter as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , fromRank `Rank'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Isend as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , +
    } -> `Request' return*#}

{#fun Probe as ^
    { getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , +
    } -> `Status' return*#}

{#fun Recv as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , +
    } -> `Status' return*#}

{#fun Reduce as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getRank `Rank'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Scan as ^
    { id `Ptr ()'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getOp `Op'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Scatter as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , fromRank `Rank'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Send as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    } -> `()' return*-#}

{#fun Sendrecv as ^
    { id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , id `Ptr ()'
    , fromIntegral `Int'
    , getDatatype `Datatype'
    , getRank `Rank'
    , getTag `Tag'
    , getComm `Comm'
    , +
    } -> `Status' return*#}

test_ :: Request -> IO (Bool, Status)
test_ (Request freq) =
  do withForeignPtr freq $ \req ->
       do fst <- mallocForeignPtrBytes {#sizeof MPI_Status#}
          withForeignPtr fst $ \st ->
            do alloca $ \flag ->
                 do _ <- {#call Test as test__#} req flag st
                    x <- peekBool flag
                    let y = Status fst
                    return (x, y)

test :: Request -> IO (Maybe Status)
test req = bool2maybe <$> test_ req

{#fun Wait as ^
    { `Request'
    , +
    } -> `Status' return*#}

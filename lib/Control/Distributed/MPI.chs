{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

#include <mpi.h>
#include <mpihs.h>



-- | Module: Control.Distributed.MPI
-- Description: MPI bindings for Haskell
-- Copyright: (C) 2018 Erik Schnetter
-- License: Apache-2.0
-- Maintainer: Erik Schnetter <schnetter@gmail.com>
-- Stability: experimental
-- Portability: Requires an externally installed MPI library
--
-- MPI (the [Message Passing Interface](https://www.mpi-forum.org)) is
-- widely used standard for distributed-memory programming on HPC
-- (High Performance Computing) systems. MPI allows exchanging data
-- (_messages_) between programs running in parallel. There are
-- several high-quality open source MPI implementations (e.g. MPICH,
-- MVAPICH, OpenMPI) as well as a variety of closed-source
-- implementations. These libraries can typically make use of
-- high-bandwidth low-latency communication hardware such as
-- InfiniBand.
--
-- This library @mpi-hs@ provides Haskell bindings for MPI. It is
-- based on ideas taken from
-- [haskell-mpi](https://github.com/bjpop/haskell-mpi),
-- [Boost.MPI](https://www.boost.org/doc/libs/1_64_0/doc/html/mpi.html),
-- and [MPI for Python](https://mpi4py.readthedocs.io/en/stable/).
--
-- @mpi-hs@ provides two API levels: A low-level API gives rather
-- direct access to the MPI API, apart from certain "reasonable"
-- mappings from C to Haskell (e.g. output arguments that are in C
-- stored to a pointer are in Haskell regular return values). A
-- high-level API simplifies exchanging arbitrary values that can be
-- serialized.
--
-- This module 'MPI' is the low-level interface.

module Control.Distributed.MPI
  ( Comm(..)
  , ComparisonResult(..)
  , Count(..)
  , fromCount
  , toCount
  , Datatype(..)
  , Op(..)
  , Rank(..)
  , fromRank
  , rootRank
  , toRank
  , Request(..)
  , Status(..)
  --, statusError
  , getSource
  , getTag
  , Tag(..)
  , fromTag
  , toTag
  , unitTag
  , ThreadSupport(..)
  , threadSupport

  , Pointer(..)
  , commNull
  , commSelf
  , commWorld
  , countUndefined
  -- TODO: use a module for this namespace
  , datatypeNull
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
  -- , datatypeOf
  -- TODO: use a module for this namespace
  , opNull
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
  -- , HasOp(..)
  , anySource
  , requestNull
  -- , statusIgnore
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
  , getElements
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
  , probe_
  , recv
  , recv_
  , reduce
  , scan
  , scatter
  , send
  , sendrecv
  , sendrecv_
  , test
  , test_
  , wait
  , wait_
  , wtick
  , wtime
  ) where

import Prelude hiding (fromEnum, fst, init, toEnum)
import qualified Prelude

import Control.Monad (liftM)
import Data.Coerce
import Data.IORef
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

-- | A generic pointer-like type that supports converting to a 'Ptr'.
-- This class describes the buffers used to send and receive messages.
class Pointer p where
  withPtr :: Storable a => p a -> (Ptr a -> IO b) -> IO b

instance Pointer Ptr where
  withPtr p f = f p

instance Pointer ForeignPtr where
  withPtr = withForeignPtr

instance Pointer StablePtr where
  withPtr p f = f (castPtr (castStablePtrToPtr p))



-- | An MPI communicator, wrapping @MPI_Comm@. A communicator defines
-- an independent communication channel between a group of processes.
-- Communicators need to be explicitly created and freed by the MPI
-- library. 'commWorld' is a communicator that is always available,
-- and which includes all processes.
{#pointer *MPI_Comm as Comm foreign newtype#}

deriving instance Eq Comm
deriving instance Ord Comm
deriving instance Show Comm

-- | The result of comparing two MPI communicator (see 'commCompare').
{#enum ComparisonResult {} deriving (Eq, Ord, Read, Show)#}



-- | A newtype wrapper describing the size of a message. Use 'toCount'
-- and 'fromCount' to convert between 'Count' and other integral
-- types.
newtype Count = Count CInt
  deriving (Eq, Ord, Enum, Integral, Num, Real, Storable)

instance Read Count where
  readsPrec p = map (\(c, s) -> (Count c, s)) . readsPrec p

instance Show Count where
  showsPrec p (Count c) = showsPrec p c

-- | Convert an integer to a count.
toCount :: Integral i => i -> Count
toCount i = Count (fromIntegral i)

-- | Convert a count to an integer.
fromCount :: Integral i => Count -> i
fromCount (Count c) = fromIntegral c



-- | An MPI datatype, wrapping @MPI_Datatype@. Datatypes need to be
-- explicitly created and freed by the MPI library. Predefined
-- datatypes exist for most simple C types such as 'CInt' or
-- 'CDouble'.
{#pointer *MPI_Datatype as Datatype foreign newtype#}

deriving instance Eq Datatype
deriving instance Ord Datatype
deriving instance Show Datatype



-- | An MPI reduction operation, wrapping @MPI_Op@. Reduction
-- operations need to be explicitly created and freed by the MPI
-- library. Predefined operation exist for simple semigroups such as
-- sum, maximum, or minimum.
--
-- An MPI reduction operation corresponds to a Semigroup, not a
-- Monoid, i.e. MPI has no notion of a respective neutral element.
{#pointer *MPI_Op as Op foreign newtype#}

deriving instance Eq Op
deriving instance Ord Op
deriving instance Show Op



-- | A newtype wrapper describing the source or destination of a
-- message, i.e. a process. Each communicator numbers its processes
-- sequentially starting from zero. Use 'toRank' and 'fromRank' to
-- convert between 'Rank' and other integral types. 'rootRank' is the
-- root (first) process of a communicator.
--
-- The association between a rank and a communicator is not explicitly
-- tracked. From MPI's point of view, ranks are simply integers. The
-- same rank might correspond to different processes in different
-- communicators.
newtype Rank = Rank CInt
  deriving (Eq, Ord, Enum, Integral, Num, Real, Storable)

instance Read Rank where
  readsPrec p = map (\(r, s) -> (Rank r, s)) . readsPrec p

instance Show Rank where
  showsPrec p (Rank r) = showsPrec p r

instance Ix Rank where
  range (Rank rmin, Rank rmax) = Rank <$> [rmin..rmax]
  {-# INLINE index #-}
  index b@(Rank rmin, _) i@(Rank r)
    | inRange b i = fromIntegral (r - rmin)
    | otherwise   = indexError b i "MPI.Rank"
  inRange (Rank rmin, Rank rmax) (Rank r) = rmin <= r && r <= rmax

-- | Convert an enum to a rank.
toRank :: Enum e => e -> Rank
toRank e = Rank (fromIntegral (fromEnum e))

-- | Convert a rank to an enum.
fromRank :: Enum e => Rank -> e
fromRank (Rank r) = toEnum (fromIntegral r)

-- | The root (first) rank of a communicator.
rootRank :: Rank
rootRank = toRank 0



-- | An MPI request, wrapping @MPI_Request@. A request describes a
-- communication that is currently in progress. Each request must be
-- explicitly freed via 'cancel', 'test', or 'wait'.
{#pointer *MPI_Request as Request foreign newtype#}

deriving instance Eq Request
deriving instance Ord Request
deriving instance Show Request



-- | An MPI status, wrapping @MPI_Status@. The status describes
-- certain properties of a message. It contains information such as
-- the source of a communication ('getSource'), the message tag
-- ('getTag'), or the size of the message ('getCount', 'getElements').
--
-- In many cases, the status is not interesting. In this case, you can
-- use alternative functions ending with an underscore (e.g. 'recv_')
-- that do not calculate a status.
--
-- The status is particularly interesting when using 'probe' or
-- 'iprobe', as it describes a message that is ready to be received,
-- but which has not been received yet.
{#pointer *MPI_Status as Status foreign newtype#}

deriving instance Eq Status
deriving instance Ord Status
deriving instance Show Status

-- statusError :: Status -> IO Error
-- statusError (Status mst) =
--   Error $ {#get MPI_Status.MPI_ERROR#} mst

-- | Get the source rank of a message.
getSource :: Status -> IO Rank
getSource (Status fst) =
  withForeignPtr fst (\pst -> Rank <$> {#get MPI_Status->MPI_SOURCE#} pst)

-- | Get the message tag.
getTag :: Status -> IO Tag
getTag (Status fst) =
  withForeignPtr fst (\pst -> Tag <$> {#get MPI_Status->MPI_TAG#} pst)



-- | A newtype wrapper describing a message tag. A tag defines a
-- sub-channel within a communicator. While communicators are
-- heavy-weight object that are expensive to set up and tear down, a
-- tag is a lightweight mechanism using an integer. Use 'toTag' and
-- 'fromTag' to convert between 'Count' and other enum types.
-- 'unitTag' defines a standard tag that can be used as default.
newtype Tag = Tag CInt
  deriving (Eq, Ord, Read, Show, Enum, Num, Storable)

-- | Convert an enum to a tag.
toTag :: Enum e => e -> Tag
toTag e = Tag (fromIntegral (fromEnum e))

-- | Convert a tag to an enum.
fromTag :: Enum e => Tag -> e
fromTag (Tag t) = toEnum (fromIntegral t)

-- | Useful default tag.
unitTag :: Tag
unitTag = toTag ()



-- | Thread support levels for MPI.
--
-- * 'ThreadSingle': The application must be single-threaded
--
-- * 'ThreadFunneled': The application might be multi-threaded, but
--   only a single thread will call MPI
--
-- * 'ThreadSerialized': The application might be multi-threaded, but
--   the application guarantees that only one thread at a time will
--   call MPI
--
-- * 'ThreadMultiple': The application is multi-threaded, and
--   different threads might call MPI at the same time
{#enum ThreadSupport {} deriving (Eq, Ord, Read, Show)#}

-- | When MPI is initialized with this library, then it will remember
-- the provided level of thread support. (This might be less than the
-- requested level.)
threadSupport :: IO (Maybe ThreadSupport)
threadSupport = readIORef providedThreadSupport

providedThreadSupport :: IORef (Maybe ThreadSupport)
providedThreadSupport = unsafePerformIO (newIORef Nothing)



--------------------------------------------------------------------------------

-- | A null (invalid) communicator.
{#fun pure mpihs_get_comm_null as commNull {+} -> `Comm'#}

-- | The self communicator. Each process has its own self communicator
-- that includes only this process.
{#fun pure mpihs_get_comm_self as commSelf {+} -> `Comm'#}

-- | The world communicator, which includes all processes.
{#fun pure mpihs_get_comm_world as commWorld {+} -> `Comm'#}



-- | Error value returned by 'getCount' if the message is too large,
-- or if the message size is not an integer multiple of the provided
-- datatype.
{#fun pure mpihs_get_undefined as countUndefined {} -> `Count' toCount#}



-- | A null (invalid) datatype.
{#fun pure mpihs_get_datatype_null as datatypeNull {+} -> `Datatype'#}

-- | MPI datatype for a byte (essentially 'CUChar') (@MPI_BYTE@).
{#fun pure mpihs_get_byte as datatypeByte {+} -> `Datatype'#}

-- | MPI datatype for 'CChar' (@MPI_CHAR@).
{#fun pure mpihs_get_char as datatypeChar {+} -> `Datatype'#}

-- | MPI datatype for 'CDouble' (@MPI_DOUBLE@).
{#fun pure mpihs_get_double as datatypeDouble {+} -> `Datatype'#}

-- | MPI datatype for 'CFloat' (@MPI_FLOAT@).
{#fun pure mpihs_get_float as datatypeFloat {+} -> `Datatype'#}

-- | MPI datatype for 'CInt' (@MPI_INT@).
{#fun pure mpihs_get_int as datatypeInt {+} -> `Datatype'#}

-- | MPI datatype for 'CLong' (@MPI_LONG@).
{#fun pure mpihs_get_long as datatypeLong {+} -> `Datatype'#}

-- | MPI datatype for the C type 'long double' (@MPI_LONG_DOUBLE@).
{#fun pure mpihs_get_long_double as datatypeLongDouble {+} -> `Datatype'#}

-- | MPI datatype for 'CLLong' (@MPI_LONG_LONG_INT@). (There is no MPI
-- datatype for 'CULLong@).
{#fun pure mpihs_get_long_long_int as datatypeLongLongInt {+} -> `Datatype'#}

-- | MPI datatype for 'CShort' (@MPI_SHORT@).
{#fun pure mpihs_get_short as datatypeShort {+} -> `Datatype'#}

-- | MPI datatype for 'CUInt' (@MPI_UNSIGNED@).
{#fun pure mpihs_get_unsigned as datatypeUnsigned {+} -> `Datatype'#}

-- | MPI datatype for 'CUChar' (@MPI_UNSIGNED_CHAR@).
{#fun pure mpihs_get_unsigned_char as datatypeUnsignedChar {+} -> `Datatype'#}

-- | MPI datatype for 'CULong' (@MPI_UNSIGNED_LONG@).
{#fun pure mpihs_get_unsigned_long as datatypeUnsignedLong {+} -> `Datatype'#}

-- | MPI datatype for 'CUShort' (@MPI_UNSIGNED_SHORT@).
{#fun pure mpihs_get_unsigned_short as datatypeUnsignedShort {+} -> `Datatype'#}

-- | A type class mapping Haskell types to MPI datatypes. This is used
-- to automatically determine the MPI datatype for communication
-- buffers.
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

-- instance Coercible Int CChar => HasDatatype Int where
--   datatype = datatype @CChar
-- instance Coercible Int CShort => HasDatatype Int where
--   datatype = datatype @CShort
-- instance Coercible Int CInt => HasDatatype Int where
--   datatype = datatype @CInt
-- instance Coercible Int CLong => HasDatatype Int where
--   datatype = datatype @CLong
-- instance Coercible Int CLLong => HasDatatype Int where
--   datatype = datatype @CLLong

-- instance HasDatatype Int where
--   datatype = if | coercible @Int @CChar -> datatype @CChar
--                 | coercible @Int @CShort -> datatype @CShort
--                 | coercible @Int @CInt -> datatype @CInt
--                 | coercible @Int @CLong -> datatype @CLong
--                 | coercible @Int @CLLong -> datatype @CLLong
-- instance HasDatatype Int8 where
--   datatype = if | coercible @Int @CChar -> datatype @CChar
--                 | coercible @Int @CShort -> datatype @CShort
--                 | coercible @Int @CInt -> datatype @CInt
--                 | coercible @Int @CLong -> datatype @CLong
--                 | coercible @Int @CLLong -> datatype @CLLong
-- instance HasDatatype Int16 where
--   datatype = if | coercible @Int @CChar -> datatype @CChar
--                 | coercible @Int @CShort -> datatype @CShort
--                 | coercible @Int @CInt -> datatype @CInt
--                 | coercible @Int @CLong -> datatype @CLong
--                 | coercible @Int @CLLong -> datatype @CLLong
-- instance HasDatatype Int32 where
--   datatype = if | coercible @Int @CChar -> datatype @CChar
--                 | coercible @Int @CShort -> datatype @CShort
--                 | coercible @Int @CInt -> datatype @CInt
--                 | coercible @Int @CLong -> datatype @CLong
--                 | coercible @Int @CLLong -> datatype @CLLong
-- instance HasDatatype Int64 where
--   datatype = if | coercible @Int @CChar -> datatype @CChar
--                 | coercible @Int @CShort -> datatype @CShort
--                 | coercible @Int @CInt -> datatype @CInt
--                 | coercible @Int @CLong -> datatype @CLong
--                 | coercible @Int @CLLong -> datatype @CLLong
-- instance HasDatatype Word where
--   datatype = if | coercible @Int @CUChar -> datatype @CUChar
--                 | coercible @Int @CUShort -> datatype @CUShort
--                 | coercible @Int @CUInt -> datatype @CUInt
--                 | coercible @Int @CULong -> datatype @CULong
--                 -- | coercible @Int @CULLong -> datatype @CULLong
-- instance HasDatatype Word8 where
--   datatype = if | coercible @Int @CUChar -> datatype @CUChar
--                 | coercible @Int @CUShort -> datatype @CUShort
--                 | coercible @Int @CUInt -> datatype @CUInt
--                 | coercible @Int @CULong -> datatype @CULong
--                 -- | coercible @Int @CULLong -> datatype @CULLong
-- instance HasDatatype Word16 where
--   datatype = if | coercible @Int @CUChar -> datatype @CUChar
--                 | coercible @Int @CUShort -> datatype @CUShort
--                 | coercible @Int @CUInt -> datatype @CUInt
--                 | coercible @Int @CULong -> datatype @CULong
--                 -- | coercible @Int @CULLong -> datatype @CULLong
-- instance HasDatatype Word32 where
--   datatype = if | coercible @Int @CUChar -> datatype @CUChar
--                 | coercible @Int @CUShort -> datatype @CUShort
--                 | coercible @Int @CUInt -> datatype @CUInt
--                 | coercible @Int @CULong -> datatype @CULong
--                 -- | coercible @Int @CULLong -> datatype @CULLong
-- instance HasDatatype Word64 where
--   datatype = if | coercible @Int @CUChar -> datatype @CUChar
--                 | coercible @Int @CUShort -> datatype @CUShort
--                 | coercible @Int @CUInt -> datatype @CUInt
--                 | coercible @Int @CULong -> datatype @CULong
--                 -- | coercible @Int @CULLong -> datatype @CULLong
-- instance HasDatatype Float where
--   datatype = if | coercible @Float @CFloat -> datatype @CFloat
--                 | coercible @Float @CDouble -> datatype @CDouble
-- instance HasDatatype Double where
--   datatype = if | coercible @Double @CFloat -> datatype @CFloat
--                 | coercible @Double @CDouble -> datatype @CDouble

-- datatypeOf :: forall a p. HasDatatype a => p a -> Datatype
-- datatypeOf _ = datatype @a



-- | A null (invalid) reduction operation.
{#fun pure mpihs_get_op_null as opNull {+} -> `Op'#}

-- | The bitwise and @(.&.)@ reduction operation (@MPI_BAND@).
{#fun pure mpihs_get_band as opBand {+} -> `Op'#}

-- | The bitwise or @(.|.)@ reduction operation (@MPI_BOR@).
{#fun pure mpihs_get_bor as opBor {+} -> `Op'#}

-- | The bitwise (@xor@) reduction operation (@MPI_BXOR@).
{#fun pure mpihs_get_bxor as opBxor {+} -> `Op'#}

-- | The logical and @(&&)@ reduction operation (@MPI_LAND@).
{#fun pure mpihs_get_land as opLand {+} -> `Op'#}

-- | The logical or @(||)@ reduction operation (@MPI_LOR@).
{#fun pure mpihs_get_lor as opLor {+} -> `Op'#}

-- | The logical xor reduction operation (@MPI_LXOR@).
{#fun pure mpihs_get_lxor as opLxor {+} -> `Op'#}

-- | The 'maximum' reduction operation (@MPI_MAX@).
{#fun pure mpihs_get_max as opMax {+} -> `Op'#}

-- | The argmax reduction operation to find the maximum and its rank
-- (@MPI_MAXLOC@).
{#fun pure mpihs_get_maxloc as opMaxloc {+} -> `Op'#}

-- | The 'minimum' reduction operation (@MPI_MIN@).
{#fun pure mpihs_get_min as opMin {+} -> `Op'#}

-- | The argmin reduction operation to find the minimum and its rank
-- (@MPI_MINLOC@).
{#fun pure mpihs_get_minloc as opMinloc {+} -> `Op'#}

-- | The (@product@) reduction operation (@MPI_PROD@).
{#fun pure mpihs_get_prod as opProd {+} -> `Op'#}

-- | The (@sum@) reduction operation (@MPI_SUM@).
{#fun pure mpihs_get_sum as opSum {+} -> `Op'#}

instance HasDatatype a => HasDatatype (Monoid.Product a) where
  datatype = datatype @a
instance HasDatatype a => HasDatatype (Monoid.Sum a) where
  datatype = datatype @a
instance HasDatatype a => HasDatatype (Semigroup.Max a) where
  datatype = datatype @a
instance HasDatatype a => HasDatatype (Semigroup.Min a) where
  datatype = datatype @a

-- class (Monoid a, HasDatatype a) => HasOp a where op :: Op
-- instance (Num a, HasDatatype a) => HasOp (Monoid.Product a) where
--   op = opProd
-- instance (Num a, HasDatatype a) => HasOp (Monoid.Sum a) where
--   op = opSum
-- instance (Bounded a, Ord a, HasDatatype a) => HasOp (Semigroup.Max a) where
--   op = opMax
-- instance (Bounded a, Ord a, HasDatatype a) => HasOp (Semigroup.Min a) where
--   op = opMin



-- | Rank placeholder to specify that a message can be received from
-- any source. When calling 'probe' or 'recv' (or 'iprobe' or 'irecv')
-- with 'anySource' as source, the actual source can be determined
-- from the returned message status via 'getSource'.
{#fun pure mpihs_get_any_source as anySource {} -> `Rank' toRank#}



-- | A null (invalid) request.
{#fun pure mpihs_get_request_null as requestNull {+} -> `Request'#}



{#fun pure mpihs_get_status_ignore as statusIgnore {} -> `Status'#}

withStatusIgnore :: (Ptr Status -> IO a) -> IO a
withStatusIgnore = withStatus statusIgnore



-- | Tag placeholder to specify that a message can have any tag. When
-- calling 'probe' or 'recv' (or 'iprobe' or 'irecv') with 'anyTag' as
-- tag, the actual tag can be determined from the returned message
-- status via 'getTag'.
{#fun pure mpihs_get_any_tag as anyTag {} -> `Tag' toTag#}



--------------------------------------------------------------------------------

-- | Terminate MPI execution environment (@MPI_Abort@).
{#fun Abort as ^
    { withComm* %`Comm' -- ^ Communicator describing which processes
                        -- to terminate
    , fromIntegral `Int' -- ^ Error code
    } -> `()' return*-#}

{#fun Allgather as allgatherTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withComm* %`Comm'
    } -> `()' return*-#}

-- | Gather data from all processes and broadcast the result
-- (@MPI_Allgather@). The MPI datatypes are determined automatically
-- from the buffer pointer types.
allgather :: forall a b p q.
             ( Pointer p, Pointer q
             , Storable a, HasDatatype a, Storable b, HasDatatype b)
          => p a                -- ^ Source buffer
          -> Count              -- ^ Number of source elements
          -> q b                -- ^ Destination buffer
          -> Count              -- ^ Number of destination elements
          -> Comm               -- ^ Communicator
          -> IO ()
allgather sendbuf sendcount recvbuf recvcount comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  allgatherTyped (castPtr sendbuf') sendcount (datatype @a)
                 (castPtr recvbuf') recvcount (datatype @b)
                 comm

{#fun Allreduce as allreduceTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , withComm* %`Comm'
    } -> `()' return*-#}

-- | Reduce data from all processes and broadcast the result
-- (@MPI_Allreduce@). The MPI datatype is determined automatically
-- from the buffer pointer types.
allreduce :: forall a p q.
             ( Pointer p, Pointer q, Storable a, HasDatatype a)
          => p a                -- ^ Source buffer
          -> q a                -- ^ Destination buffer
          -> Count              -- ^ Number of elements
          -> Op                 -- ^ Reduction operation
          -> Comm               -- ^ Communicator
          -> IO ()
allreduce sendbuf recvbuf count op comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  allreduceTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op
                 comm

{#fun Alltoall as alltoallTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withComm* %`Comm'
    } -> `()' return*-#}

-- | Send data from all processes to all processes (@MPI_Alltoall@).
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
alltoall :: forall a b p q.
            ( Pointer p, Pointer q
            , Storable a, HasDatatype a, Storable b, HasDatatype b)
         => p a                 -- ^ Source buffer
         -> Count               -- ^ Number of source elements
         -> q b                 -- ^ Destination buffer
         -> Count               -- ^ Number of destination elements
         -> Comm                -- ^ Communicator
         -> IO ()
alltoall sendbuf sendcount recvbuf recvcount comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  alltoallTyped (castPtr sendbuf') sendcount (datatype @a)
                (castPtr recvbuf') recvcount (datatype @b)
                comm

-- | Barrier.
{#fun Barrier as ^
    { withComm* %`Comm'         -- ^ Communicator
    } -> `()' return*-#}

{#fun Bcast as bcastTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , withComm* %`Comm'
    } -> `()' return*-#}

-- | Broadcast data from one process to all processes (@MPI_Bcast@).
-- The MPI datatype is determined automatically from the buffer
-- pointer type.
bcast :: forall a p. (Pointer p, Storable a, HasDatatype a)
      => p a -- ^ Buffer pointer (read on the root process, written on
             -- all other processes)
      -> Count                  -- ^ Number of elements
      -> Rank                   -- ^ Root rank (sending process)
      -> Comm                   -- ^ Communicator
      -> IO ()
bcast buf count root comm =
  withPtr buf $ \buf' ->
  bcastTyped (castPtr buf') count (datatype @a) root comm

{#fun unsafe Comm_compare as ^
    { withComm* %`Comm'
    , withComm* %`Comm'
    , alloca- `ComparisonResult' peekEnum*
    } -> `()' return*-#}

{#fun unsafe Comm_rank as ^
    { withComm* %`Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' return*-#}

{#fun unsafe Comm_size as ^
    { withComm* %`Comm'
    , alloca- `Rank' peekCoerce*
    } -> `()' return*-#}

{#fun Exscan as exscanTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , withComm* %`Comm'
    } -> `()' return*-#}

exscan :: forall a p q.
          ( Pointer p, Pointer q, Storable a, HasDatatype a) =>
          p a -> q a -> Count -> Op -> Comm -> IO ()
exscan sendbuf recvbuf count op comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  exscanTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op comm

{#fun Finalize as ^ {} -> `()' return*-#}

{#fun Finalized as ^ {alloca- `Bool' peekBool*} -> `()' return*-#}

{#fun Gather as gatherTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , withComm* %`Comm'
    } -> `()' return*-#}

gather :: forall a b p q.
          ( Pointer p, Pointer q
          , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
          p a -> Count -> q b -> Count -> Rank -> Comm -> IO ()
gather sendbuf sendcount recvbuf recvcount root comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  gatherTyped (castPtr sendbuf') sendcount (datatype @a)
              (castPtr recvbuf') recvcount (datatype @b)
              root comm

-- | Get the size of a message, in terms of objects of type 'Datatype'.
{#fun unsafe Get_count as ^
    { withStatus* `Status'
    , withDatatype* %`Datatype'
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

-- | Get the number of elements in message, in terms of sub-object of
-- the type 'datatype'. This is useful when a message contains partial
-- objects of type 'datatype'.
{#fun unsafe Get_elements as ^
    { withStatus* `Status'
    , withDatatype* %`Datatype'
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

{#fun Iallgather as iallgatherTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

iallgather :: forall a b p q.
              ( Pointer p, Pointer q
              , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
              p a -> Count -> q b -> Count -> Comm -> IO Request
iallgather sendbuf sendcount recvbuf recvcount comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  iallgatherTyped (castPtr sendbuf') sendcount (datatype @a)
                  (castPtr recvbuf') recvcount (datatype @b)
                  comm

{#fun Iallreduce as iallreduceTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

iallreduce :: forall a p q.
              ( Pointer p, Pointer q, Storable a, HasDatatype a) =>
              p a -> q a -> Count -> Op -> Comm -> IO Request
iallreduce sendbuf recvbuf count op comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  iallreduceTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op
                  comm

{#fun Ialltoall as ialltoallTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

ialltoall :: forall a b p q.
             ( Pointer p, Pointer q
             , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
             p a -> Count -> q b -> Count -> Comm -> IO Request
ialltoall sendbuf sendcount recvbuf recvcount comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  ialltoallTyped (castPtr sendbuf') sendcount (datatype @a)
                 (castPtr recvbuf') recvcount (datatype @b)
                 comm

{#fun Ibarrier as ^
    { withComm* %`Comm'
    , +
    } -> `Request' return*#}

{#fun Ibcast as ibcastTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

ibcast :: forall a p. (Pointer p, Storable a, HasDatatype a) =>
          p a -> Count -> Rank -> Comm -> IO Request
ibcast buf count root comm =
  withPtr buf $ \buf' ->
  ibcastTyped (castPtr buf') count (datatype @a) root comm

{#fun Iexscan as iexscanTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

iexscan :: forall a p q.
           ( Pointer p, Pointer q, Storable a, HasDatatype a) =>
           p a -> q a -> Count -> Op -> Comm -> IO Request
iexscan sendbuf recvbuf count op comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  iexscanTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op comm

{#fun Igather as igatherTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

igather :: forall a b p q.
           ( Pointer p, Pointer q
           , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
           p a -> Count -> q b -> Count -> Rank -> Comm -> IO Request
igather sendbuf sendcount recvbuf recvcount root comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  igatherTyped (castPtr sendbuf') sendcount (datatype @a)
               (castPtr recvbuf') recvcount (datatype @b)
               root comm

{#fun unsafe Initialized as ^ {alloca- `Bool' peekBool*} -> `()' return*-#}

{#fun Init as init_
    { with* `CInt'
    , with* `Ptr CString'
    } -> `()' return*-#}

init :: IO ()
init = do init_ argc argv
          writeIORef providedThreadSupport (Just ThreadSingle)

{#fun Init_thread as initThread_
    { with* `CInt'
    , with* `Ptr CString'
    , fromEnum `ThreadSupport'
    , alloca- `ThreadSupport' peekEnum*
    } -> `()' return*-#}

initThread :: ThreadSupport -> IO ThreadSupport
initThread ts = do ts' <- initThread_ argc argv ts
                   writeIORef providedThreadSupport (Just ts')
                   return ts'

iprobeBool :: Rank -> Tag -> Comm -> IO (Bool, Status)
iprobeBool rank tag comm =
  withComm comm $ \comm' ->
  do st <- Status <$> mallocForeignPtrBytes {#sizeof MPI_Status#}
     withStatus st $ \st' ->
       do alloca $ \flag ->
            do _ <- {#call mpihs_iprobe as iprobeBool_#}
                    (fromRank rank) (fromTag tag) comm' flag st'
               b <- peekBool flag
               return (b, st)

iprobe :: Rank -> Tag -> Comm -> IO (Maybe Status)
iprobe rank tag comm = bool2maybe <$> iprobeBool rank tag comm

{#fun Irecv as irecvTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

irecv :: forall a p. (Pointer p, Storable a, HasDatatype a) =>
        p a -> Count -> Rank -> Tag -> Comm -> IO Request
irecv recvbuf recvcount recvrank recvtag comm =
  withPtr recvbuf $ \recvbuf' ->
  irecvTyped (castPtr recvbuf') recvcount (datatype @a) recvrank recvtag comm

{#fun Ireduce as ireduceTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , fromRank `Rank'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

ireduce :: forall a p q.
           ( Pointer p, Pointer q, Storable a, HasDatatype a) =>
           p a -> q a -> Count -> Op -> Rank -> Comm -> IO Request
ireduce sendbuf recvbuf count op rank comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  ireduceTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op rank
               comm

{#fun Iscan as iscanTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

iscan :: forall a p q.
         ( Pointer p, Pointer q, Storable a, HasDatatype a) =>
         p a -> q a -> Count -> Op -> Comm -> IO Request
iscan sendbuf recvbuf count op comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  iscanTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op comm

{#fun Iscatter as iscatterTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

iscatter :: forall a b p q.
            ( Pointer p, Pointer q
            , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
            p a -> Count -> q b -> Count -> Rank -> Comm -> IO Request
iscatter sendbuf sendcount recvbuf recvcount root comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  iscatterTyped (castPtr sendbuf') sendcount (datatype @a)
                (castPtr recvbuf') recvcount (datatype @b)
                root comm

{#fun Isend as isendTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

isend :: forall a p. (Pointer p, Storable a, HasDatatype a) =>
         p a -> Count -> Rank -> Tag -> Comm -> IO Request
isend sendbuf sendcount sendrank sendtag comm =
  withPtr sendbuf $ \sendbuf' ->
  isendTyped (castPtr sendbuf') sendcount (datatype @a) sendrank sendtag comm

{#fun Probe as ^
    { fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , +
    } -> `Status' return*#}

{#fun Probe as probe_
    { fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , withStatusIgnore- `Status'
    } -> `()' return*-#}

{#fun Recv as recvTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , +
    } -> `Status' return*#}

recv :: forall a p. (Pointer p, Storable a, HasDatatype a) =>
        p a -> Count -> Rank -> Tag -> Comm -> IO Status
recv recvbuf recvcount recvrank recvtag comm =
  withPtr recvbuf $ \recvbuf' ->
  recvTyped (castPtr recvbuf') recvcount (datatype @a) recvrank recvtag comm

{#fun Recv as recvTyped_
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , withStatusIgnore- `Status'
    } -> `()' return*-#}

recv_ :: forall a p. (Pointer p, Storable a, HasDatatype a) =>
         p a -> Count -> Rank -> Tag -> Comm -> IO ()
recv_ recvbuf recvcount recvrank recvtag comm =
  withPtr recvbuf $ \recvbuf' ->
  recvTyped_ (castPtr recvbuf') recvcount (datatype @a) recvrank recvtag comm

{#fun Reduce as reduceTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , fromRank `Rank'
    , withComm* %`Comm'
    } -> `()' return*-#}

reduce :: forall a p q.
          ( Pointer p, Pointer q, Storable a, HasDatatype a) =>
          p a -> q a -> Count -> Op -> Rank -> Comm -> IO ()
reduce sendbuf recvbuf count op rank comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  reduceTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op rank
              comm

{#fun Scan as scanTyped
    { id `Ptr ()'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , withOp* %`Op'
    , withComm* %`Comm'
    } -> `()' return*-#}

scan :: forall a p q.
        ( Pointer p, Pointer q, Storable a, HasDatatype a) =>
        p a -> q a -> Count -> Op -> Comm -> IO ()
scan sendbuf recvbuf count op comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  scanTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op comm

{#fun Scatter as scatterTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , withComm* %`Comm'
    } -> `()' return*-#}

scatter :: forall a b p q.
           ( Pointer p, Pointer q
           , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
           p a -> Count -> q b -> Count -> Rank -> Comm -> IO ()
scatter sendbuf sendcount recvbuf recvcount root comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  scatterTyped (castPtr sendbuf') sendcount (datatype @a)
               (castPtr recvbuf') recvcount (datatype @b)
               root comm

{#fun Send as sendTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    } -> `()' return*-#}

send :: forall a p. (Pointer p, Storable a, HasDatatype a) =>
        p a -> Count -> Rank -> Tag -> Comm -> IO ()
send sendbuf sendcount sendrank sendtag comm =
  withPtr sendbuf $ \sendbuf' ->
  sendTyped (castPtr sendbuf') sendcount (datatype @a) sendrank sendtag comm

{#fun Sendrecv as sendrecvTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , +
    } -> `Status' return*#}

sendrecv :: forall a b p q.
            ( Pointer p, Pointer q
            , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
            p a -> Count -> Rank -> Tag ->
            q b -> Count -> Rank -> Tag ->
            Comm -> IO Status
sendrecv sendbuf sendcount sendrank sendtag
         recvbuf recvcount recvrank recvtag
         comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  sendrecvTyped (castPtr sendbuf') sendcount (datatype @a) sendrank sendtag
                (castPtr recvbuf') recvcount (datatype @b) recvrank recvtag
                comm

{#fun Sendrecv as sendrecvTyped_
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , withStatusIgnore- `Status'
    } -> `()' return*-#}

sendrecv_ :: forall a b p q.
             ( Pointer p, Pointer q
             , Storable a, HasDatatype a, Storable b, HasDatatype b) =>
             p a -> Count -> Rank -> Tag ->
             q b -> Count -> Rank -> Tag ->
             Comm -> IO ()
sendrecv_ sendbuf sendcount sendrank sendtag
          recvbuf recvcount recvrank recvtag
          comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  sendrecvTyped_ (castPtr sendbuf') sendcount (datatype @a) sendrank sendtag
                 (castPtr recvbuf') recvcount (datatype @b) recvrank recvtag
                 comm

testBool :: Request -> IO (Bool, Status)
testBool req =
  withRequest req $ \req' ->
  alloca $ \flag ->
  do st <- Status <$> mallocForeignPtrBytes {#sizeof MPI_Status#}
     withStatus st $ \st' ->
       do _ <- {#call Test as testBool_#} req' flag st'
          b <- peekBool flag
          return (b, st)

test :: Request -> IO (Maybe Status)
test req = bool2maybe <$> testBool req

-- {#fun Test as test_
--     { withRequest* `Request'
--     , alloca- `Bool' peekBool*
--     , withStatusIgnore- `Status'
--     } -> `()' return*-#}

test_ :: Request -> IO Bool
test_ req =
  withRequest req $ \req' ->
  alloca $ \flag ->
  withStatusIgnore $ \st ->
  do _ <- {#call Test as test__#} req' flag st
     peekBool flag

{#fun Wait as ^
    { withRequest* `Request'
    , +
    } -> `Status' return*#}

{#fun Wait as wait_
    { withRequest* `Request'
    , withStatusIgnore- `Status'
    } -> `()' return*-#}

{#fun unsafe Wtick as ^ {} -> `Double'#}

{#fun unsafe Wtime as ^ {} -> `Double'#}

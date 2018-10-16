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
--
-- In general, the MPI C API is translated to Haskell in the following
-- way, greatly aided by @c2hs@:
--
-- * Names of constants and functions have the @MPI_@ prefix removed.
--   Underscores are replaced by CamelCase. The 'MPI' module is
--   intended to be imported qualified, as in 'import qualified
--   Control.Distributed.MPI as MPI'.
--
-- * Opaque types such as @MPI_Request@ are wrapped via newtypes
--   holding pointers and are allocated on the heap as foreign
--   pointers.
--
-- * The MPI error return code is omitted. Currently error codes are
--   ignored, since the default MPI behaviour is to terminate the
--   application instead of actually returning error codes. In the
--   future, error codes might be reported via exceptions.
--
-- * Output arguments that are written via pointers in C are returned.
--   Some functions now return tuples. If the output argument is a
--   boolean value that indicates whether another output argument is
--   value, then this is translated into a 'Maybe'.
--
-- * MPI has a facility to pass @MPI_STATUS_IGNORE@ to indicate that
--   no message status should be returned. This is instead handled by
--   providing alternative functions ending with an underscore (e.g.
--   'recv_') that return @()@ instead of 'Status'.
--
-- * Datatype arguments are hidden. Instead, the correct MPI datatypes
--   are inferred from the pointer type specifying the communication
--   buffers. (This translation could be relaxed, and the original MPI
--   functions could be exposed as well when needed.)

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
  , iprobe_
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

-- | Get the source rank of a message (@MPI_SOURCE@).
getSource :: Status -> IO Rank
getSource (Status fst) =
  withForeignPtr fst (\pst -> Rank <$> {#get MPI_Status->MPI_SOURCE#} pst)

-- | Get the message tag (@MPI_TAG@).
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



-- | Thread support levels for MPI (see 'initThread'):
--
-- * 'ThreadSingle' (@MPI_THREAD_SINGLE@): The application must be
-- * single-threaded
--
-- * 'ThreadFunneled' (@MPI_THREAD_FUNNELED@): The application might
--   be multi-threaded, but only a single thread will call MPI
--
-- * 'ThreadSerialized' (@MPI_THREAD_SERIALIZED@): The application
--   might be multi-threaded, but the application guarantees that only
--   one thread at a time will call MPI
--
-- * 'ThreadMultiple' (@MPI_THREAD_MULTIPLE@): The application is
--   multi-threaded, and different threads might call MPI at the same
--   time
{#enum ThreadSupport {} deriving (Eq, Ord, Read, Show)#}

-- | When MPI is initialized with this library, then it will remember
-- the provided level of thread support. (This might be less than the
-- requested level.)
threadSupport :: IO (Maybe ThreadSupport)
threadSupport = readIORef providedThreadSupport

providedThreadSupport :: IORef (Maybe ThreadSupport)
providedThreadSupport = unsafePerformIO (newIORef Nothing)



--------------------------------------------------------------------------------

-- | A null (invalid) communicator (@MPI_COMM_NULL@).
{#fun pure mpihs_get_comm_null as commNull {+} -> `Comm'#}

-- | The self communicator (@MPI_COMM_SELF@). Each process has its own
-- self communicator that includes only this process.
{#fun pure mpihs_get_comm_self as commSelf {+} -> `Comm'#}

-- | The world communicator, which includes all processes
-- (@MPI_COMM_WORLD@).
{#fun pure mpihs_get_comm_world as commWorld {+} -> `Comm'#}



-- | Error value returned by 'getCount' if the message is too large,
-- or if the message size is not an integer multiple of the provided
-- datatype (@MPI_UNDEFINED@).
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



-- | A null (invalid) reduction operation (@MPI_OP_NULL@).
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
-- any source (@MPI_ANY_SOURCE@). When calling 'probe' or 'recv' (or
-- 'iprobe' or 'irecv') with 'anySource' as source, the actual source
-- can be determined from the returned message status via 'getSource'.
{#fun pure mpihs_get_any_source as anySource {} -> `Rank' toRank#}



-- | A null (invalid) request (@MPI_REQUEST_NULL@).
{#fun pure mpihs_get_request_null as requestNull {+} -> `Request'#}



{#fun pure mpihs_get_status_ignore as statusIgnore {} -> `Status'#}

withStatusIgnore :: (Ptr Status -> IO a) -> IO a
withStatusIgnore = withStatus statusIgnore



-- | Tag placeholder to specify that a message can have any tag
-- (@MPI_ANY_TAG@). When calling 'probe' or 'recv' (or 'iprobe' or
-- 'irecv') with 'anyTag' as tag, the actual tag can be determined
-- from the returned message status via 'getTag'.
{#fun pure mpihs_get_any_tag as anyTag {} -> `Tag' toTag#}



--------------------------------------------------------------------------------

-- | Terminate MPI execution environment
-- (@[MPI_Abort](https://www.open-mpi.org/doc/current/man3/MPI_Abort.3.php)@).
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
-- (collective,
-- @[MPI_Allgather](https://www.open-mpi.org/doc/current/man3/MPI_Allgather.3.php)@).
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
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
-- (collective,
-- @[MPI_Allreduce](https://www.open-mpi.org/doc/current/man3/MPI_Allreduce.3.php)@).
-- The MPI datatype is determined automatically from the buffer
-- pointer types.
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

-- | Send data from all processes to all processes (collective,
-- @[MPI_Alltoall](https://www.open-mpi.org/doc/current/man3/MPI_Alltoall.php)@).
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

-- | Barrier (collective,
-- @[MPI_Barrier](https://www.open-mpi.org/doc/current/man3/MPI_Barrier.3.php)@).
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

-- | Broadcast data from one process to all processes (collective,
-- @[MPI_Bcast](https://www.open-mpi.org/doc/current/man3/MPI_Bcast.3.php)@).
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

-- | Compare two communicators
-- (@[MPI_Comm_compare](https://www.open-mpi.org/doc/current/man3/MPI_Comm_compare.3.php)@).
{#fun unsafe Comm_compare as ^
    { withComm* %`Comm'         -- ^ Communicator
    , withComm* %`Comm'         -- ^ Other communicator
    , alloca- `ComparisonResult' peekEnum*
    } -> `()' return*-#}

-- | Return this process's rank in a communicator
-- (@[MPI_Comm_rank](https://www.open-mpi.org/doc/current/man3/MPI_Comm_rank.3.php)@).
{#fun unsafe Comm_rank as ^
    { withComm* %`Comm'         -- ^ Communicator
    , alloca- `Rank' peekCoerce*
    } -> `()' return*-#}

-- | Return the number of processes in a communicator
-- (@[MPI_Comm_size](https://www.open-mpi.org/doc/current/man3/MPI_Comm_size.3.php)@).
{#fun unsafe Comm_size as ^
    { withComm* %`Comm'         -- ^ Communicator
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

-- | Reduce data from all processes via an exclusive (prefix) scan
-- (collective,
-- @[MPI_Exscan](https://www.open-mpi.org/doc/current/man3/MPI_Exscan.3.php)@).
-- Each process with rank @r@ receives the result of reducing data
-- from rank @0@ to rank @r-1@ (inclusive). Rank 0 should logically
-- receive a neutral element of the reduction operation, but instead
-- receives an undefined value since MPI is not aware of neutral
-- values for reductions.
--
-- The MPI datatype is determined automatically from the buffer
-- pointer type.
exscan :: forall a p q.
          ( Pointer p, Pointer q, Storable a, HasDatatype a)
       => p a                   -- ^ Source buffer
       -> q a                   -- ^ Destination buffer
       -> Count                 -- ^ Number of elements
       -> Op                    -- ^ Reduction operation
       -> Comm                  -- ^ Communicator
       -> IO ()
exscan sendbuf recvbuf count op comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  exscanTyped (castPtr sendbuf') (castPtr recvbuf') count (datatype @a) op comm

-- | Finalize (shut down) the MPI library (collective, @[MPI_Finalize](https://www.open-mpi.org/doc/current/man3/MPI_Finalize.3.php)@).
{#fun Finalize as ^ {} -> `()' return*-#}

-- | Return whether the MPI library has been finalized
-- (@[MPI_Finalized](https://www.open-mpi.org/doc/current/man3/MPI_Finalized.3.php)@).
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

-- | Gather data from all processes to the root process (collective,
-- @[MPI_Gather](https://www.open-mpi.org/doc/current/man3/MPI_Gather.3.php)@).
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
gather :: forall a b p q.
          ( Pointer p, Pointer q
          , Storable a, HasDatatype a, Storable b, HasDatatype b)
       => p a                   -- ^ Source buffer
       -> Count                 -- ^ Number of source elements
       -> q b  -- ^ Destination buffer (only used on the root process)
       -> Count -- ^ Number of destination elements (only used on the
                -- root process)
       -> Rank                  -- ^ Root rank
       -> Comm                  -- ^ Communicator
       -> IO ()
gather sendbuf sendcount recvbuf recvcount root comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  gatherTyped (castPtr sendbuf') sendcount (datatype @a)
              (castPtr recvbuf') recvcount (datatype @b)
              root comm

-- | Get the size of a message, in terms of objects of type 'Datatype'
-- (@[MPI_Get_count](https://www.open-mpi.org/doc/current/man3/MPI_Get_count.3.php)@).
-- To determine the MPI datatype for a given Haskell type, use
-- 'datatype' (call e.g. as 'datatype @CInt').
{#fun unsafe Get_count as ^
    { withStatus* `Status'      -- ^ Message status
    , withDatatype* %`Datatype' -- ^ MPI datatype
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

-- | Get the number of elements in message, in terms of sub-object of
-- the type 'datatype'
-- (@[MPI_Get_elements](https://www.open-mpi.org/doc/current/man3/MPI_Get_elements.3.php)@).
-- This is useful when a message contains partial objects of type
-- 'datatype'. To determine the MPI datatype for a given Haskell type,
-- use 'datatype' (call e.g. as 'datatype @CInt').
{#fun unsafe Get_elements as ^
    { withStatus* `Status'      -- ^ Message status
    , withDatatype* %`Datatype' -- ^ MPI datatype
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

{#fun unsafe Get_library_version as getLibraryVersion_
    { id `CString'
    , alloca- `Int' peekInt*
    } -> `()' return*-#}

-- | Return the version of the MPI library
-- (@[MPI_Get_library_version](https://www.open-mpi.org/doc/current/man3/MPI_Get_library_version.3.php)@).
-- Note that the version of the MPI standard that this library
-- implements is returned by 'getVersion'.
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

-- | Return the name of the current process
-- (@[MPI_Get_Processor_name](https://www.open-mpi.org/doc/current/man3/MPI_Get_processor_name.3.php)@).
-- This should uniquely identify the hardware on which this process is
-- running.
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

-- | Return the version of the MPI standard implemented by this
-- library
-- (@[MPI_Get_version](https://www.open-mpi.org/doc/current/man3/MPI_Get_version.3.php)@).
-- Note that the version of the MPI library itself is returned by
-- 'getLibraryVersion'.
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

-- | Begin to gather data from all processes and broadcast the result,
-- and return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Iallgather](https://www.open-mpi.org/doc/current/man3/MPI_Iallgather.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
iallgather :: forall a b p q.
              ( Pointer p, Pointer q
              , Storable a, HasDatatype a, Storable b, HasDatatype b)
           => p a               -- ^ Source buffer
           -> Count             -- ^ Number of source elements
           -> q b               -- ^ Destination buffer
           -> Count             -- ^ Number of destination elements
           -> Comm              -- ^ Communicator
           -> IO Request        -- ^ Communication request
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

-- | Begin to reduce data from all processes and broadcast the result,
-- and return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Iallreduce](https://www.open-mpi.org/doc/current/man3/MPI_Iallreduce.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatype is determined automatically from the buffer
-- pointer types.
iallreduce :: forall a p q.
              ( Pointer p, Pointer q, Storable a, HasDatatype a)
           => p a               -- ^ Source buffer
           -> q a               -- ^ Destination buffer
           -> Count             -- ^ Number of elements
           -> Op                -- ^ Reduction operation
           -> Comm              -- ^ Communicator
           -> IO Request        -- ^ Communication request
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

-- | Begin to send data from all processes to all processes, and
-- return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Ialltoall](https://www.open-mpi.org/doc/current/man3/MPI_Ialltoall.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
ialltoall :: forall a b p q.
             ( Pointer p, Pointer q
             , Storable a, HasDatatype a, Storable b, HasDatatype b)
          => p a                -- ^ Source buffer
          -> Count              -- ^ Number of source elements
          -> q b                -- ^ Destination buffer
          -> Count              -- ^ Number of destination elements
          -> Comm               -- ^ Communicator
          -> IO Request         -- ^ Communication request
ialltoall sendbuf sendcount recvbuf recvcount comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  ialltoallTyped (castPtr sendbuf') sendcount (datatype @a)
                 (castPtr recvbuf') recvcount (datatype @b)
                 comm

-- | Start a barrier, and return a handle to the communication request
-- (collective, non-blocking,
-- @[MPI_Ibarrier](https://www.open-mpi.org/doc/current/man3/MPI_Ibarrier.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
{#fun Ibarrier as ^
    { withComm* %`Comm'         -- ^ Communicator
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

-- | Begin to broadcast data from one process to all processes, and
-- return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Ibcast](https://www.open-mpi.org/doc/current/man3/MPI_Ibcast.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatype is determined automatically from the buffer
-- pointer type.
ibcast :: forall a p. (Pointer p, Storable a, HasDatatype a)
       => p a -- ^ Buffer pointer (read on the root process, written on
              -- all other processes)
       -> Count                 -- ^ Number of elements
       -> Rank                  -- ^ Root rank (sending process)
       -> Comm                  -- ^ Communicator
       -> IO Request            -- ^ Communication request
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

-- | Begin to reduce data from all processes via an exclusive (prefix)
-- scan, and return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Iexscan](https://www.open-mpi.org/doc/current/man3/MPI_Iexscan.3.php)@).
-- Each process with rank @r@ receives the result of reducing data
-- from rank @0@ to rank @r-1@ (inclusive). Rank 0 should logically
-- receive a neutral element of the reduction operation, but instead
-- receives an undefined value since MPI is not aware of neutral
-- values for reductions.
--
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatype is determined automatically from the buffer
-- pointer type.
iexscan :: forall a p q.
           ( Pointer p, Pointer q, Storable a, HasDatatype a)
        => p a                  -- ^ Source buffer
        -> q a                  -- ^ Destination buffer
        -> Count                -- ^ Number of elements
        -> Op                   -- ^ Reduction operation
        -> Comm                 -- ^ Communicator
        -> IO Request           -- ^ Communication request
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

-- | Begin to gather data from all processes to the root process, and
-- return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Igather](https://www.open-mpi.org/doc/current/man3/MPI_Igather.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
igather :: forall a b p q.
           ( Pointer p, Pointer q
           , Storable a, HasDatatype a, Storable b, HasDatatype b)
        => p a                  -- ^ Source buffer
        -> Count                -- ^ Number of source elements
        -> q b                  -- ^ Destination buffer (relevant only
                                -- on the root process)
        -> Count                -- ^ Number of destination elements
                                -- (relevant only on the root process)
        -> Rank                 -- ^ Root rank
        -> Comm                 -- ^ Communicator
        -> IO Request           -- ^ Communication request
igather sendbuf sendcount recvbuf recvcount root comm =
  withPtr sendbuf $ \sendbuf' ->
  withPtr recvbuf $ \recvbuf' ->
  igatherTyped (castPtr sendbuf') sendcount (datatype @a)
               (castPtr recvbuf') recvcount (datatype @b)
               root comm

-- | Return whether the MPI library has been initialized
-- (@[MPI_Initialized](https://www.open-mpi.org/doc/current/man3/MPI_Initialized.3.php)@).
{#fun unsafe Initialized as ^ {alloca- `Bool' peekBool*} -> `()' return*-#}

{#fun Init as init_
    { with* `CInt'
    , with* `Ptr CString'
    } -> `()' return*-#}

-- | Initialize the MPI library (collective,
-- @[MPI_Init](https://www.open-mpi.org/doc/current/man3/MPI_Init.3.php)@).
-- This corresponds to calling 'initThread' 'ThreadSingle'.
init :: IO ()
init = do init_ argc argv
          writeIORef providedThreadSupport (Just ThreadSingle)

{#fun Init_thread as initThread_
    { with* `CInt'
    , with* `Ptr CString'
    , fromEnum `ThreadSupport'
    , alloca- `ThreadSupport' peekEnum*
    } -> `()' return*-#}

-- | Initialize the MPI library (collective,
-- @[MPI_Init_thread](https://www.open-mpi.org/doc/current/man3/MPI_Init_thread.3.php)@).
-- Note that the provided level of thread support might be less than
-- (!) the required level.
initThread :: ThreadSupport    -- ^ required level of thread support
           -> IO ThreadSupport -- ^ provided level of thread support
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

-- | Probe (check) for incoming messages without waiting
-- (non-blocking,
-- @[MPI_Iprobe](https://www.open-mpi.org/doc/current/man3/MPI_Iprobe.3.php)@).
iprobe :: Rank                  -- ^ Source rank (may be 'anySource')
       -> Tag                   -- ^ Message tag (may be 'anyTag')
       -> Comm                  -- ^ Communicator
       -> IO (Maybe Status) -- ^ 'Just' 'Status' of the message if a
                            -- message is available, else 'Nothing'
iprobe rank tag comm = bool2maybe <$> iprobeBool rank tag comm

-- | Probe (check) for an incoming message without waiting
-- (@[MPI_Iprobe](https://www.open-mpi.org/doc/current/man3/MPI_Iprobe.3.php)@).
-- This function does not return a status, which might be more
-- efficient if the status is not needed.
iprobe_ :: Rank                 -- ^ Source rank (may be 'anySource')
        -> Tag                  -- ^ Message tag (may be 'anyTag')
        -> Comm                 -- ^ Communicator
        -> IO Bool              -- ^ Whether a message is available
iprobe_ rank tag comm =
  withComm comm $ \comm' ->
  do withStatusIgnore $ \st ->
       do alloca $ \flag ->
            do _ <- {#call mpihs_iprobe as iprobe__#}
                    (fromRank rank) (fromTag tag) comm' flag st
               peekBool flag

{#fun Irecv as irecvTyped
    { id `Ptr ()'
    , fromCount `Count'
    , withDatatype* %`Datatype'
    , fromRank `Rank'
    , fromTag `Tag'
    , withComm* %`Comm'
    , +
    } -> `Request' return*#}

-- | Begin to receive a message, and return a handle to the
-- communication request (non-blocking,
-- @[MPI_Irecv](https://www.open-mpi.org/doc/current/man3/MPI_Irecv.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatype is determined automatically from the buffer
-- pointer type.
irecv :: forall a p. (Pointer p, Storable a, HasDatatype a)
      => p a                    -- ^ Receive buffer
      -> Count                  -- ^ Number of elements to receive
      -> Rank                   -- ^ Source rank (may be 'anySource')
      -> Tag                    -- ^ Message tag (may be 'anyTag')
      -> Comm                   -- ^ Communicator
      -> IO Request             -- ^ Communication request
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

-- | Begin to reduce data from all processes, and return a handle to
-- the communication request (collective, non-blocking,
-- @[MPI_Ireduce](https://www.open-mpi.org/doc/current/man3/MPI_Ireduce.3.php)@).
-- The result is only available on the root process. The request must
-- be freed by calling 'test', 'wait', or similar. The MPI datatypes
-- are determined automatically from the buffer pointer types.
ireduce :: forall a p q.
           ( Pointer p, Pointer q, Storable a, HasDatatype a)
        => p a                  -- ^ Source buffer
        -> q a                  -- ^ Destination buffer
        -> Count                -- ^ Number of elements
        -> Op                   -- ^ Reduction operation
        -> Rank                 -- ^ Root rank
        -> Comm                 -- ^ Communicator
        -> IO Request           -- ^ Communication request
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

-- | Begin to reduce data from all processes via an (inclusive) scan,
-- and return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Iscan](https://www.open-mpi.org/doc/current/man3/MPI_Iscan.3.php)@).
-- Each process with rank @r@ receives the result of reducing data
-- from rank @0@ to rank @r@ (inclusive). The request must be freed by
-- calling 'test', 'wait', or similar. The MPI datatype is determined
-- automatically from the buffer pointer type.
iscan :: forall a p q.
         ( Pointer p, Pointer q, Storable a, HasDatatype a)
      => p a                    -- ^ Source buffer
      -> q a                    -- ^ Destination buffer
      -> Count                  -- ^ Number of elements
      -> Op                     -- ^ Reduction operation
      -> Comm                   -- ^ Communicator
      -> IO Request             -- ^ Communication request
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

-- | Begin to scatter data from the root process to all processes, and
-- return a handle to the communication request (collective,
-- non-blocking,
-- @[MPI_Iscatter](https://www.open-mpi.org/doc/current/man3/MPI_Iscatter.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
iscatter :: forall a b p q.
            ( Pointer p, Pointer q
            , Storable a, HasDatatype a, Storable b, HasDatatype b)
         => p a     -- ^ Source buffer (only used on the root process)
         -> Count -- ^ Number of source elements (only used on the root process)
         -> q b                 -- ^ Destination buffer
         -> Count               -- ^ Number of destination elements
         -> Rank                -- ^ Root rank
         -> Comm                -- ^ Communicator
         -> IO Request          -- ^ Communication request
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

-- | Begin to send a message, and return a handle to the
-- communication request (non-blocking,
-- @[MPI_Isend](https://www.open-mpi.org/doc/current/man3/MPI_Isend.3.php)@).
-- The request must be freed by calling 'test', 'wait', or similar.
-- The MPI datatype is determined automatically from the buffer
-- pointer type.
isend :: forall a p. (Pointer p, Storable a, HasDatatype a)
      => p a                    -- ^ Send buffer
      -> Count                  -- ^ Number of elements to send
      -> Rank                   -- ^ Destination rank
      -> Tag                    -- ^ Message tag
      -> Comm                   -- ^ Communicator
      -> IO Request             -- ^ Communication request
isend sendbuf sendcount sendrank sendtag comm =
  withPtr sendbuf $ \sendbuf' ->
  isendTyped (castPtr sendbuf') sendcount (datatype @a) sendrank sendtag comm

-- | Probe (wait) for an incoming message
-- (@[MPI_Probe](https://www.open-mpi.org/doc/current/man3/MPI_Probe.3.php)@).
{#fun Probe as ^
    { fromRank `Rank'           -- ^ Source rank (may be 'anySource')
    , fromTag `Tag'             -- ^ Message tag (may be 'anyTag')
    , withComm* %`Comm'         -- ^ Communicator
    , +
    } -> `Status' return*       -- ^ Message status
#}

-- | Probe (wait) for an incoming message
-- (@[MPI_Probe](https://www.open-mpi.org/doc/current/man3/MPI_Probe.3.php)@).
-- This function does not return a status, which might be more
-- efficient if the status is not needed.
{#fun Probe as probe_
    { fromRank `Rank'           -- ^ Source rank (may be 'anySource')
    , fromTag `Tag'             -- ^ Message tag (may be 'anyTag')
    , withComm* %`Comm'         -- ^ Communicator
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

-- | Receive a message
-- (@[MPI_Recv](https://www.open-mpi.org/doc/current/man3/MPI_Recv.3.php)@).
-- The MPI datatypeis determined automatically from the buffer
-- pointer type.
recv :: forall a p. (Pointer p, Storable a, HasDatatype a)
     => p a                     -- ^ Receive buffer
     -> Count                   -- ^ Number of elements to receive
     -> Rank                    -- ^ Source rank (may be 'anySource')
     -> Tag                     -- ^ Message tag (may be 'anyTag')
     -> Comm                    -- ^ Communicator
     -> IO Status               -- ^ Message status
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

-- | Receive a message
-- (@[MPI_Recv](https://www.open-mpi.org/doc/current/man3/MPI_Recv.3.php)@).
-- The MPI datatype is determined automatically from the buffer
-- pointer type. This function does not return a status, which might
-- be more efficient if the status is not needed.
recv_ :: forall a p. (Pointer p, Storable a, HasDatatype a)
      => p a                    -- ^ Receive buffer
      -> Count                  -- ^ Number of elements to receive
      -> Rank                   -- ^ Source rank (may be 'anySource')
      -> Tag                    -- ^ Message tag (may be 'anyTag')
      -> Comm                   -- ^ Communicator
      -> IO ()
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

-- | Reduce data from all processes (collective,
-- @[MPI_Reduce](https://www.open-mpi.org/doc/current/man3/MPI_Reduce.3.php)@).
-- The result is only available on the root process. The MPI datatypes
-- are determined automatically from the buffer pointer types.
reduce :: forall a p q.
          ( Pointer p, Pointer q, Storable a, HasDatatype a)
       => p a                   -- ^ Source buffer
       -> q a                   -- ^ Destination buffer
       -> Count                 -- ^ Number of elements
       -> Op                    -- ^ Reduction operation
       -> Rank                  -- ^ Root rank
       -> Comm                  -- ^ Communicator
       -> IO ()
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

-- | Reduce data from all processes via an (inclusive) scan
--  (collective,
--  @[MPI_Scan](https://www.open-mpi.org/doc/current/man3/MPI_Scan.3.php)@).
--  Each process with rank @r@ receives the result of reducing data
--  from rank @0@ to rank @r@ (inclusive). The MPI datatype is
--  determined automatically from the buffer pointer type.
scan :: forall a p q.
        ( Pointer p, Pointer q, Storable a, HasDatatype a)
     => p a                     -- ^ Source buffer
     -> q a                     -- ^ Destination buffer
     -> Count                   -- ^ Number of elements
     -> Op                      -- ^ Reduction operation
     -> Comm                    -- ^ Communicator
     -> IO ()
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

-- | Scatter data from the root process to all processes (collective,
-- @[MPI_Scatter](https://www.open-mpi.org/doc/current/man3/MPI_Scatter.3.php)@).
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
scatter :: forall a b p q.
           ( Pointer p, Pointer q
           , Storable a, HasDatatype a, Storable b, HasDatatype b)
        => p a      -- ^ Source buffer (only used on the root process)
        -> Count -- ^ Number of source elements (only used on the root process)
        -> q b                  -- ^ Destination buffer
        -> Count                -- ^ Number of destination elements
        -> Rank                 -- ^ Root rank
        -> Comm                 -- ^ Communicator
        -> IO ()
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

-- | Send a message
-- (@[MPI_Send](https://www.open-mpi.org/doc/current/man3/MPI_Send.3.php)@).
-- The MPI datatype is determined automatically from the buffer
-- pointer type.
send :: forall a p. (Pointer p, Storable a, HasDatatype a)
     => p a                     -- ^ Send buffer
     -> Count                   -- ^ Number of elements to send
     -> Rank                    -- ^ Destination rank
     -> Tag                     -- ^ Message tag
     -> Comm                    -- ^ Communicator
     -> IO ()
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

-- | Send and receive a message with a single call
-- (@[MPI_Sendrecv](https://www.open-mpi.org/doc/current/man3/MPI_Sendrecv.3.php)@).
-- The MPI datatypes are determined automatically from the buffer
-- pointer types.
sendrecv :: forall a b p q.
            ( Pointer p, Pointer q
            , Storable a, HasDatatype a, Storable b, HasDatatype b)
         => p a                 -- ^ Send buffer
         -> Count               -- ^ Number of elements to send
         -> Rank                -- ^ Destination rank
         -> Tag                 -- ^ Sent message tag
         -> q a                 -- ^ Receive buffer
         -> Count               -- ^ Number of elements to receive
         -> Rank                -- ^ Source rank (may be 'anySource')
         -> Tag                 -- ^ Received message tag (may be 'anyTag')
         -> Comm                -- ^ Communicator
         -> IO Status           -- ^ Status for received message
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

-- | Send and receive a message with a single call
-- (@[MPI_Sendrecv](https://www.open-mpi.org/doc/current/man3/MPI_Sendrecv.3.php)@).
-- The MPI datatypes are determined automatically from the buffer
-- pointer types. This function does not return a status, which might
-- be more efficient if the status is not needed.
sendrecv_ :: forall a b p q.
             ( Pointer p, Pointer q
             , Storable a, HasDatatype a, Storable b, HasDatatype b)
          => p a                -- ^ Send buffer
          -> Count              -- ^ Number of elements to send
          -> Rank               -- ^ Destination rank
          -> Tag                -- ^ Sent message tag
          -> q a                -- ^ Receive buffer
          -> Count              -- ^ Number of elements to receive
          -> Rank               -- ^ Source rank (may be 'anySource')
          -> Tag                -- ^ Received message tag (may be 'anyTag')
          -> Comm               -- ^ Communicator
          -> IO ()
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

-- | Check whether a communication has completed, and free the
-- communication request if so
-- (@[MPI_Test](https://www.open-mpi.org/doc/current/man3/MPI_Test.3.php)@).
test :: Request           -- ^ Communication request
     -> IO (Maybe Status) -- ^ 'Just' 'Status' if the request has completed,
                          -- else 'Nothing'
test req = bool2maybe <$> testBool req

-- {#fun Test as test_
--     { withRequest* `Request'
--     , alloca- `Bool' peekBool*
--     , withStatusIgnore- `Status'
--     } -> `()' return*-#}

-- | Check whether a communication has completed, and free the
-- communication request if so
-- (@[MPI_Test](https://www.open-mpi.org/doc/current/man3/MPI_Test.3.php)@).
-- This function does not return a status, which might be more
-- efficient if the status is not needed.
test_ :: Request                -- ^ Communication request
      -> IO Bool                -- ^ Whether the request had completed
test_ req =
  withRequest req $ \req' ->
  alloca $ \flag ->
  withStatusIgnore $ \st ->
  do _ <- {#call Test as test__#} req' flag st
     peekBool flag

-- | Wait for a communication request to complete, then free the
--  request
--  (@[MPI_Wait](https://www.open-mpi.org/doc/current/man3/MPI_Wait.3.php)@).
{#fun Wait as ^
    { withRequest* `Request'    -- ^ Communication request
    , +
    } -> `Status' return*       -- ^ Message status
#}

-- | Wait for a communication request to complete, then free the
--  request
--  (@[MPI_Wait](https://www.open-mpi.org/doc/current/man3/MPI_Wait.3.php)@).
-- This function does not return a status, which might be more
-- efficient if the status is not needed.
{#fun Wait as wait_
    { withRequest* `Request'    -- ^ Communication request
    , withStatusIgnore- `Status'
    } -> `()' return*-#}

-- | Wall time tick (accuracy of 'wtime') (in seconds)
{#fun unsafe Wtick as ^ {} -> `Double'#}

-- | Current wall time (in seconds)
{#fun unsafe Wtime as ^ {} -> `Double'#}

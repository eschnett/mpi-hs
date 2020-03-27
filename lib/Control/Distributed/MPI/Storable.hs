{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Module: Control.Distributed.MPI.Storable
-- Description: Simplified MPI bindings with automatic serialization
--              based on Foreign.Storable
-- Copyright: (C) 2019 Erik Schnetter
-- License: Apache-2.0
-- Maintainer: Erik Schnetter <schnetter@gmail.com>
-- Stability: experimental
-- Portability: Requires an externally installed MPI library

module Control.Distributed.MPI.Storable
  ( -- * Types, and associated functions and constants
    MPIException(..)

    -- ** Communicators
  , Comm(..)
  , commSelf
  , commWorld

    -- ** Message sizes
  , Count(..)
  , fromCount
  , toCount

    -- ** Process ranks
  , Rank(..)
  , anySource
  , commRank
  , commSize
  , fromRank
  , rootRank
  , toRank

    -- ** Message status
  , Status(..)

    -- ** Message tags
  , Tag(..)
  , anyTag
  , fromTag
  , toTag
  , unitTag

  , Request

    -- * Functions

    -- ** Initialization and shutdown
  , abort
  , mainMPI

    -- ** Point-to-point (blocking)
  , recv
  , recv_
  , send
  , sendrecv
  , sendrecv_

    -- ** Point-to-point (non-blocking)
  , irecv
  , isend
  , test
  , test_
  , wait
  , wait_

    -- ** Collective (blocking)
  , barrier
  , bcast
  , bcastRecv
  , bcastSend
  , bcastSend_

    -- ** Collective (non-blocking)
  , ibarrier
  , ibcast
  , ibcastRecv
  , ibcastSend
  , ibcastSend_
  ) where

import Prelude hiding (init)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Maybe
import Data.Typeable
import Foreign
import Foreign.C.Types
import qualified Foreign.Storable as Storable

import qualified Control.Distributed.MPI as MPI
import Control.Distributed.MPI
  ( Comm(..)
  , commSelf
  , commWorld
  , Count(..)
  , fromCount
  , toCount
  , Rank(..)
  , anySource
  , commRank
  , commSize
  , fromRank
  , rootRank
  , toRank
  , Tag(..)
  , anyTag
  , fromTag
  , toTag
  , unitTag
  , abort
  , barrier
  )



-- Serialization, based on Data.Binary
type CanSerialize a = Storable.Storable a
serialize :: CanSerialize a => a -> IO B.ByteString
serialize x = do let len = Storable.sizeOf x
                 ptr <- malloc
                 Storable.poke ptr x
                 B.unsafePackMallocCStringLen (castPtr ptr, len)
deserialize :: CanSerialize a => B.ByteString -> IO a
deserialize bs = B.unsafeUseAsCStringLen bs $ \(ptr, _) -> peek (castPtr ptr)



-- | Run the supplied Maybe computation repeatedly while it returns
-- Nothing. If it returns a value, then returns that value.
whileNothing :: Monad m => m (Maybe a) -> m () -> m a
whileNothing cond loop = go
  where go = do mx <- cond
                case mx of
                  Nothing -> do loop
                                go
                  Just x -> return x



-- | Exception type indicating an error in a call to MPI
newtype MPIException = MPIException String
  deriving (Eq, Ord, Read, Show, Typeable)
instance Exception MPIException

mpiAssert :: Bool -> String -> IO ()
mpiAssert cond msg =
  do when (not cond) $ throw (MPIException msg)
     return ()



data DidInit = DidInit | DidNotInit

initMPI :: IO DidInit
initMPI =
  do isInit <- MPI.initialized
     if isInit
       then return DidNotInit
       else do ts <- MPI.initThread MPI.ThreadMultiple
               mpiAssert (ts >= MPI.ThreadMultiple)
                 ("MPI.init: Insufficient thread support: requiring " ++
                  show MPI.ThreadMultiple ++
                  ", but MPI library provided only " ++ show ts)
               return DidInit

finalizeMPI :: DidInit -> IO ()
finalizeMPI DidInit =
  do isFinalized <- MPI.finalized
     if isFinalized
       then return ()
       else do MPI.finalize
finalizeMPI DidNotInit = return ()

-- | Convenience function to initialize and finalize MPI. This
-- initializes MPI with 'ThreadMultiple' thread support.
mainMPI :: IO () -- ^ action to run with MPI, typically the whole program
        -> IO ()
mainMPI action = bracket initMPI finalizeMPI (\_ -> action)



-- | A communication request, usually created by a non-blocking
-- communication function.
newtype Request a = Request (MVar (Status, a))

-- | The status of a finished communication, indicating rank and tag
-- of the other communication end point.
data Status = Status { msgRank :: !Rank
                     , msgTag :: !Tag
                     }
  deriving (Eq, Ord, Read, Show)



-- | Receive an object.
recv :: CanSerialize a
     => Rank                    -- ^ Source rank
     -> Tag                     -- ^ Source tag
     -> Comm                    -- ^ Communicator
     -> IO (Status, a)          -- ^ Message status and received object
recv recvrank recvtag comm =
  do status <- whileNothing (MPI.iprobe recvrank recvtag comm) yield
     source <- MPI.getSource status
     tag <- MPI.getTag status
     count <- MPI.getCount status MPI.datatypeByte
     let len = MPI.fromCount count
     ptr <- mallocBytes len
     buffer <- B.unsafePackMallocCStringLen (ptr, len)
     req <- MPI.irecv buffer source tag comm
     whileM_ (not <$> MPI.test_ req) yield
     recvobj <- deserialize buffer
     return (Status source tag, recvobj)

-- | Receive an object without returning a status.
recv_ :: CanSerialize a
      => Rank                   -- ^ Source rank
      -> Tag                    -- ^ Source tag
      -> Comm                   -- ^ Communicator
      -> IO a                   -- ^ Received object
recv_ recvrank recvtag comm =
  snd <$> recv recvrank recvtag comm

-- | Send an object.
send :: CanSerialize a
     => a                     -- ^ Object to send
     -> Rank                  -- ^ Destination rank
     -> Tag                   -- ^ Message tag
     -> Comm                  -- ^ Communicator
     -> IO ()
send sendobj sendrank sendtag comm =
  do sendbuf <- serialize sendobj
     -- Use 'unsafeUseAsCStringLen' to ensure 'sendbuf' is not freed
     -- too early
     B.unsafeUseAsCStringLen sendbuf $ \_ ->
       do req <- MPI.isend sendbuf sendrank sendtag comm
          whileM_ (not <$> MPI.test_ req) yield

-- | Send and receive objects simultaneously.
sendrecv :: (CanSerialize a, CanSerialize b)
         => a                   -- ^ Object to send
         -> Rank                -- ^ Destination rank
         -> Tag                 -- ^ Send message tag
         -> Rank                -- ^ Source rank
         -> Tag                 -- ^ Receive message tag
         -> Comm                -- ^ Communicator
         -> IO (Status, b)      -- ^ Message status and received object
sendrecv sendobj sendrank sendtag recvrank recvtag comm =
  do recvreq <- irecv recvrank recvtag comm
     send sendobj sendrank sendtag comm
     wait recvreq

-- | Send and receive objects simultaneously, without returning a
-- status for the received message.
sendrecv_ :: (CanSerialize a, CanSerialize b)
          => a                  -- ^ Object to send
          -> Rank               -- ^ Destination rank
          -> Tag                -- ^ Send message tag
          -> Rank               -- ^ Source rank
          -> Tag                -- ^ Receive message tag
          -> Comm               -- ^ Communicator
          -> IO b               -- ^ Received object
sendrecv_ sendobj sendrank sendtag recvrank recvtag comm =
  snd <$> sendrecv sendobj sendrank sendtag recvrank recvtag comm

-- | Begin to receive an object. Call `test` or `wait` to finish the
-- communication, and to obtain the received object.
irecv :: CanSerialize a
      => Rank                   -- ^ Source rank
      -> Tag                    -- ^ Source tag
      -> Comm                   -- ^ Communicator
      -> IO (Request a)         -- ^ Communication request
irecv recvrank recvtag comm =
  do result <- newEmptyMVar
     _ <- forkIO $
       do res <- recv recvrank recvtag comm
          putMVar result res
     return (Request result)

-- | Begin to send an object. Call 'test' or 'wait' to finish the
-- communication.
isend :: CanSerialize a
      => a                     -- ^ Object to send
      -> Rank                  -- ^ Destination rank
      -> Tag                   -- ^ Message tag
      -> Comm                  -- ^ Communicator
      -> IO (Request ())       -- ^ Communication request
isend sendobj sendrank sendtag comm =
  do result <- newEmptyMVar
     _ <- forkIO $ do send sendobj sendrank sendtag comm
                      putMVar result (Status sendrank sendtag, ())
     return (Request result)

-- | Check whether a communication has finished, and return the
-- communication result if so.
test :: Request a               -- ^ Communication request
     -> IO (Maybe (Status, a))  -- ^ 'Just' communication result, if
                                -- communication has finished, else 'Nothing'
test (Request result) = tryTakeMVar result

-- | Check whether a communication has finished, and return the
-- communication result if so, without returning a message status.
test_ :: Request a       -- ^ Communication request
      -> IO (Maybe a)    -- ^ 'Just' communication result, if
                         -- communication has finished, else 'Nothing'
test_ req = fmap snd <$> test req

-- | Wait for a communication to finish and return the communication
-- result.
wait :: Request a               -- ^ Communication request
     -> IO (Status, a)          -- ^ Message status and communication result
wait (Request result) = takeMVar result

-- | Wait for a communication to finish and return the communication
-- result, without returning a message status.
wait_ :: Request a              -- ^ Communication request
      -> IO a                   -- ^ Communication result
wait_ req = snd <$> wait req



-- | Broadcast a message from one process (the "root") to all other
-- processes in the communicator. The send object must be present
-- (`Just`) on the root, and is ignored on all non-root processes.
bcast :: CanSerialize a
      => Maybe a
      -> Rank
      -> Comm
      -> IO a
bcast sendobj root comm =
  do rank <- MPI.commRank comm
     if rank == root
       then do mpiAssert (isJust sendobj) "bcast: expected send object on root"
               bcastSend (fromJust sendobj) root comm
       else bcastRecv root comm

-- | Broadcast a message from one process (the "root") to all other
-- processes in the communicator. Call this function on all non-root
-- processes. Call 'bcastSend' instead on the root process.
bcastRecv :: CanSerialize a
          => Rank
          -> Comm
          -> IO a
bcastRecv root comm =
  do rank <- MPI.commRank comm
     mpiAssert (rank /= root) "bcastRecv: expected rank /= root"
     lenbuf <- mallocForeignPtr @CInt
     lenreq <- MPI.ibcast (lenbuf, 1::Int) root comm
     whileM_ (not <$> MPI.test_ lenreq) yield
     len <- withForeignPtr lenbuf peek
     ptr <- mallocBytes (fromIntegral len)
     recvbuf <- B.unsafePackMallocCStringLen (ptr, fromIntegral len)
     req <- MPI.ibcast recvbuf root comm               
     whileM_ (not <$> MPI.test_ req) yield
     recvobj <- deserialize recvbuf
     return recvobj

-- | Broadcast a message from one process (the "root") to all other
-- processes in the communicator. Call this function on the root
-- process. Call 'bcastRecv' instead on all non-root processes.
bcastSend_ :: CanSerialize a
           => a
           -> Rank
           -> Comm
           -> IO ()
bcastSend_ sendobj root comm =
  do rank <- MPI.commRank comm
     mpiAssert (rank == root) "bcastSend: expected rank == root"
     sendbuf <- serialize sendobj
     lenbuf <- mallocForeignPtr @CInt
     withForeignPtr lenbuf $ \ptr -> poke ptr (fromIntegral (B.length sendbuf))
     lenreq <- MPI.ibcast (lenbuf, 1::Int) root comm
     whileM_ (not <$> MPI.test_ lenreq) yield
     req <- MPI.ibcast sendbuf root comm
     whileM_ (not <$> MPI.test_ req) yield

-- | Broadcast a message from one process (the "root") to all other
-- processes in the communicator. Call this function on the root
-- process. Call 'bcastRecv' instead on all non-root processes.
bcastSend :: CanSerialize a
          => a
          -> Rank
          -> Comm
          -> IO a
bcastSend sendobj root comm =
  do bcastSend_ sendobj root comm
     return sendobj

-- | Begin a barrier. Call 'test' or 'wait' to finish the
-- communication.
ibarrier :: Comm
         -> IO (Request ())
ibarrier comm =
  do result <- newEmptyMVar
     _ <- forkIO $
       do req <- MPI.ibarrier comm
          whileM_ (not <$> MPI.test_ req) yield
          putMVar result (Status MPI.anySource MPI.anyTag, ())
     return (Request result)

ibcast :: CanSerialize a
       => Maybe a
       -> Rank
       -> Comm
       -> IO (Request a)
ibcast sendobj root comm =
  do rank <- MPI.commRank comm
     if rank == root
       then do mpiAssert (isJust sendobj) "ibcast: expected send object on root"
               ibcastSend (fromJust sendobj) root comm
       else ibcastRecv root comm

ibcastRecv :: CanSerialize a
           => Rank
           -> Comm
           -> IO (Request a)
ibcastRecv root comm =
  do result <- newEmptyMVar
     _ <- forkIO $
       do recvobj <- bcastRecv root comm
          putMVar result (Status root MPI.anyTag, recvobj)
     return (Request result)

ibcastSend :: CanSerialize a
           => a
           -> Rank
           -> Comm
           -> IO (Request a)
ibcastSend sendobj root comm =
  do result <- newEmptyMVar
     _ <- forkIO $
       do bcastSend_ sendobj root comm
          putMVar result (Status root MPI.anyTag, sendobj)
     return (Request result)

ibcastSend_ :: CanSerialize a
            => a
            -> Rank
            -> Comm
            -> IO (Request ())
ibcastSend_ sendobj root comm =
  do result <- newEmptyMVar
     _ <- forkIO $
       do bcastSend_ sendobj root comm
          putMVar result (Status root MPI.anyTag, ())
     return (Request result)

-- scatter :: CanSerialize a
--         => Maybe a
--         -> Rank
--         -> Comm
--         -> IO (Request a)
-- scatter msendobj root comm =
--   do rank <- commRank comm
--      sendbuf <-
--        if (rank == root)
--        then do mpiAssert (isJust msendobj)
--                serialize (fromJust sendobj)
--        else return nullPtr
--      lenbuf <- mallocForeignPtr @CInt
--      withForeignPtr lenbuf $ \ptr -> poke ptr (fromIntegral (B.length sendbuf))
--      lenreq <- MPI.ibcast (lenbuf, 1::Int) root comm
--      whileM_ (not <$> MPI.test_ lenreq) yield
--      req <- MPI.iscatterv sendbuf root comm
--      whileM_ (not <$> MPI.test_ req) yield
-- 
-- scatterRecv :: CanSerialize a
--             => Rank
--             -> Comm
--             -> IO (Request a)
-- 
-- scatterSend :: CanSerialize a
--             => a
--             -> Rank
--             -> Comm
--             -> IO (Request a)

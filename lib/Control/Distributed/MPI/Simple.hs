{-# LANGUAGE TypeApplications #-}

-- | Module: Control.Distributed.MPI.Simple
-- Description: Simplified MPI bindings with automatic serialization
-- Copyright: (C) 2018 Erik Schnetter
-- License: Apache-2.0
-- Maintainer: Erik Schnetter <schnetter@gmail.com>
-- Stability: experimental
-- Portability: Requires an externally installed MPI library

module Control.Distributed.MPI.Simple
  ( -- * Types, and associated functions constants
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
  , rootRank

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
  , bcastRecv
  , bcastSend

    -- ** Collective (non-blocking)
  , ibarrier
  ) where

import Prelude hiding (init)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Store hiding (peek, poke)
import Data.Typeable
import Foreign
import Foreign.C.Types

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
  , rootRank
  , Tag(..)
  , anyTag
  , fromTag
  , toTag
  , unitTag
  , abort
  , barrier
  )



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
recv :: Store a
     => Rank                    -- ^ Source rank
     -> Tag                     -- ^ Source tag
     -> Comm                    -- ^ Communicator
     -> IO (Status, a)          -- ^ Message status and received object
recv recvrank recvtag comm =
  do status <- untilJust $
       do yield
          MPI.iprobe recvrank recvtag comm
     source <- MPI.getSource status
     tag <- MPI.getTag status
     count <- MPI.getCount status MPI.datatypeByte
     let len = MPI.fromCount count
     ptr <- mallocBytes len
     buffer <- B.unsafePackMallocCStringLen (ptr, len)
     req <- MPI.irecv buffer source tag comm
     whileM_ (not <$> MPI.test_ req) yield
     return (Status source tag, decodeEx buffer)

-- | Receive an object without returning a status.
recv_ :: Store a
      => Rank                   -- ^ Source rank
      -> Tag                    -- ^ Source tag
      -> Comm                   -- ^ Communicator
      -> IO a                   -- ^ Received object
recv_ recvrank recvtag comm =
  snd <$> recv recvrank recvtag comm

-- | Send an object.
send :: Store a
     => a                     -- ^ Object to send
     -> Rank                  -- ^ Destination rank
     -> Tag                   -- ^ Message tag
     -> Comm                  -- ^ Communicator
     -> IO ()
send sendobj sendrank sendtag comm =
  do let sendbuf = encode sendobj
     MPI.send sendbuf sendrank sendtag comm

-- | Send and receive objects simultaneously.
sendrecv :: (Store a, Store b)
         => a                   -- ^ Object to send
         -> Rank                -- ^ Destination rank
         -> Tag                 -- ^ Send message tag
         -> Rank                -- ^ Source rank
         -> Tag                 -- ^ Receive message tag
         -> Comm                -- ^ Communicator
         -> IO (Status, b)      -- ^ Message status and received object
sendrecv sendobj sendrank sendtag recvrank recvtag comm =
  do sendreq <- isend sendobj sendrank sendtag comm
     recvreq <- irecv recvrank recvtag comm
     wait_ sendreq
     wait recvreq

-- | Send and receive objects simultaneously, without returning a
-- status for the received message.
sendrecv_ :: (Store a, Store b)
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
irecv :: Store a
      => Rank                   -- ^ Source rank
      -> Tag                    -- ^ Source tag
      -> Comm                   -- ^ Communicator
      -> IO (Request a)         -- ^ Communication request
irecv recvrank recvtag comm =
  do result <- newEmptyMVar
     _ <- forkIO $
       do status <- untilJust $
            do yield
               MPI.iprobe recvrank recvtag comm
          source <- MPI.getSource status
          tag <- MPI.getTag status
          count <- MPI.getCount status MPI.datatypeByte
          let len = MPI.fromCount count
          ptr <- mallocBytes len
          buffer <- B.unsafePackMallocCStringLen (ptr, len)
          req <- MPI.irecv buffer source tag comm
          whileM_ (not <$> MPI.test_ req) yield
          putMVar result (Status source tag, decodeEx buffer)
     return (Request result)

-- | Begin to send an object. Call 'test' or 'wait' to finish the
-- communication.
isend :: Store a
      => a                     -- ^ Object to send
      -> Rank                  -- ^ Destination rank
      -> Tag                   -- ^ Message tag
      -> Comm                  -- ^ Communicator
      -> IO (Request ())       -- ^ Communication request
isend sendobj sendrank sendtag comm =
  do let sendbuf = encode sendobj
     req <- MPI.isend sendbuf sendrank sendtag comm
     result <- newEmptyMVar
     _ <- forkIO $ B.unsafeUseAsCString sendbuf $ \_ ->
       do whileM_ (not <$> MPI.test_ req) yield
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
      -> IO (Maybe a) -- ^ 'Just' communication result, if
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
-- processes in the communicator. Call this function on all non-root
-- processes. Call 'bcastSend' instead on the root process.
bcastRecv :: Store a
          => Rank
          -> Comm
          -> IO a
bcastRecv root comm =
  do rank <- MPI.commRank comm
     mpiAssert (rank /= root) "bcastRecv: expected rank /= root"
     buf <- mallocForeignPtr @CLong
     MPI.bcast (buf, 1::Int) root comm
     len <- withForeignPtr buf peek
     ptr <- mallocBytes (fromIntegral len)
     recvbuf <- B.unsafePackMallocCStringLen (ptr, fromIntegral len)
     MPI.bcast recvbuf root comm               
     return (decodeEx recvbuf)

-- | Broadcast a message from one process (the "root") to all other
-- processes in the communicator. Call this function on the root
-- process. Call 'bcastRecv' instead on all non-root processes.
bcastSend :: Store a
          => a
          -> Rank
          -> Comm
          -> IO ()
bcastSend sendobj root comm =
  do rank <- MPI.commRank comm
     mpiAssert (rank == root) "bcastSend: expected rank == root"
     let sendbuf = encode sendobj
     buf <- mallocForeignPtr @CLong
     withForeignPtr buf $ \ptr -> poke ptr (fromIntegral (B.length sendbuf))
     MPI.bcast (buf, 1::Int) root comm
     MPI.bcast sendbuf root comm

-- | Begin a barrier. Call 'test' or 'wait' to finish the
-- communication.
ibarrier :: Comm
         -> IO (Request ())
ibarrier comm =
  do result <- newEmptyMVar
     req <- MPI.ibarrier comm
     _ <- forkIO $
       do whileM_ (not <$> MPI.test_ req) yield
          putMVar result (Status MPI.anySource MPI.anyTag, ())
     return (Request result)

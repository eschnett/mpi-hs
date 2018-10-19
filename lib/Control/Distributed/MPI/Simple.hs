{-# LANGUAGE TypeApplications #-}

module Control.Distributed.MPI.Simple
  ( MPIException(..)
  , mainMPI
  , Comm(..)
  , commSelf
  , commWorld
  , Rank(..)
  , anySource
  , commRank
  , commSize
  , rootRank
  , Status(..)
  , Tag(..)
  , anyTag
  , unitTag
  , Request(..)
  , abort
  , irecv
  , isend
  , recv
  , recv_
  , send
  , sendrecv
  , sendrecv_
  , test
  , test_
  , wait
  , wait_
  , barrier
  , bcastRecv
  , bcastSend
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
  , Rank(..)
  , anySource
  , commRank
  , commSize
  , rootRank
  , Tag(..)
  , anyTag
  , unitTag
  , abort
  , barrier
  )



newtype MPIException = MPIException String
  deriving (Eq, Ord, Read, Show, Typeable)
instance Exception MPIException



data DidInit = DidInit | DidNotInit

initMPI :: IO DidInit
initMPI =
  do isInit <- MPI.initialized
     if isInit
       then return DidNotInit
       else do ts <- MPI.initThread MPI.ThreadMultiple
               when (ts < MPI.ThreadMultiple) $
                 throw $ MPIException
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

mainMPI :: IO () -> IO ()
mainMPI task = bracket initMPI finalizeMPI (\_ -> task)



newtype Request a = Request (MVar (Status, a))

data Status = Status { msgRank :: !Rank
                     , msgTag :: !Tag
                     }
  deriving (Eq, Ord, Read, Show)



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

recv_ :: Store a
      => Rank                   -- ^ Source rank
      -> Tag                    -- ^ Source tag
      -> Comm                   -- ^ Communicator
      -> IO a                   -- ^ Received object
recv_ recvrank recvtag comm =
  snd <$> recv recvrank recvtag comm

send :: Store a
     => a                     -- ^ Object to send
     -> Rank                  -- ^ Destination rank
     -> Tag                   -- ^ Message tag
     -> Comm                  -- ^ Communicator
     -> IO ()
send sendobj sendrank sendtag comm =
  do let sendbuf = encode sendobj
     MPI.send sendbuf sendrank sendtag comm

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

test :: Request a               -- ^ Communication request
     -> IO (Maybe (Status, a))  -- ^ 'Just' communication result, if
                                -- communication has finished, else 'Nothing'
test (Request result) = tryTakeMVar result

test_ :: Request a       -- ^ Communication request
      -> IO (Maybe a) -- ^ 'Just' communication result, if
                      -- communication has finished, else 'Nothing'
test_ req = fmap snd <$> test req

wait :: Request a               -- ^ Communication request
     -> IO (Status, a)          -- ^ Message status and communication result
wait (Request result) = takeMVar result

wait_ :: Request a              -- ^ Communication request
      -> IO a                   -- ^ Communication result
wait_ req = snd <$> wait req



bcastRecv :: Store a
          => Rank
          -> Comm
          -> IO a
bcastRecv root comm =
  do rank <- MPI.commRank comm
     assert (rank /= root) $ return ()
     buf <- mallocForeignPtr @CLong
     MPI.bcast (buf, 1::Int) root comm
     len <- withForeignPtr buf peek
     ptr <- mallocBytes (fromIntegral len)
     recvbuf <- B.unsafePackMallocCStringLen (ptr, fromIntegral len)
     MPI.bcast recvbuf root comm               
     return (decodeEx recvbuf)

bcastSend :: Store a
          => a
          -> Rank
          -> Comm
          -> IO ()
bcastSend sendobj root comm =
  do rank <- MPI.commRank comm
     assert (rank == root) $ return ()
     let sendbuf = encode sendobj
     buf <- mallocForeignPtr @CLong
     withForeignPtr buf $ \ptr -> poke ptr (fromIntegral (B.length sendbuf))
     MPI.bcast (buf, 1::Int) root comm
     MPI.bcast sendbuf root comm

ibarrier :: Comm
         -> IO (Request ())
ibarrier comm =
  do result <- newEmptyMVar
     req <- MPI.ibarrier comm
     _ <- forkIO $
       do whileM_ (not <$> MPI.test_ req) yield
          putMVar result (Status MPI.anySource MPI.anyTag, ())
     return (Request result)

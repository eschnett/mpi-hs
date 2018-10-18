module Control.Distributed.MPI.Simple
  ( MPIException(..)
  , withMPI
  , Comm(..)
  , Rank(..)
  , Tag(..)
  , Request(..)
  , irecv
  , isend
  , test
  , wait
  ) where

import Prelude hiding (init)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString.Unsafe as B
import Data.Store
import Data.Typeable
import Foreign.Marshal

import qualified Control.Distributed.MPI as MPI
import Control.Distributed.MPI
  ( Comm(..)
  , Rank(..)
  , Tag(..)
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

withMPI :: IO () -> IO ()
withMPI task = bracket initMPI finalizeMPI (\_ -> task)



newtype Request a = Request (MVar a)

irecv :: Store a => Rank -> Tag -> Comm -> IO (Request a)
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
          putMVar result (decodeEx buffer)
     return (Request result)

test :: Request a -> IO (Maybe a)
test (Request result) = tryTakeMVar result

wait :: Request a -> IO a
wait (Request result) = takeMVar result

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
          putMVar result ()
     return (Request result)

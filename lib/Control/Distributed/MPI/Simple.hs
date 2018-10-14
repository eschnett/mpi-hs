module Control.Distributed.MPI.Simple
  ( MPIException(..)
  , finalize
  , init
  , initThread
  ) where

import Prelude hiding (init)

import Control.Concurrent
import Control.Exception
import Data.Typeable
import System.IO.Unsafe

import qualified Control.Distributed.MPI as MPI



didInit :: MVar Bool
didInit = unsafePerformIO newEmptyMVar



newtype MPIException = MPIException String
  deriving (Eq, Ord, Read, Show, Typeable)
instance Exception MPIException



finalize :: IO ()
finalize =
  do e <- isEmptyMVar didInit
     if e
       then throw (MPIException "Control flow error")
       else return ()
     did <- takeMVar didInit
     if did
       then MPI.finalize
       else return ()

init :: IO ()
init =
  do e <- isEmptyMVar didInit
     if not e
       then throw (MPIException "Control flow error")
       else return ()
     i <- MPI.initialized
     if not i
       then do MPI.init
               putMVar didInit True
       else putMVar didInit False

initThread :: MPI.ThreadSupport -> IO ()
initThread threadSupport =
  do e <- isEmptyMVar didInit
     if not e
       then throw (MPIException "Control flow error")
       else return ()
     i <- MPI.initialized
     if not i
       then do ts <- MPI.initThread threadSupport
               if ts < threadSupport
                 then throw $ MPIException
                      ("Insufficient thread support: caller required " ++
                        show threadSupport ++ ", MPI library provided only " ++
                        show ts)
                 else return ()
               putMVar didInit True
       else putMVar didInit False

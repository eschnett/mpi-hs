{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import Data.Array.Storable
-- import Data.ByteString.Unsafe (unsafeUseAsCString)
-- import Data.Serialize
import Foreign (castPtr)
import Foreign.C.Types (CInt)

import qualified Control.Distributed.MPI as MPI

main :: IO ()
main = do i0 <- MPI.initialized
          _ <- assert (i0 == False) $ return ()
          ts <- MPI.initThread MPI.Multiple
          _ <- assert (ts == MPI.Multiple) $ return ()
          i1 <- MPI.initialized
          _ <- assert (i1 == True) $ return ()
          f1 <- MPI.finalized
          _ <- assert (f1 == False) $ return ()

          rank0 <- MPI.commRank MPI.commSelf
          _ <- assert (rank0 == 0) $ return ()
          size1 <- MPI.commSize MPI.commSelf
          _ <- assert (size1 == 1) $ return ()
          rank <- MPI.commRank MPI.commWorld
          size <- MPI.commSize MPI.commWorld
          _ <- assert (rank >= 0 && rank < size) $ return ()
          
          -- let msg = 42 :: CInt
          -- let buf = encode msg
          -- ptr <- unsafeUseAsCString buf return
          -- MPI.send (castPtr ptr) 1 MPI.datatypeInt rank MPI.unitTag MPI.commSelf
          -- let buf' = encode (0 :: CInt)
          -- ptr' <- unsafeUseAsCString buf' return
          -- st <- MPI.recv (castPtr ptr') 1 MPI.datatypeInt rank MPI.unitTag MPI.commSelf
          -- let Right msg' = decode buf'
          -- _ <- assert (msg == msg') $ return ()

          let msg = 42 :: CInt
          buf <- newArray @StorableArray ((), ()) msg
          ptr <- withStorableArray buf return
          MPI.send (castPtr ptr) 1 MPI.datatypeInt rank
            MPI.unitTag MPI.commSelf
          touchStorableArray buf

          buf' <- newArray_ @StorableArray ((), ())
          ptr' <- withStorableArray buf' return
          st <- MPI.recv (castPtr ptr') 1 MPI.datatypeInt rank
            MPI.unitTag MPI.commSelf
          msg' :: CInt <- readArray buf' ()
          touchStorableArray buf'
          _ <- assert (msg == msg') $ return ()

          MPI.finalize
          i2 <- MPI.initialized
          _ <- assert (i2 == True) $ return ()
          f2 <- MPI.finalized
          _ <- assert (f2 == True) $ return ()
          return ()

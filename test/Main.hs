{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import Data.Array.Storable
import Foreign (castPtr)
import Foreign.C.Types (CInt)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Distributed.MPI as MPI

main :: IO ()
main = bracket
  MPI.init
  (\_ -> MPI.finalize)
  (\_ -> do rank <- MPI.commRank MPI.commWorld
            putStrLn $ "rank: " ++ show rank
            defaultMain tests)

tests :: TestTree
tests = testGroup "MPI"
  [ initialized
  , rankSize
  , pointwise
  , collective
  ]



initialized :: TestTree
initialized = testGroup "initialized"
  [ testCase "initialized" $
      do isInit <- MPI.initialized
         isInit @?= True
  , testCase "finalized" $
      do isFini <- MPI.finalized
         isFini @?= False
  ]



rankSize :: TestTree
rankSize = testGroup "rank and size"
  [ testCase "commSelf" $
    do rank <- MPI.commRank MPI.commSelf
       size <- MPI.commSize MPI.commSelf
       rank == 0 && size == 1 @? ""
  , testCase "commWorld" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       rank >= 0 && rank < size @? ""
  ]



pointwise :: TestTree
pointwise = testGroup "pointwise"
  [ testCase "send and recv" $
    do rank <- MPI.commRank MPI.commSelf
  
       let msg = 42 :: CInt
       buf <- newArray @StorableArray ((), ()) msg

       ptr <- withStorableArray buf return
       MPI.send (castPtr ptr) 1 MPI.datatypeInt rank MPI.unitTag
         MPI.commSelf
       -- MPI.send (castPtr ptr) 1 (MPI.datatypeOf buf) rank MPI.unitTag
       --   MPI.commSelf
       touchStorableArray buf
  
       buf' <- newArray_ @StorableArray ((), ())
       ptr' <- withStorableArray buf' return
       st <- MPI.recv (castPtr ptr') 1 MPI.datatypeInt rank MPI.unitTag
         MPI.commSelf
       msg' :: CInt <- readArray buf' ()

       source <- MPI.statusSource st
       tag <- MPI.statusTag st
       count <- MPI.getCount st MPI.datatypeInt
       (msg == msg' && source == rank && tag == MPI.unitTag && count == 1) @? ""
  , testCase "sendrecv" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld

       let msg = (42 + MPI.fromRank rank) :: CInt
       buf <- newArray @StorableArray ((), ()) msg
       ptr <- withStorableArray buf return
       buf' <- newArray_ @StorableArray ((), ())
       ptr' <- withStorableArray buf' return

       st <- MPI.sendrecv
         (castPtr ptr) 1 MPI.datatypeInt ((rank + 1) `mod` size) MPI.unitTag
         (castPtr ptr') 1 MPI.datatypeInt ((rank - 1) `mod` size) MPI.unitTag
         MPI.commWorld
       touchStorableArray buf

       msg' :: CInt <- readArray buf' ()

       source <- MPI.statusSource st
       tag <- MPI.statusTag st
       count <- MPI.getCount st MPI.datatypeInt
       (msg' == 42 + MPI.fromRank ((rank - 1) `mod` size) &&
        source == (rank - 1) `mod` size &&
        tag == MPI.unitTag &&
        count == 1) @? ""
  ]



collective :: TestTree
collective = testGroup "collective"
  [ testCase "allgather" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msg = (42 + rk) :: CInt
       buf <- newArray @StorableArray ((), ()) msg
       ptr <- withStorableArray buf return
       buf' <- newArray_ @StorableArray (0, size-1)
       ptr' <- withStorableArray buf' return
       MPI.allgather (castPtr ptr) 1 MPI.datatypeInt
                     (castPtr ptr') 1 MPI.datatypeInt
                     MPI.commWorld
       touchStorableArray buf
       msgs' :: [CInt]  <- getElems buf'
       msgs' == [42 .. 42 + (sz-1)] @? ""
  , testCase "alltoall" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msgs = [42 + sz * rk + i | i <- [0 .. sz-1]] :: [CInt]
       buf <- newListArray @StorableArray (0, size-1) msgs
       ptr <- withStorableArray buf return
       buf' <- newArray_ @StorableArray (0, size-1)
       ptr' <- withStorableArray buf' return
       MPI.alltoall (castPtr ptr) 1 MPI.datatypeInt
                    (castPtr ptr') 1 MPI.datatypeInt
                    MPI.commWorld
       touchStorableArray buf
       msgs' :: [CInt] <- getElems buf'
       msgs' == [42 + sz * i + rk | i <- [0 .. sz-1]] @? ""
  , testCase "barrier" $
    MPI.barrier MPI.commWorld
  , testCase "bcast" $
    do rank <- MPI.commRank MPI.commWorld
       let rk = MPI.fromRank rank
       let msg = (42 + rk) :: CInt
       buf <- newArray @StorableArray ((), ()) msg
       ptr <- withStorableArray buf return
       MPI.bcast (castPtr ptr) 1 MPI.datatypeInt MPI.rootRank MPI.commWorld
       msg' :: CInt <- readArray buf ()
       msg' == 42 @? ""
  , testCase "gather" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = (42 + rk) :: CInt
       buf <- newArray @StorableArray ((), ()) msg
       ptr <- withStorableArray buf return
       buf' <- newArray_ @StorableArray (0, if isroot then size-1 else -1)
       ptr' <- withStorableArray buf' return
       MPI.gather (castPtr ptr) 1 MPI.datatypeInt
                  (castPtr ptr') 1 MPI.datatypeInt
                  MPI.rootRank MPI.commWorld
       touchStorableArray buf
       msgs' :: [CInt] <- if isroot then getElems buf' else return []
       msgs' == (if isroot then [42 .. 42 + sz-1] else []) @? ""
  , testCase "scatter" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msgs = [42 + i | i <- [0 .. sz-1]] :: [CInt]
       buf <- newListArray @StorableArray
              (0, if isroot then size-1 else -1) msgs
       ptr <- withStorableArray buf return
       buf' <- newArray_ @StorableArray ((), ())
       ptr' <- withStorableArray buf' return
       MPI.scatter (castPtr ptr) 1 MPI.datatypeInt
                   (castPtr ptr') 1 MPI.datatypeInt
                   MPI.rootRank MPI.commWorld
       touchStorableArray buf
       msg' :: CInt <- readArray buf' ()
       msg' == 42 + rk @? ""
  ]

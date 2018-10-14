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
  (\_ -> defaultMain tests)

tests :: TestTree
tests = testGroup "MPI"
  [ initialized
  , rankSize
  , sendRecv
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



sendRecv :: TestTree
sendRecv = testGroup "send and recv"
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
       (msg == msg' && source == rank && tag == MPI.unitTag) @? ""
  , testCase "sendrecv" $
    do rank <- MPI.commRank MPI.commSelf
  
       let msg = 42 :: CInt
       buf <- newArray @StorableArray ((), ()) msg
       ptr <- withStorableArray buf return
       buf' <- newArray_ @StorableArray ((), ())
       ptr' <- withStorableArray buf' return
  
       st <- MPI.sendrecv
         (castPtr ptr) 1 MPI.datatypeInt rank MPI.unitTag
         (castPtr ptr') 1 MPI.datatypeInt rank MPI.unitTag
         MPI.commSelf
       touchStorableArray buf
  
       msg' :: CInt <- readArray buf' ()
  
       source <- MPI.statusSource st
       tag <- MPI.statusTag st
       (msg == msg' && source == rank && tag == MPI.unitTag) @? ""
  ]

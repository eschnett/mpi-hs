{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import Foreign
import Foreign.C.Types
import System.Exit
import System.IO
-- import Test.Tasty
-- import Test.Tasty.HUnit

import qualified Control.Distributed.MPI as MPI



--------------------------------------------------------------------------------

infix 1 @?
(@?) :: Bool -> String -> IO ()
x @? _ = if not x then exitFailure else return ()

infix 1 @?=
(@?=) :: Eq a => a -> a -> IO ()
x @?= y = x == y @? ""



type TestTree = IO ()

testCase :: String -> IO () -> TestTree
testCase name test =
  do rank <- MPI.commRank MPI.commWorld
     if rank == 0
       then do putStrLn $ "  " ++ name ++ "..."
               hFlush stdout
       else return ()
     test



testGroup :: String -> [TestTree] -> TestTree
testGroup name cases =
  do rank <- MPI.commRank MPI.commWorld
     if rank == 0
       then do putStrLn $ name ++ ":"
               hFlush stdout
       else return ()
     sequence_ cases



defaultMain :: TestTree -> IO ()
defaultMain tree =
  do rank <- MPI.commRank MPI.commWorld
     size <- MPI.commSize MPI.commWorld
     if rank == 0
       then do putStrLn $ "MPI Tests: running on " ++ show size ++ " processes"
               hFlush stdout
       else return ()
     tree



--------------------------------------------------------------------------------



main :: IO ()
main = bracket
  MPI.init
  (\_ -> MPI.finalize)
  (\_ -> defaultMain tests)

tests :: TestTree
tests = testGroup "MPI"
  [ initialized
  , rankSize
  , pointToPoint
  , pointToPointNonBlocking
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



pointToPoint :: TestTree
pointToPoint = testGroup "point-to-point"
  [ testCase "send and recv" $
    do rank <- MPI.commRank MPI.commWorld

       let msg = 42
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg

       MPI.send buf 1 rank MPI.unitTag MPI.commWorld

       buf' <- mallocForeignPtr @CInt
       st <- MPI.recv buf' 1 rank MPI.unitTag MPI.commWorld
       msg' <- withForeignPtr buf' peek

       source <- MPI.getSource st
       tag <- MPI.getTag st
       count <- MPI.getCount st MPI.datatypeInt
       (msg' == msg && source == rank && tag == MPI.unitTag && count == 1) @? ""
  , testCase "sendrecv" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld

       let msg = 42 + MPI.fromRank rank
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg

       buf' <- mallocForeignPtr @CInt

       st <- MPI.sendrecv
             buf 1 ((rank + 1) `mod` size) MPI.unitTag
             buf' 1 ((rank - 1) `mod` size) MPI.unitTag
             MPI.commWorld

       msg' <- withForeignPtr buf' peek

       source <- MPI.getSource st
       tag <- MPI.getTag st
       count <- MPI.getCount st MPI.datatypeInt
       (msg' == 42 + MPI.fromRank ((rank - 1) `mod` size) &&
        source == (rank - 1) `mod` size &&
        tag == MPI.unitTag &&
        count == 1) @? ""
  ]



pointToPointNonBlocking :: TestTree
pointToPointNonBlocking = testGroup "point-to-point non-blocking"
  [ testCase "send and recv" $
    do rank <- MPI.commRank MPI.commWorld

       let msg = 42
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg

       req <- MPI.isend buf 1 rank MPI.unitTag MPI.commWorld

       buf' <- mallocForeignPtr @CInt
       req' <- MPI.irecv buf' 1 rank MPI.unitTag MPI.commWorld

       MPI.wait_ req
       st <- MPI.wait req'

       touchForeignPtr buf
       msg' <- withForeignPtr buf' peek

       source <- MPI.getSource st
       tag <- MPI.getTag st
       count <- MPI.getCount st MPI.datatypeInt
       (msg' == msg && source == rank && tag == MPI.unitTag && count == 1) @? ""
  ]



collective :: TestTree
collective = testGroup "collective"
  [ testCase "allgather" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtrArray @CInt sz
       MPI.allgather buf 1 buf' 1 MPI.commWorld
       msgs' <- withForeignPtr buf' (peekArray sz)
       msgs' == [42 .. 42 + fromIntegral (sz-1)] @? ""
  , testCase "alltoall" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msgs = fromIntegral <$> [42 + sz * rk + i | i <- [0 .. sz-1]]
       buf <- mallocForeignPtrArray @CInt sz
       withForeignPtr buf $ \ptr -> pokeArray ptr msgs
       buf' <- mallocForeignPtrArray @CInt sz
       MPI.alltoall buf 1 buf' 1 MPI.commWorld
       msgs' <- withForeignPtr buf' (peekArray sz)
       msgs' == (fromIntegral <$> [42 + sz * i + rk | i <- [0 .. sz-1]]) @? ""
  , testCase "barrier" $
    MPI.barrier MPI.commWorld
  , testCase "bcast" $
    do rank <- MPI.commRank MPI.commWorld
       let rk = MPI.fromRank rank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       MPI.bcast buf 1 MPI.rootRank MPI.commWorld
       msg' <- withForeignPtr buf peek
       msg' == 42 @? ""
  , testCase "gather" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtrArray @CInt (if isroot then sz else 0)
       MPI.gather buf 1 buf' 1 MPI.rootRank MPI.commWorld
       msgs' <- withForeignPtr buf' $ peekArray (if isroot then sz else 0)
       msgs' == (if isroot then [42 .. 42 + fromIntegral sz-1] else []) @? ""
  , testCase "scatter" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msgs = [42 + fromIntegral i | i <- [0 .. sz-1]]
       buf <- mallocForeignPtrArray @CInt (if isroot then sz else 0)
       withForeignPtr buf $ \ptr -> pokeArray ptr msgs
       buf' <- mallocForeignPtr @CInt
       MPI.scatter buf 1 buf' 1 MPI.rootRank MPI.commWorld
       msg' <- withForeignPtr buf' peek
       msg' == 42 + rk @? ""
  ]

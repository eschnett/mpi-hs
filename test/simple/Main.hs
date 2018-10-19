{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import System.Exit

import qualified Control.Distributed.MPI.Simple as MPI

default (Int)



--------------------------------------------------------------------------------

infix 1 @?
(@?) :: Bool -> String -> IO ()
x @? msg = if not x then die msg else return ()

infix 1 @?=
(@?=) :: Eq a => a -> a -> IO ()
x @?= y = x == y @? "test failed"



type TestTree = IO ()

testCase :: String -> IO () -> TestTree
testCase name test =
  do rank <- MPI.commRank MPI.commWorld
     if rank == 0
       then do putStrLn $ "  " ++ name ++ "..."
               hFlush stdout
       else return ()
     MPI.barrier MPI.commWorld
     test
     MPI.barrier MPI.commWorld



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
main = MPI.mainMPI $ defaultMain tests

tests :: TestTree
tests = testGroup "MPI"
  [ rankSize
  , pointToPoint
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
  [ testCase "sendrecv" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let sendmsg :: String = "Hello, World!"
       let sendrank = (rank + 1) `mod` size
       let recvrank = (rank - 1) `mod` size
       (status, recvmsg :: String) <-
         MPI.sendrecv sendmsg sendrank MPI.unitTag recvrank MPI.unitTag
         MPI.commWorld
       (recvmsg == sendmsg &&
        MPI.msgRank status == recvrank &&
        MPI.msgTag status == MPI.unitTag) @? ""
  ]

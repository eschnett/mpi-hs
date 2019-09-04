{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.String
import Foreign
import System.IO
import System.Exit

import qualified Control.Distributed.MPI.Storable as MPI

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
  , pointToPointNonBlocking
  , collective
  , collectiveNonBlocking
  ]



data V16 a = V16 a a a a a a a a a a a a a a a a
  deriving (Eq, Ord, Read, Show)

instance Storable a => Storable (V16 a) where
  sizeOf _ = 16 * sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  poke ptr (V16 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf) =
    do pokeElemOff (castPtr ptr) 0x0 c0
       pokeElemOff (castPtr ptr) 0x1 c1
       pokeElemOff (castPtr ptr) 0x2 c2
       pokeElemOff (castPtr ptr) 0x3 c3
       pokeElemOff (castPtr ptr) 0x4 c4
       pokeElemOff (castPtr ptr) 0x5 c5
       pokeElemOff (castPtr ptr) 0x6 c6
       pokeElemOff (castPtr ptr) 0x7 c7
       pokeElemOff (castPtr ptr) 0x8 c8
       pokeElemOff (castPtr ptr) 0x9 c9
       pokeElemOff (castPtr ptr) 0xa ca
       pokeElemOff (castPtr ptr) 0xb cb
       pokeElemOff (castPtr ptr) 0xc cc
       pokeElemOff (castPtr ptr) 0xd cd
       pokeElemOff (castPtr ptr) 0xe ce
       pokeElemOff (castPtr ptr) 0xf cf
  peek ptr = do c0 <- peekElemOff (castPtr ptr) 0x0
                c1 <- peekElemOff (castPtr ptr) 0x1
                c2 <- peekElemOff (castPtr ptr) 0x2
                c3 <- peekElemOff (castPtr ptr) 0x3
                c4 <- peekElemOff (castPtr ptr) 0x4
                c5 <- peekElemOff (castPtr ptr) 0x5
                c6 <- peekElemOff (castPtr ptr) 0x6
                c7 <- peekElemOff (castPtr ptr) 0x7
                c8 <- peekElemOff (castPtr ptr) 0x8
                c9 <- peekElemOff (castPtr ptr) 0x9
                ca <- peekElemOff (castPtr ptr) 0xa
                cb <- peekElemOff (castPtr ptr) 0xb
                cc <- peekElemOff (castPtr ptr) 0xc
                cd <- peekElemOff (castPtr ptr) 0xd
                ce <- peekElemOff (castPtr ptr) 0xe
                cf <- peekElemOff (castPtr ptr) 0xf
                return (V16 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf)

type FixedString = V16 Char

instance IsString FixedString where
  fromString s =
    let c0:c1:c2:c3:c4:c5:c6:c7:c8:c9:ca:cb:cc:cd:ce:cf:_ = s ++ repeat '\NUL'
    in (V16 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf)



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
       let sendmsg :: FixedString = "Hello, World!"
       let sendrank = (rank + 1) `mod` size
       let recvrank = (rank - 1) `mod` size
       (status, recvmsg :: FixedString) <-
         MPI.sendrecv sendmsg sendrank MPI.unitTag recvrank MPI.unitTag
         MPI.commWorld
       (recvmsg == sendmsg &&
        MPI.msgRank status == recvrank &&
        MPI.msgTag status == MPI.unitTag) @? ""
  ]



pointToPointNonBlocking :: TestTree
pointToPointNonBlocking = testGroup "point-to-point non-blocking"
  [ testCase "send and recv" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let sendmsg :: FixedString = "Hello, World!"
       let sendrank = (rank + 1) `mod` size
       sendreq <- MPI.isend sendmsg sendrank MPI.unitTag MPI.commWorld
       let recvrank = (rank - 1) `mod` size
       recvreq <- MPI.irecv recvrank MPI.unitTag MPI.commWorld
       (sendstatus, ()) <- MPI.wait sendreq
       (recvstatus, recvmsg :: FixedString) <- MPI.wait recvreq
       (recvmsg == sendmsg &&
        MPI.msgRank sendstatus == sendrank &&
        MPI.msgTag sendstatus == MPI.unitTag &&
        MPI.msgRank recvstatus == recvrank &&
        MPI.msgTag recvstatus == MPI.unitTag) @? ""
  ]



collective :: TestTree
collective = testGroup "collective"
  [ testCase "barrier" $
    do MPI.barrier MPI.commWorld
  , testCase "bcast" $
    do rank <- MPI.commRank MPI.commWorld
       let sendmsg :: FixedString = "Hello, World!"
       recvmsg :: FixedString <-
         if rank == MPI.rootRank
         then do MPI.bcastSend sendmsg MPI.rootRank MPI.commWorld
                 return sendmsg
         else do MPI.bcastRecv MPI.rootRank MPI.commWorld
       recvmsg == sendmsg @? ""
  ]



collectiveNonBlocking :: TestTree
collectiveNonBlocking = testGroup "collective non-blocking"
  [ testCase "barrier" $
    do req <- MPI.ibarrier MPI.commWorld
       MPI.wait_ req
  , testCase "bcast" $
    do rank <- MPI.commRank MPI.commWorld
       let sendmsg :: FixedString = "Hello, World!"
       recvmsg :: FixedString <-
         if rank == MPI.rootRank
         then do req <- MPI.ibcastSend sendmsg MPI.rootRank MPI.commWorld
                 MPI.wait_ req
                 return sendmsg
         else do req <- MPI.ibcastRecv MPI.rootRank MPI.commWorld
                 MPI.wait_ req
       recvmsg == sendmsg @? ""
  ]

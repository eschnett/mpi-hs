{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.IORef
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
main = bracket
  (do _ <- MPI.initThread MPI.ThreadMultiple
      return ())
  (\_ -> MPI.finalize)
  (\_ -> defaultMain tests)

tests :: TestTree
tests = testGroup "MPI"
  [ initialized
  , rankSize
  , pointToPoint
  , pointToPointNonBlocking
  , collective
  , collectiveNonBlocking
  , reductions
  , dynamic
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
       size <- MPI.commSize MPI.commWorld
       
       let msg = 42
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       
       if rank + 1 < size
         then MPI.send (buf, 1::Int) (rank + 1) MPI.unitTag MPI.commWorld
         else return ()
       
       if rank - 1 >= 0 then
         do buf' <- mallocForeignPtr @CInt
            st <- MPI.recv (buf', 1::Int) (rank - 1) MPI.unitTag MPI.commWorld
            msg' <- withForeignPtr buf' peek
            
            source <- MPI.getSource st
            tag <- MPI.getTag st
            count <- MPI.getCount st MPI.datatypeInt
            (msg' == msg && source == rank - 1 && tag == MPI.unitTag &&
             count == 1) @? ""
         else True @? ""
  , testCase "sendrecv" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld

       let msg = 42 + MPI.fromRank rank
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg

       buf' <- mallocForeignPtr @CInt

       st <- MPI.sendrecv
             (buf, 1::Int) ((rank + 1) `mod` size) MPI.unitTag
             (buf', 1::Int) ((rank - 1) `mod` size) MPI.unitTag
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

       req <- MPI.isend (buf, 1::Int) rank MPI.unitTag MPI.commWorld

       buf' <- mallocForeignPtr @CInt
       req' <- MPI.irecv (buf', 1::Int) rank MPI.unitTag MPI.commWorld

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
       MPI.allgather (buf, 1::Int) (buf', 1::Int) MPI.commWorld
       msgs' <- withForeignPtr buf' (peekArray sz)
       msgs' == [42 .. 42 + fromIntegral (sz-1)] @? ""
  , testCase "allreduce" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       MPI.allreduce (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.commWorld
       msg' <- withForeignPtr buf' peek
       msg' == sum [42 .. 42 + (sz-1)] @? ""
  , testCase "alltoall" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msgs = fromIntegral <$> [42 + sz * rk + i | i <- [0 .. sz-1]]
       buf <- mallocForeignPtrArray @CInt sz
       withForeignPtr buf $ \ptr -> pokeArray ptr msgs
       buf' <- mallocForeignPtrArray @CInt sz
       MPI.alltoall (buf, 1::Int) (buf', 1::Int) MPI.commWorld
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
       MPI.bcast (buf, 1::Int) MPI.rootRank MPI.commWorld
       msg' <- withForeignPtr buf peek
       msg' == 42 @? ""
  , testCase "exscan" $
    do rank <- MPI.commRank MPI.commWorld
       let rk = MPI.fromRank rank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       MPI.exscan (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.commWorld
       msg' <- withForeignPtr buf' (if rank == 0 then \_ -> return 0 else peek)
       msg' == sum [42 .. 42 + rk-1] @? ""
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
       MPI.gather (buf, 1::Int) (buf', 1::Int) MPI.rootRank MPI.commWorld
       msgs' <- withForeignPtr buf' $ peekArray (if isroot then sz else 0)
       (if isroot then msgs' == [42 .. 42 + fromIntegral sz-1] else True) @? ""
  , testCase "reduce" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       MPI.reduce (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.rootRank
         MPI.commWorld
       msg' <- withForeignPtr buf' $ if isroot then peek else \_ -> return 0
       (if isroot then msg' == sum [42 .. 42 + sz-1] else True) @? ""
  , testCase "scan" $
    do rank <- MPI.commRank MPI.commWorld
       let rk = MPI.fromRank rank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       MPI.scan (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.commWorld
       msg' <- withForeignPtr buf' peek
       msg' == sum [42 .. 42 + rk] @? ""
  , testCase "scatter" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msgs =
             if isroot then [42 + fromIntegral i | i <- [0 .. sz-1]] else []
       buf <- mallocForeignPtrArray @CInt (if isroot then sz else 0)
       withForeignPtr buf $
         \ptr -> if isroot then pokeArray ptr msgs else return ()
       buf' <- mallocForeignPtr @CInt
       MPI.scatter (buf, 1::Int) (buf', 1::Int) MPI.rootRank MPI.commWorld
       msg' <- withForeignPtr buf' peek
       msg' == 42 + rk @? ""
  ]



reductions :: TestTree
reductions = testGroup "reduction operations"
  [ testCase "max" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       MPI.reduce (buf, 1::Int) (buf', 1::Int) MPI.opMax MPI.rootRank
         MPI.commWorld
       msg' <- withForeignPtr buf' $ if isroot then peek else \_ -> return 0
       (if isroot then msg' == maximum [42 .. 42 + sz-1] else True) @? ""
  , testCase "min" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       MPI.reduce (buf, 1::Int) (buf', 1::Int) MPI.opMin MPI.rootRank
         MPI.commWorld
       msg' <- withForeignPtr buf' $ if isroot then peek else \_ -> return 0
       (if isroot then msg' == minimum [42 .. 42 + sz-1] else True) @? ""
  , testCase "sum" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       MPI.reduce (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.rootRank
         MPI.commWorld
       msg' <- withForeignPtr buf' $ if isroot then peek else \_ -> return 0
       (if isroot then msg' == sum [42 .. 42 + sz-1] else True) @? ""
  ]



collectiveNonBlocking :: TestTree
collectiveNonBlocking = testGroup "collective non-blocking"
  [ testCase "iallgather" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtrArray @CInt sz
       req <- MPI.iallgather (buf, 1::Int) (buf', 1::Int) MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msgs' <- withForeignPtr buf' (peekArray sz)
       msgs' == [42 .. 42 + fromIntegral (sz-1)] @? ""
  , testCase "iallreduce" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       req <- MPI.iallreduce (buf, 1::Int) (buf', 1::Int) MPI.opSum
              MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msg' <- withForeignPtr buf' peek
       msg' == sum [42 .. 42 + (sz-1)] @? ""
  , testCase "ialltoall" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let msgs = fromIntegral <$> [42 + sz * rk + i | i <- [0 .. sz-1]]
       buf <- mallocForeignPtrArray @CInt sz
       withForeignPtr buf $ \ptr -> pokeArray ptr msgs
       buf' <- mallocForeignPtrArray @CInt sz
       req <- MPI.ialltoall (buf, 1::Int) (buf', 1::Int) MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msgs' <- withForeignPtr buf' (peekArray sz)
       msgs' == (fromIntegral <$> [42 + sz * i + rk | i <- [0 .. sz-1]]) @? ""
  , testCase "ibarrier" $
    do req <- MPI.ibarrier MPI.commWorld
       MPI.wait_ req
  , testCase "ibcast" $
    do rank <- MPI.commRank MPI.commWorld
       let rk = MPI.fromRank rank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       req <- MPI.ibcast (buf, 1::Int) MPI.rootRank MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msg' <- withForeignPtr buf peek
       msg' == 42 @? ""
  , testCase "iexscan" $
    do rank <- MPI.commRank MPI.commWorld
       let rk = MPI.fromRank rank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       req <- MPI.iexscan (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msg' <- withForeignPtr buf' (if rank == 0 then \_ -> return 0 else peek)
       msg' == sum [42 .. 42 + rk-1] @? ""
  , testCase "igather" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtrArray @CInt (if isroot then sz else 0)
       req <- MPI.igather (buf, 1::Int) (buf', 1::Int) MPI.rootRank
              MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msgs' <- withForeignPtr buf' $ peekArray (if isroot then sz else 0)
       (if isroot then msgs' == [42 .. 42 + fromIntegral sz-1] else True) @? ""
  , testCase "ireduce" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       req <- MPI.ireduce (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.rootRank
              MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msg' <- withForeignPtr buf' $ if isroot then peek else \_ -> return 0
       (if isroot then msg' == sum [42 .. 42 + sz-1] else True) @? ""
  , testCase "iscan" $
    do rank <- MPI.commRank MPI.commWorld
       let rk = MPI.fromRank rank
       let msg = 42 + rk
       buf <- mallocForeignPtr @CInt
       withForeignPtr buf $ \ptr -> poke ptr msg
       buf' <- mallocForeignPtr @CInt
       req <- MPI.iscan (buf, 1::Int) (buf', 1::Int) MPI.opSum MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msg' <- withForeignPtr buf' peek
       msg' == sum [42 .. 42 + rk] @? ""
  , testCase "iscatter" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld
       let rk = MPI.fromRank rank
       let sz = MPI.fromRank size
       let isroot = rank == MPI.rootRank
       let msgs = [42 + fromIntegral i | i <- [0 .. sz-1]]
       buf <- mallocForeignPtrArray @CInt (if isroot then sz else 0)
       withForeignPtr buf $ \ptr -> pokeArray ptr msgs
       buf' <- mallocForeignPtr @CInt
       req <- MPI.iscatter (buf, 1::Int) (buf', 1::Int) MPI.rootRank
              MPI.commWorld
       MPI.wait_ req
       touchForeignPtr buf
       msg' <- withForeignPtr buf' peek
       msg' == 42 + rk @? ""
  ]



dynamic :: TestTree
dynamic = testGroup "dynamic"
  [ testCase "sequential" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld

       breq <- newIORef Nothing
       let signalDone =
             do r <- MPI.ibarrier MPI.commWorld
                writeIORef breq (Just r)
       let checkDone =
             do mreq <- readIORef breq
                case mreq of
                  Nothing -> return False
                  Just req -> MPI.test_ req

       sendreqs <- newIORef []
       let sendMsg dst =
             when (dst < size) $
             do buf <- mallocForeignPtr @CInt
                withForeignPtr buf $ \ptr -> poke ptr 42
                r <- MPI.isend (buf, 1::Int) dst MPI.unitTag MPI.commWorld
                modifyIORef' sendreqs ((buf, r) :)
       let drainSendQueue =
             do srs <- readIORef sendreqs
                srs' <- filterM (\(_, r) -> not <$> MPI.test_ r) srs
                writeIORef sendreqs srs'
       let checkForMsg = MPI.iprobe MPI.anySource MPI.unitTag MPI.commWorld
       let recvMsg st =
             do src <- MPI.getSource st
                buf <- mallocForeignPtr @CInt
                MPI.recv_ (buf, 1::Int) src MPI.unitTag MPI.commWorld

       -- each rank sends to the next
       when (rank == 0) $
         do sendMsg (rank + 1)
            signalDone

       untilM_
         (do drainSendQueue
             mst <- checkForMsg
             case mst of
               Nothing -> return ()
               Just st -> do recvMsg st
                             sendMsg (rank + 1)
                             signalDone
         )
         checkDone
  , testCase "tree" $
    do rank <- MPI.commRank MPI.commWorld
       size <- MPI.commSize MPI.commWorld

       breq <- newIORef Nothing
       let signalDone =
             do r <- MPI.ibarrier MPI.commWorld
                writeIORef breq (Just r)
       let checkDone =
             do mreq <- readIORef breq
                case mreq of
                  Nothing -> return False
                  Just req -> MPI.test_ req

       sendreqs <- newIORef []
       let sendMsg dst =
             when (dst < size) $
             do buf <- mallocForeignPtr @CInt
                withForeignPtr buf $ \ptr -> poke ptr 42
                r <- MPI.isend (buf, 1::Int) dst MPI.unitTag MPI.commWorld
                modifyIORef' sendreqs ((buf, r) :)
       let drainSendQueue =
             do srs <- readIORef sendreqs
                srs' <- filterM (\(_, r) -> not <$> MPI.test_ r) srs
                writeIORef sendreqs srs'
       let checkForMsg = MPI.iprobe MPI.anySource MPI.unitTag MPI.commWorld
       let recvMsg st =
             do src <- MPI.getSource st
                buf <- mallocForeignPtr @CInt
                MPI.recv_ (buf, 1::Int) src MPI.unitTag MPI.commWorld

       -- rank r sends to 2*r+1 and 2*r+2
       when (rank == 0) $
         do sendMsg (2 * rank + 1)
            sendMsg (2 * rank + 2)
            signalDone

       untilM_
         (do drainSendQueue
             mst <- checkForMsg
             case mst of
               Nothing -> return ()
               Just st -> do recvMsg st
                             sendMsg (2 * rank + 1)
                             sendMsg (2 * rank + 2)
                             signalDone
         )
         checkDone
  , testCase "multi-threaded" $
    do mts <- MPI.threadSupport
       let Just ts = mts
       when (ts >= MPI.ThreadMultiple) $
         do rank <- MPI.commRank MPI.commWorld
            size <- MPI.commSize MPI.commWorld
     
            breq <- newEmptyMVar
            let signalDone =
                  do _ <- forkIO $
                       do req <- MPI.ibarrier MPI.commWorld
                          whileM_ (not <$> MPI.test_ req) yield
                          putMVar breq ()
                     return ()
            let checkDone = not <$> isEmptyMVar breq
     
            let sendMsg dst =
                  when (dst < size) $
                  do _ <- forkIO $
                       do buf <- mallocForeignPtr @CInt
                          withForeignPtr buf $ \ptr -> poke ptr 42
                          req <- MPI.isend (buf, 1::Int) dst MPI.unitTag
                                 MPI.commWorld
                          whileM_ (not <$> MPI.test_ req) yield
                     return ()
            let checkForMsg = MPI.iprobe MPI.anySource MPI.unitTag MPI.commWorld
            let recvMsg st =
                  do src <- MPI.getSource st
                     buf <- mallocForeignPtr @CInt
                     MPI.recv_ (buf, 1::Int) src MPI.unitTag MPI.commWorld
     
            -- rank r sends to 2*r+1 and 2*r+2
            when (rank == 0) $
              do sendMsg (2 * rank + 1)
                 sendMsg (2 * rank + 2)
                 signalDone
     
            untilM_
              (do mst <- checkForMsg
                  case mst of
                    Nothing -> return ()
                    Just st -> do recvMsg st
                                  sendMsg (2 * rank + 1)
                                  sendMsg (2 * rank + 2)
                                  signalDone
                  yield
              )
              checkDone
  ]

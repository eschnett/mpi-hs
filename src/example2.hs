{-# LANGUAGE TypeApplications #-}

import qualified Control.Distributed.MPI.Storable as MPI

main :: IO ()
main = MPI.mainMPI $ do
  rank <- MPI.commRank MPI.commWorld
  size <- MPI.commSize MPI.commWorld
  putStrLn $ "This is process " ++ show rank ++ " of " ++ show size
  let msg = MPI.fromRank rank :: Int
  msg' <- MPI.bcast (Just msg) MPI.rootRank MPI.commWorld
  putStrLn $ "Process " ++ show msg' ++ " says hi"

{-# LANGUAGE TypeApplications #-}

import qualified Control.Distributed.MPI as MPI
import Foreign
import Foreign.C.Types

main :: IO ()
main = do
  MPI.init
  rank <- MPI.commRank MPI.commWorld
  size <- MPI.commSize MPI.commWorld
  putStrLn $ "This is process " ++ show rank ++ " of " ++ show size
  let msg = MPI.fromRank rank
  buf <- mallocForeignPtr @CInt
  withForeignPtr buf $ \ptr -> poke ptr msg
  MPI.bcast (buf, 1::Int) MPI.rootRank MPI.commWorld
  msg' <- withForeignPtr buf peek
  putStrLn $ "Process " ++ show msg' ++ " says hi"
  MPI.finalize

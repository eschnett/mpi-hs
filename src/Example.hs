import Control.Exception
import Data.Version

import qualified Control.Distributed.MPI as MPI

main :: IO ()
main = bracket
  MPI.init
  (\_ ->  MPI.finalize) $
  \_ -> do library <- MPI.getLibraryVersion
           putStrLn $ "MPI library " ++ library
           version <- MPI.getVersion
           putStrLn $ "MPI standard " ++ showVersion version
           processor <- MPI.getProcessorName
           rank <- MPI.commRank MPI.commWorld
           size <- MPI.commSize MPI.commWorld
           putStrLn $ "MPI processor " ++ processor ++
             " (" ++ show rank ++ "/" ++ show size ++ ")"

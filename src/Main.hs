import Data.Version

import qualified Control.Distributed.MPI as MPI

main :: IO ()
main = do MPI.init
          version <- MPI.getVersion
          putStrLn $ "MPI standard " ++ showVersion version
          MPI.finalize

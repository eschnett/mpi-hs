# [mpi-hs](https://github.com/eschnett/mpi-hs)

[MPI](https://www.mpi-forum.org) bindings for Haskell

[![Github](share/GitHub_Logo.png)](https://github.com/eschnett/mpi-hs)
[[Hackage]](http://hackage.haskell.org/package/mpi-hs)
[![CircleCI](https://circleci.com/gh/eschnett/mpi-hs.svg?style=svg)](https://circleci.com/gh/eschnett/mpi-hs)



## Overview

MPI (the _Message Passing Interface_) is widely used standard for
distributed-memory programming on HPC (High Performance Computing)
systems. MPI allows exchanging data (_messages_) between programs
running in parallel. There are several high-quality open source MPI
implementations (e.g. MPICH, MVAPICH, OpenMPI) as well as a variety of
closed-source implementations. These libraries can typically make use
of high-bandwidth low-latency communication hardware such as
InfiniBand.

This library `mpi-hs` provides Haskell bindings for MPI. It is based
on ideas taken from
[haskell-mpi](https://github.com/bjpop/haskell-mpi),
[Boost.MPI](https://www.boost.org/doc/libs/1_64_0/doc/html/mpi.html),
and [MPI for Python](https://mpi4py.readthedocs.io/en/stable/).

`mpi-hs` provides two API levels: A low-level API gives rather direct
access to the MPI API, apart from certain "reasonable" mappings from C
to Haskell (e.g. output arguments that are in C stored to a pointer
are in Haskell regular return values). A high-level API simplifies
exchanging arbitrary values that can be serialized.



## Example

This is a typical MPI C code:
```C
#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv) {
  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  printf("This is process %d of %d\n", rank, size);
  MPI_Finalize();
  return 0;
}
```

The Haskell equivalent looks like this:
```Haskell
import Control.Distributed.MPI as MPI

main :: IO ()
main =
  do MPI.init
     rank <- MPI.commRank MPI.commWorld
     size <- MPI.commSize MPI.commWorld
     putStrLn $ "This is process " ++ show rank ++ " of " ++ show size
     MPI.finalize
```

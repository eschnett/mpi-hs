# [mpi-hs](https://github.com/eschnett/mpi-hs)

[MPI](https://www.mpi-forum.org) bindings for Haskell

* [GitHub](https://github.com/eschnett/mpi-hs): Source code repository
* [Hackage](http://hackage.haskell.org/package/mpi-hs): Haskell
  package and documentation
* [Stackage](https://www.stackage.org/package/mpi-hs): Stackage
  snapshots
* [GitHub Actions](https://docs.github.com/en/actions) Build Status
  [![.github/workflows/build.yml](https://github.com/eschnett/mpi-hs/actions/workflows/build.yml/badge.svg)](https://github.com/eschnett/mpi-hs/actions/workflows/build.yml)



## Overview

MPI (the Message Passing Interface) is a widely used standard for
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
[Boost.MPI](https://www.boost.org/doc/libs/1_64_0/doc/html/mpi.html)
for C++, and [MPI for
Python](https://mpi4py.readthedocs.io/en/stable/).

`mpi-hs` provides two API levels: A low-level API gives rather direct
access to the actual MPI API, apart from certain "reasonable" mappings
from C to Haskell (e.g. output arguments that are in C stored via a
pointer are in Haskell regular return values). A high-level API
simplifies exchanging arbitrary values that can be serialized.



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
  int msg = rank;
  MPI_Bcast(&msg, 1, MPI_INT, 0, MPI_COMM_WORLD):
  printf("Process %d says hi\n", msg);
  MPI_Finalize();
  return 0;
}
```

The Haskell equivalent looks like this:
```Haskell
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
```

The high-level API simplifies exchanging data; no need to allocate a
buffer nor to use poke or peek:
```
{-# LANGUAGE TypeApplications #-}

import qualified Control.Distributed.MPI as MPI
import qualified Control.Distributed.MPI.Storable as MPI

main :: IO ()
main = MPI.mainMPI $ do
  rank <- MPI.commRank MPI.commWorld
  size <- MPI.commSize MPI.commWorld
  putStrLn $ "This is process " ++ show rank ++ " of " ++ show size
  let msg = MPI.fromRank rank :: Int
  msg' <- MPI.bcast (Just msg) MPI.rootRank MPI.commWorld
  putStrLn $ "Process " ++ show msg' ++ " says hi"
```



## Installing

`mpi-hs` requires an external MPI library to be available on the
system. How to install such a library is beyond the scope of these
instructions.

`mpi-hs` uses pkg-config to find an MPI installation and supports:

- OpenMPI via the `-fopenmpi` cabal flag (default)
- MVAPICH2 via the `-fmvapich -f-openmpi` cabal flags
- MVAPICH3 via the `-fmvapich3 -f-openmpi` cabal flags
- MPICH via the `-fmpich -f-openmpi` cabal flags

(MVAPICH2 identifies as `mvapich2` in pkg-config, but MVAPICH3 as `mvapich`).
Alternatively, `mpi-hs` can link against `-lmpi` and `mpi.h` generically, if all
pkg-config options are disabled via `-f-openmpi -f-mpich -f-mvapich -f-mvapich3`.
In this case you may need to specify `--extra-include-dirs` and `--extra-lib-dirs`
and point them to your MPI installation directory.


### Testing the MPI installation with a C program

To test your MPI installation independently of using Haskell, copy the
example MPI C code into a file `mpi-example.c`, and run these commands:

```sh
mpicc -c mpi-example.c
mpicc -o mpi-example mpi-example.o
mpiexec -n 3 ./mpi-example
```

All three commands must complete without error, and the last command
must output something like

```
This is process 0 of 3
This is process 1 of 3
This is process 2 of 3
```

where the lines will be output in a random order. (The output might
even be jumbled, i.e. the characters in these three lines might be
mixed up.)

If these commands do not work, then this needs to be corrected before
`mpi-hs` can work. If additional compiler options or libraries are
needed, then these need to be added to the `stack.yaml` configuration
file (for include and library paths; see `extra-include-dirs` and
`extra-lib-dirs` there) or the `package.yaml` configuration file (for
additional libraries; see `extra-libraries` there).



## Examples and Tests

### Running the example

To run the example provided in the `src` directory:

```
stack build
mpiexec stack exec version
mpiexec -n 3 stack exec example1
mpiexec -n 3 stack exec example2
```

### Running the tests

There are two test cases provided in `tests`. The first (`mpi-test`)
tests the low-level API, the second (`mpi-test-storable`) tests the
high-level API:

```
stack build --test --no-run-tests
mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test/mpi-test
mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test-storable/mpi-test-storable
```



## Related packages

There are three companion packages that provide the same high-level
API via different serialization packages. These are separate packages
to reduce the number of dependencies of `mpi-hs`:
- [`mpi-hs-binary`](https://github.com/eschnett/mpi-hs-binary), based
  on `Data.Binary` in the
  [`binary`](https://hackage.haskell.org/package/binary) package
- [`mpi-hs-cereal`](https://github.com/eschnett/mpi-hs-cereal), based
  on `Data.Serialize` in the
  [`cereal`](https://hackage.haskell.org/package/cereal) package
- [`mpi-hs-store`](https://github.com/eschnett/mpi-hs-store), based on
  `Data.Store` in the
  [`store`](https://hackage.haskell.org/package/store) package

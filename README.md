# [mpi-hs](https://github.com/eschnett/mpi-hs)

[MPI](https://www.mpi-forum.org) bindings for Haskell

* [GitHub](https://github.com/eschnett/mpi-hs): Source code repository
* [Hackage](http://hackage.haskell.org/package/mpi-hs): Haskell
  package and documentation
* [CircleCI](https://circleci.com/gh/eschnett/mpi-hs): Continuous
  integration
  [![CircleCI](https://circleci.com/gh/eschnett/mpi-hs.svg?style=svg)](https://circleci.com/gh/eschnett/mpi-hs)



## Overview

MPI (the Message Passing Interface) is widely used standard for
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



## Installing

`mpi-hs` requires an external MPI library to be available on the
system. How to install such a library is beyond the scope of these
instructions.

<!---
(It is important that the MPI library's include files, libraries, and
executables are installed consistently. A common source of problems is
that there are several MPI implementations available on a system, and
that the default include file `mpi.h`, the library `libmpi.a`, and/or
the executable `mpirun` are provided by different implementations.
This will lead to various problems, often segfaults, since neither the
operating system nor these libraries provide any protection against
such a mismatch.)
-->

In many cases, the MPI library will be installed in `/usr/include`,
`/usr/lib`, and `/usr/bin`, respectively. In this case, no further
configuration is necessary, and `mpi-hs` will build out of the box
with `stack build`.

On Ubuntu, one MPI package is `openmpi-dev`. It installs into
`/usr/lib/openmpi/include`, `/usr/lib/openmpi/lib`, and `/usr/bin/`.
You need to ensure that these settings are present in `stack.yaml`:

```yaml
extra-include-dirs:
  - /usr/lib/openmpi/include
extra-lib-dirs:
  - /usr/lib/openmpi/lib
```

On MacOS, one MPI package is the [MacPorts](https://www.macports.org)
package `openmpi`. It installs into `/opt/local/include/openmpi-mp`,
`/opt/local/lib/openmpi-mp`, and `/opt/local/bin`. You need to ensure
that these settings are present in `stack.yaml`:

```yaml
extra-include-dirs:
  - /opt/local/include/openmpi-mp
extra-lib-dirs:
  - /opt/local/lib/openmpi-mp
```

Both these settings are there by default.

### Testing the MPI installation

To test your MPI installation independently of using Haskell, copy the
example MPI C code into a file `mpi-example.c`, and run these commands:

```sh
cc -I/usr/lib/openmpi/include -c mpi-example.c
cc -o mpi-example mpi-example.o -L/usr/lib/openmpi/lib -lmpi
mpirun -np 3 ./mpi-example
```

All three commands must complete without error, and the last command
must output something like

```
This is process 0 of 3
This is process 1 of 3
This is process 2 of 3
```

where the order in which the lines are printed can be random. (The
output might even be jumbled, i.e. the characters of the three lines
might be mixed up.)

If these commands do not work, then this needs to be corrected before
`mpi-hs` can work. If additional compiler options or libraries are
needed, then these need to be added to the `stack.yaml` configuration
file (for include and library paths; see `extra-include-dirs` and
`extra-lib-dirs` there) or the `package.yaml` configuration file (for
additional libraries; see `extra-libraries` there).



## Examples and Tests

### Running the example

To run the example provided in `src/Main.hs`:

```
stack build
mpirun -np 3 stack exec example && echo SUCCESS || echo FAILURE
```

With OpenMPI, and when running on a single node (e.g. on a laptop or a
workstation), these additional `mpirun` options might be useful:

```
mpirun -np 3 --mca btl self,vader --oversubscribe stack exec example && echo SUCCESS || echo FAILURE
```

The options `--mca btl self,vader` enable the shared memory byte
transfer layer (called "vader"), and also disable any network
communication.

The option `--oversubscribe` lets you run as many MPI processes on the
local node as you want, without being limited by the physical number
of cores. This is convenient for testing.

Other MPI implementations should have equivalent (but differently
named) options.

### Running the tests

There are four test cases provided in `tests`:

```
stack build --test --no-run-tests
mpirun-openmpi-mp -np 3 --mca btl self,vader --oversubscribe stack exec $(stack path --dist-dir)/build/mpi-test/mpi-test && echo SUCCESS || echo FAILURE
mpirun-openmpi-mp -np 3 --mca btl self,vader --oversubscribe stack exec $(stack path --dist-dir)/build/mpi-test-binary/mpi-test-binary && echo SUCCESS || echo FAILURE
mpirun-openmpi-mp -np 3 --mca btl self,vader --oversubscribe stack exec $(stack path --dist-dir)/build/mpi-test-serialize/mpi-test-serialize && echo SUCCESS || echo FAILURE
mpirun-openmpi-mp -np 3 --mca btl self,vader --oversubscribe stack exec $(stack path --dist-dir)/build/mpi-test-store/mpi-test-store && echo SUCCESS || echo FAILURE
```

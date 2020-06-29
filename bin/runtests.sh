#!/bin/sh

set -euxo pipefail

stack build --flag mpi-hs:mpich-macports --test --no-run-tests --haddock --no-haddock-deps

mpiexec stack exec version
mpiexec -n 3 stack exec example1
mpiexec -n 3 stack exec example2

mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test/mpi-test
mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test-storable/mpi-test-storable

echo 'Done.'

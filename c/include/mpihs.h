#ifndef MPIHS_H
#define MPIHS_H

#include <mpi.h>

enum ComparisonResult {
  Identical = MPI_IDENT,
  Congruent = MPI_CONGRUENT,
  Similar   = MPI_SIMILAR,
  Unequal   = MPI_UNEQUAL
};

enum ThreadSupport {
  Single = MPI_THREAD_SINGLE,
  Funneled = MPI_THREAD_FUNNELED,
  Serialized = MPI_THREAD_SERIALIZED,
  Multiple = MPI_THREAD_MULTIPLE
};

int mpihs_init(void);
int mpihs_init_thread(int required, int *provided);

#endif // #ifndef MPIHS_H

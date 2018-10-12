#include <mpi.h>
#include <mpihs.h>

const int mpi_any_source = MPI_ANY_SOURCE;

const int mpi_any_tag = MPI_ANY_TAG;

const MPI_Comm mpi_comm_self = MPI_COMM_SELF;
const MPI_Comm mpi_comm_world = MPI_COMM_WORLD;

const MPI_Datatype mpi_byte = MPI_BYTE;
const MPI_Datatype mpi_char = MPI_CHAR;
const MPI_Datatype mpi_double = MPI_DOUBLE;
const MPI_Datatype mpi_float = MPI_FLOAT;
const MPI_Datatype mpi_int = MPI_INT;
const MPI_Datatype mpi_long = MPI_LONG;
const MPI_Datatype mpi_long_double = MPI_LONG_DOUBLE;
const MPI_Datatype mpi_long_long_int = MPI_LONG_LONG_INT;
const MPI_Datatype mpi_short = MPI_SHORT;
const MPI_Datatype mpi_unsigned = MPI_UNSIGNED;
const MPI_Datatype mpi_unsigned_char = MPI_UNSIGNED_CHAR;
const MPI_Datatype mpi_unsigned_long = MPI_UNSIGNED_LONG;
const MPI_Datatype mpi_unsigned_short = MPI_UNSIGNED_SHORT;



// See GHC's includes/rts/Flags.h
extern int rts_argc;
extern char **rts_argv;

int mpihs_init(void) { return MPI_Init(&rts_argc, &rts_argv); }

int mpihs_init_thread(int required, int *provided) {
  return MPI_Init_thread(&rts_argc, &rts_argv, required, provided);
}

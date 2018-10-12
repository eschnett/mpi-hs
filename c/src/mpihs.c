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

const MPI_Op mpi_band = MPI_BAND;
const MPI_Op mpi_bor = MPI_BOR;
const MPI_Op mpi_bxor = MPI_BXOR;
const MPI_Op mpi_land = MPI_LAND;
const MPI_Op mpi_lor = MPI_LOR;
const MPI_Op mpi_lxor = MPI_LXOR;
const MPI_Op mpi_max = MPI_MAX;
const MPI_Op mpi_maxloc = MPI_MAXLOC;
const MPI_Op mpi_min = MPI_MIN;
const MPI_Op mpi_minloc = MPI_MINLOC;
const MPI_Op mpi_prod = MPI_PROD;
const MPI_Op mpi_sum = MPI_SUM;



// See GHC's includes/rts/Flags.h
extern int rts_argc;
extern char **rts_argv;

int mpihs_init(void) { return MPI_Init(&rts_argc, &rts_argv); }

int mpihs_init_thread(int required, int *provided) {
  return MPI_Init_thread(&rts_argc, &rts_argv, required, provided);
}

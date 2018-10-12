#include <mpi.h>
#include <mpihs.h>

const int hsmpi_any_source = MPI_ANY_SOURCE;

const int hsmpi_any_tag = MPI_ANY_TAG;

const MPI_Comm hsmpi_comm_self = MPI_COMM_SELF;
const MPI_Comm hsmpi_comm_world = MPI_COMM_WORLD;

const MPI_Datatype hsmpi_byte = MPI_BYTE;
const MPI_Datatype hsmpi_char = MPI_CHAR;
const MPI_Datatype hsmpi_double = MPI_DOUBLE;
const MPI_Datatype hsmpi_float = MPI_FLOAT;
const MPI_Datatype hsmpi_int = MPI_INT;
const MPI_Datatype hsmpi_long = MPI_LONG;
const MPI_Datatype hsmpi_long_double = MPI_LONG_DOUBLE;
const MPI_Datatype hsmpi_long_long_int = MPI_LONG_LONG_INT;
const MPI_Datatype hsmpi_short = MPI_SHORT;
const MPI_Datatype hsmpi_unsigned = MPI_UNSIGNED;
const MPI_Datatype hsmpi_unsigned_char = MPI_UNSIGNED_CHAR;
const MPI_Datatype hsmpi_unsigned_long = MPI_UNSIGNED_LONG;
const MPI_Datatype hsmpi_unsigned_short = MPI_UNSIGNED_SHORT;

const MPI_Op hsmpi_band = MPI_BAND;
const MPI_Op hsmpi_bor = MPI_BOR;
const MPI_Op hsmpi_bxor = MPI_BXOR;
const MPI_Op hsmpi_land = MPI_LAND;
const MPI_Op hsmpi_lor = MPI_LOR;
const MPI_Op hsmpi_lxor = MPI_LXOR;
const MPI_Op hsmpi_max = MPI_MAX;
const MPI_Op hsmpi_maxloc = MPI_MAXLOC;
const MPI_Op hsmpi_min = MPI_MIN;
const MPI_Op hsmpi_minloc = MPI_MINLOC;
const MPI_Op hsmpi_prod = MPI_PROD;
const MPI_Op hsmpi_sum = MPI_SUM;

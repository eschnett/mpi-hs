#include <mpi.h>
#include <mpihs.h>

// Comm
void mpihs_get_comm_null(MPI_Comm *comm) { *comm = MPI_COMM_NULL; }
void mpihs_get_comm_self(MPI_Comm *comm) { *comm = MPI_COMM_SELF; }
void mpihs_get_comm_world(MPI_Comm *comm) { *comm = MPI_COMM_WORLD; }

// Datatype
void mpihs_get_datatype_null(MPI_Datatype *datatype) {
  *datatype = MPI_DATATYPE_NULL;
}

void mpihs_get_byte(MPI_Datatype *datatype) { *datatype = MPI_BYTE; }
void mpihs_get_char(MPI_Datatype *datatype) { *datatype = MPI_CHAR; }
void mpihs_get_double(MPI_Datatype *datatype) { *datatype = MPI_DOUBLE; }
void mpihs_get_float(MPI_Datatype *datatype) { *datatype = MPI_FLOAT; }
void mpihs_get_int(MPI_Datatype *datatype) { *datatype = MPI_INT; }
void mpihs_get_long(MPI_Datatype *datatype) { *datatype = MPI_LONG; }
void mpihs_get_long_double(MPI_Datatype *datatype) {
  *datatype = MPI_LONG_DOUBLE;
}
void mpihs_get_long_long_int(MPI_Datatype *datatype) {
  *datatype = MPI_LONG_LONG_INT;
}
void mpihs_get_short(MPI_Datatype *datatype) { *datatype = MPI_SHORT; }
void mpihs_get_unsigned(MPI_Datatype *datatype) { *datatype = MPI_UNSIGNED; }
void mpihs_get_unsigned_char(MPI_Datatype *datatype) {
  *datatype = MPI_UNSIGNED_CHAR;
}
void mpihs_get_unsigned_long(MPI_Datatype *datatype) {
  *datatype = MPI_UNSIGNED_LONG;
}
void mpihs_get_unsigned_short(MPI_Datatype *datatype) {
  *datatype = MPI_UNSIGNED_SHORT;
}

// Op
void mpihs_get_op_null(MPI_Op *op) { *op = MPI_OP_NULL; }

void mpihs_get_band(MPI_Op *op) { *op = MPI_BAND; }
void mpihs_get_bor(MPI_Op *op) { *op = MPI_BOR; }
void mpihs_get_bxor(MPI_Op *op) { *op = MPI_BXOR; }
void mpihs_get_land(MPI_Op *op) { *op = MPI_LAND; }
void mpihs_get_lor(MPI_Op *op) { *op = MPI_LOR; }
void mpihs_get_lxor(MPI_Op *op) { *op = MPI_LXOR; }
void mpihs_get_max(MPI_Op *op) { *op = MPI_MAX; }
void mpihs_get_maxloc(MPI_Op *op) { *op = MPI_MAXLOC; }
void mpihs_get_min(MPI_Op *op) { *op = MPI_MIN; }
void mpihs_get_minloc(MPI_Op *op) { *op = MPI_MINLOC; }
void mpihs_get_prod(MPI_Op *op) { *op = MPI_PROD; }
void mpihs_get_sum(MPI_Op *op) { *op = MPI_SUM; }

// Rank
int mpihs_get_any_source() { return MPI_ANY_SOURCE; }

// Request
void mpihs_get_request_null(MPI_Request *request) {
  *request = MPI_REQUEST_NULL;
}

// Status
MPI_Status *mpihs_get_status_ignore() { return MPI_STATUS_IGNORE; }

// Tag
int mpihs_get_any_tag() { return MPI_ANY_TAG; }

// Wrappers
int mpihs_iprobe(int source, int tag, MPI_Comm *comm, int *flag,
                 MPI_Status *status) {
  return MPI_Iprobe(source, tag, *comm, flag, status);
}

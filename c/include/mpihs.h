#ifndef MPIHS_H
#define MPIHS_H

#include <mpi.h>

enum ComparisonResult {
  Identical = MPI_IDENT,
  Congruent = MPI_CONGRUENT,
  Similar = MPI_SIMILAR,
  Unequal = MPI_UNEQUAL
};

enum ThreadSupport {
  ThreadSingle = MPI_THREAD_SINGLE,
  ThreadFunneled = MPI_THREAD_FUNNELED,
  ThreadSerialized = MPI_THREAD_SERIALIZED,
  ThreadMultiple = MPI_THREAD_MULTIPLE
};

// Comm
void mpihs_get_comm_null(MPI_Comm *comm);
void mpihs_get_comm_self(MPI_Comm *comm);
void mpihs_get_comm_world(MPI_Comm *comm);

// Count
int mpihs_get_undefined();

// Datatype
void mpihs_get_datatype_null(MPI_Datatype *datatype);

void mpihs_get_byte(MPI_Datatype *datatype);
void mpihs_get_char(MPI_Datatype *datatype);
void mpihs_get_double(MPI_Datatype *datatype);
void mpihs_get_float(MPI_Datatype *datatype);
void mpihs_get_int(MPI_Datatype *datatype);
void mpihs_get_long(MPI_Datatype *datatype);
void mpihs_get_long_double(MPI_Datatype *datatype);
void mpihs_get_long_long_int(MPI_Datatype *datatype);
void mpihs_get_short(MPI_Datatype *datatype);
void mpihs_get_unsigned(MPI_Datatype *datatype);
void mpihs_get_unsigned_char(MPI_Datatype *datatype);
void mpihs_get_unsigned_long(MPI_Datatype *datatype);
void mpihs_get_unsigned_short(MPI_Datatype *datatype);

// Op
void mpihs_get_op_null(MPI_Op *op);

void mpihs_get_band(MPI_Op *op);
void mpihs_get_bor(MPI_Op *op);
void mpihs_get_bxor(MPI_Op *op);
void mpihs_get_land(MPI_Op *op);
void mpihs_get_lor(MPI_Op *op);
void mpihs_get_lxor(MPI_Op *op);
void mpihs_get_max(MPI_Op *op);
void mpihs_get_maxloc(MPI_Op *op);
void mpihs_get_min(MPI_Op *op);
void mpihs_get_minloc(MPI_Op *op);
void mpihs_get_prod(MPI_Op *op);
void mpihs_get_sum(MPI_Op *op);

// Rank
int mpihs_get_any_source();

// Request
void mpihs_get_request_null(MPI_Request *request);

// Status
MPI_Status *mpihs_get_status_ignore();

// Tag
int mpihs_get_any_tag();

#endif // #ifndef MPIHS_H

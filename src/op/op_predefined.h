/*
 * $HEADER$
 */

#ifndef OMPI_OP_PREDEFINED_H
#define OMPI_OP_PREDEFINED_H

#include "op/op.h"

/*
 * Since we have so many of these, and they're all identical except
 * for the name, use macros to prototype them.
 */
#define OMPI_OP_PROTO (void *in, void *out, int *count, MPI_Datatype *dtype)
#define OMPI_OP_HANDLER_C_INTEGER(name) \
  void ompi_mpi_op_##name##_int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_long OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_short OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_unsigned_short OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_unsigned OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_unsigned_long OMPI_OP_PROTO

#define OMPI_OP_HANDLER_FORTRAN_INTEGER(name) \
  void ompi_mpi_op_##name##_fortran_integer OMPI_OP_PROTO

#define OMPI_OP_HANDLER_FLOATING_POINT(name) \
  void ompi_mpi_op_##name##_float OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_double OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_fortran_real OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_fortran_double_precision OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_long_double OMPI_OP_PROTO

#define OMPI_OP_HANDLER_LOGICAL(name) \
  void ompi_mpi_op_##name##_fortran_logical OMPI_OP_PROTO

#define OMPI_OP_HANDLER_COMPLEX(name) \
  void ompi_mpi_op_##name##_fortran_complex OMPI_OP_PROTO

#define OMPI_OP_HANDLER_BYTE(name) \
  void ompi_mpi_op_##name##_byte OMPI_OP_PROTO

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Handler functions for MPI_MAX
 */
  OMPI_OP_HANDLER_C_INTEGER(max);
  OMPI_OP_HANDLER_FORTRAN_INTEGER(max);
  OMPI_OP_HANDLER_FLOATING_POINT(max);

/**
 * Handler functions for MPI_MIN
 */
  OMPI_OP_HANDLER_C_INTEGER(min);
  OMPI_OP_HANDLER_FORTRAN_INTEGER(min);
  OMPI_OP_HANDLER_FLOATING_POINT(min);

/**
 * Handler functions for MPI_SUM
 */
  OMPI_OP_HANDLER_C_INTEGER(sum);
  OMPI_OP_HANDLER_FORTRAN_INTEGER(sum);
  OMPI_OP_HANDLER_FLOATING_POINT(sum);
  OMPI_OP_HANDLER_COMPLEX(sum);

/**
 * Handler functions for MPI_PROD
 */
  OMPI_OP_HANDLER_C_INTEGER(prod);
  OMPI_OP_HANDLER_FORTRAN_INTEGER(prod);
  OMPI_OP_HANDLER_FLOATING_POINT(prod);
  OMPI_OP_HANDLER_COMPLEX(prod);

/**
 * Handler functions for MPI_LAND
 */
  OMPI_OP_HANDLER_C_INTEGER(land);
  OMPI_OP_HANDLER_LOGICAL(land);

/**
 * Handler functions for MPI_BAND
 */
  OMPI_OP_HANDLER_C_INTEGER(band);
  OMPI_OP_HANDLER_FORTRAN_INTEGER(band);
  OMPI_OP_HANDLER_BYTE(band);

/**
 * Handler functions for MPI_LOR
 */
  OMPI_OP_HANDLER_C_INTEGER(lor);
  OMPI_OP_HANDLER_LOGICAL(lor);

/**
 * Handler functions for MPI_BOR
 */
  OMPI_OP_HANDLER_C_INTEGER(bor);
  OMPI_OP_HANDLER_FORTRAN_INTEGER(bor);
  OMPI_OP_HANDLER_BYTE(bor);

/**
 * Handler functions for MPI_LXOR
 */
  OMPI_OP_HANDLER_C_INTEGER(lxor);
  OMPI_OP_HANDLER_LOGICAL(lxor);

/**
 * Handler functions for MPI_BXOR
 */
  OMPI_OP_HANDLER_C_INTEGER(bxor);
  OMPI_OP_HANDLER_FORTRAN_INTEGER(bxor);
  OMPI_OP_HANDLER_BYTE(bxor);

/**
 * Handler functions for MPI_MAXLOC
 */

/**
 * Handler functions for MPI_MINLOC
 */

#endif /* OMPI_OP_PREDEFINED_H */

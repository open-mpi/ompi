/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_OP_PREDEFINED_H
#define OMPI_OP_PREDEFINED_H

#include "ompi/op/op.h"

/*
 * Since we have so many of these, and they're all identical except
 * for the name, use macros to prototype them.
 */
#define OMPI_OP_PROTO (void *in, void *out, int *count, MPI_Datatype *dtype)

/* C integer */

#define OMPI_OP_HANDLER_C_INTEGER_INTRINSIC(name) \
  void ompi_mpi_op_##name##_unsigned_char OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_signed_char OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_long OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_short OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_unsigned_short OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_unsigned OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_unsigned_long OMPI_OP_PROTO;
#if HAVE_LONG_LONG
#define OMPI_OP_HANDLER_C_INTEGER_OPTIONAL(name) \
  void ompi_mpi_op_##name##_long_long_int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_long_long OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_unsigned_long_long OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_C_INTEGER_OPTIONAL(name)
#endif
#define OMPI_OP_HANDLER_C_INTEGER(name) \
  OMPI_OP_HANDLER_C_INTEGER_INTRINSIC(name) \
  OMPI_OP_HANDLER_C_INTEGER_OPTIONAL(name) \

/* Fortran integer */

#define OMPI_OP_HANDLER_FORTRAN_INTEGER_INTRINSIC(name) \
  void ompi_mpi_op_##name##_fortran_integer OMPI_OP_PROTO;
#if OMPI_HAVE_FORTRAN_INTEGER1
#define OMPI_OP_HANDLER_FORTRAN_INTEGER1(name) \
  void ompi_mpi_op_##name##_fortran_integer1 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER1(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
#define OMPI_OP_HANDLER_FORTRAN_INTEGER2(name) \
  void ompi_mpi_op_##name##_fortran_integer2 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER2(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
#define OMPI_OP_HANDLER_FORTRAN_INTEGER4(name) \
  void ompi_mpi_op_##name##_fortran_integer4 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER4(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
#define OMPI_OP_HANDLER_FORTRAN_INTEGER8(name) \
  void ompi_mpi_op_##name##_fortran_integer8 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER8(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
#define OMPI_OP_HANDLER_FORTRAN_INTEGER16(name) \
  void ompi_mpi_op_##name##_fortran_integer16 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER16(name)
#endif
#define OMPI_OP_HANDLER_FORTRAN_INTEGER(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER_INTRINSIC(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER1(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER2(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER4(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER8(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER16(name)

/* Floating point */

#define OMPI_OP_HANDLER_FLOATING_POINT_INTRINSIC(name) \
  void ompi_mpi_op_##name##_float OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_double OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_fortran_real OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_fortran_double_precision OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_long_double OMPI_OP_PROTO;
#if OMPI_HAVE_FORTRAN_REAL4
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL4(name) \
  void ompi_mpi_op_##name##_fortran_real4 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL4(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL8(name) \
  void ompi_mpi_op_##name##_fortran_real8 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL8(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL16(name) \
  void ompi_mpi_op_##name##_fortran_real16 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL16(name)
#endif
#define OMPI_OP_HANDLER_FLOATING_POINT(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_INTRINSIC(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_REAL4(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_REAL8(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_REAL16(name) \

/* Logical */

#define OMPI_OP_HANDLER_LOGICAL(name) \
  void ompi_mpi_op_##name##_fortran_logical OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_bool OMPI_OP_PROTO;

/* Complex */

#if OMPI_HAVE_FORTRAN_REAL
#define OMPI_OP_HANDLER_COMPLEX_INTRINSIC(name) \
  void ompi_mpi_op_##name##_fortran_complex OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX_INTRINSIC(name)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
#define OMPI_OP_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name) \
  void ompi_mpi_op_##name##_fortran_double_complex OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
#define OMPI_OP_HANDLER_COMPLEX8(name) \
  void ompi_mpi_op_##name##_fortran_complex8 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX8(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define OMPI_OP_HANDLER_COMPLEX16(name) \
  void ompi_mpi_op_##name##_fortran_complex16 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX16(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
#define OMPI_OP_HANDLER_COMPLEX32(name) \
  void ompi_mpi_op_##name##_fortran_complex32 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX32(name)
#endif
#define OMPI_OP_HANDLER_COMPLEX(name) \
  OMPI_OP_HANDLER_COMPLEX_INTRINSIC(name) \
  OMPI_OP_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name) \
  OMPI_OP_HANDLER_COMPLEX8(name) \
  OMPI_OP_HANDLER_COMPLEX16(name) \
  OMPI_OP_HANDLER_COMPLEX32(name)

/* Byte */

#define OMPI_OP_HANDLER_BYTE(name) \
  void ompi_mpi_op_##name##_byte OMPI_OP_PROTO;

/* "2 type" */

#define OMPI_OP_HANDLER_2TYPE(name) \
  void ompi_mpi_op_##name##_2real OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_2double_precision OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_2integer OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_float_int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_double_int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_long_int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_2int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_short_int OMPI_OP_PROTO; \
  void ompi_mpi_op_##name##_long_double_int OMPI_OP_PROTO;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Handler functions for MPI_MAX
 */
  OMPI_OP_HANDLER_C_INTEGER(max)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(max)
  OMPI_OP_HANDLER_FLOATING_POINT(max)

/**
 * Handler functions for MPI_MIN
 */
  OMPI_OP_HANDLER_C_INTEGER(min)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(min)
  OMPI_OP_HANDLER_FLOATING_POINT(min)

/**
 * Handler functions for MPI_SUM
 */
  OMPI_OP_HANDLER_C_INTEGER(sum)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(sum)
  OMPI_OP_HANDLER_FLOATING_POINT(sum)
  OMPI_OP_HANDLER_COMPLEX(sum)

/**
 * Handler functions for MPI_PROD
 */
  OMPI_OP_HANDLER_C_INTEGER(prod)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(prod)
  OMPI_OP_HANDLER_FLOATING_POINT(prod)
  OMPI_OP_HANDLER_COMPLEX(prod)

/**
 * Handler functions for MPI_LAND
 */
  OMPI_OP_HANDLER_C_INTEGER(land)
  OMPI_OP_HANDLER_LOGICAL(land)

/**
 * Handler functions for MPI_BAND
 */
  OMPI_OP_HANDLER_C_INTEGER(band)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(band)
  OMPI_OP_HANDLER_BYTE(band)

/**
 * Handler functions for MPI_LOR
 */
  OMPI_OP_HANDLER_C_INTEGER(lor)
  OMPI_OP_HANDLER_LOGICAL(lor)

/**
 * Handler functions for MPI_BOR
 */
  OMPI_OP_HANDLER_C_INTEGER(bor)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(bor)
  OMPI_OP_HANDLER_BYTE(bor)

/**
 * Handler functions for MPI_LXOR
 */
  OMPI_OP_HANDLER_C_INTEGER(lxor)
  OMPI_OP_HANDLER_LOGICAL(lxor)

/**
 * Handler functions for MPI_BXOR
 */
  OMPI_OP_HANDLER_C_INTEGER(bxor)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(bxor)
  OMPI_OP_HANDLER_BYTE(bxor)

/**
 * Handler functions for MPI_MAXLOC
 */
  OMPI_OP_HANDLER_2TYPE(maxloc)

/**
 * Handler functions for MPI_MINLOC
 */
  OMPI_OP_HANDLER_2TYPE(minloc)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_OP_PREDEFINED_H */

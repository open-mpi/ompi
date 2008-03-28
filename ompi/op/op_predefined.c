/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "ompi/op/op.h"
#include "ompi/op/op_predefined.h"


/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out op in).
 */
#define OP_FUNC(name, type_name, type, op) \
  void ompi_mpi_op_##name##_##type_name(void *in, void *out, int *count, \
                                        MPI_Datatype *dtype)             \
  {                                                                      \
    int i;                                                               \
    type *a = (type *) in;                                               \
    type *b = (type *) out;                                              \
    for (i = 0; i < *count; ++i) {                                       \
      *(b++) op *(a++);                                                  \
    }                                                                    \
  }

#define COMPLEX_OP_FUNC_SUM(type_name, type) \
  void ompi_mpi_op_sum_##type_name(void *in, void *out, int *count,      \
                                   MPI_Datatype *dtype)                  \
  {                                                                      \
    int i;                                                               \
    type *a = (type *) in;                                               \
    type *b = (type *) out;                                              \
    for (i = 0; i < *count; ++i, ++b, ++a) {                             \
      b->real += a->real;                                                \
      b->imag += a->imag;                                                \
    }                                                                    \
  }

#define COMPLEX_OP_FUNC_PROD(type_name, type) \
  void ompi_mpi_op_prod_##type_name(void *in, void *out, int *count,     \
                                   MPI_Datatype *dtype)                  \
  {                                                                      \
    int i;                                                               \
    type *a = (type *) in;                                               \
    type *b = (type *) out;                                              \
    type temp;                                                           \
    for (i = 0; i < *count; ++i, ++b, ++a) {                             \
      temp.real = a->real * b->real - a->imag * b->imag;                 \
      temp.imag = a->imag * b->real + a->real * b->imag;                 \
      *b = temp;                                                         \
    }                                                                    \
  }


/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out = op(out, in))
 */
#define FUNC_FUNC(name, type_name, type) \
  void ompi_mpi_op_##name##_##type_name(void *in, void *out, int *count, \
                                        MPI_Datatype *dtype)             \
  {                                                                      \
    int i;                                                               \
    type *a = (type *) in;                                               \
    type *b = (type *) out;                                              \
    for (i = 0; i < *count; ++i) {                                       \
      *(b) = current_func(*(b), *(a));                                   \
      ++b;                                                               \
      ++a;                                                               \
    }                                                                    \
  }

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */
#define LOC_STRUCT(type_name, type1, type2) \
  typedef struct { \
    type1 v; \
    type2 k; \
  } ompi_op_predefined_##type_name##_t;

#define LOC_FUNC(name, type_name, op) \
  void ompi_mpi_op_##name##_##type_name(void *in, void *out, int *count, \
                                        MPI_Datatype *dtype) \
  { \
    int i; \
    ompi_op_predefined_##type_name##_t *a = (ompi_op_predefined_##type_name##_t*) in; \
    ompi_op_predefined_##type_name##_t *b = (ompi_op_predefined_##type_name##_t*) out; \
    for (i = 0; i < *count; ++i, ++a, ++b) { \
      if (a->v op b->v) { \
        b->v = a->v; \
        b->k = a->k; \
      } else if (a->v == b->v) { \
        b->k = (b->k < a->k ? b->k : a->k); \
      } \
    } \
  }

/*************************************************************************
 * Max
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
/* C integer */
FUNC_FUNC(max, signed_char, signed char)
FUNC_FUNC(max, unsigned_char, unsigned char)
FUNC_FUNC(max, int, int)
FUNC_FUNC(max, long, long)
FUNC_FUNC(max, short, short)
FUNC_FUNC(max, unsigned_short, unsigned short)
FUNC_FUNC(max, unsigned, unsigned)
FUNC_FUNC(max, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(max, long_long_int, long long int)
FUNC_FUNC(max, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(max, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(max, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(max, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(max, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(max, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(max, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC(max, float, float)
FUNC_FUNC(max, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC(max, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC(max, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC(max, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC(max, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC(max, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC(max, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC(max, fortran_real16, ompi_fortran_real16_t)
#endif


/*************************************************************************
 * Min
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
/* C integer */
FUNC_FUNC(min, signed_char, signed char)
FUNC_FUNC(min, unsigned_char, unsigned char)
FUNC_FUNC(min, int, int)
FUNC_FUNC(min, long, long)
FUNC_FUNC(min, short, short)
FUNC_FUNC(min, unsigned_short, unsigned short)
FUNC_FUNC(min, unsigned, unsigned)
FUNC_FUNC(min, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(min, long_long_int, long long int)
FUNC_FUNC(min, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(min, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(min, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(min, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(min, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(min, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(min, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC(min, float, float)
FUNC_FUNC(min, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC(min, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC(min, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC(min, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC(min, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC(min, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC(min, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC(min, fortran_real16, ompi_fortran_real16_t)
#endif

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC(sum, signed_char, signed char, +=)
OP_FUNC(sum, unsigned_char, unsigned char, +=)
OP_FUNC(sum, int, int, +=)
OP_FUNC(sum, long, long, +=)
OP_FUNC(sum, short, short, +=)
OP_FUNC(sum, unsigned_short, unsigned short, +=)
OP_FUNC(sum, unsigned, unsigned, +=)
OP_FUNC(sum, unsigned_long, unsigned long, +=)
#if HAVE_LONG_LONG
OP_FUNC(sum, long_long_int, long long int, +=)
OP_FUNC(sum, unsigned_long_long, unsigned long long, +=)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC(sum, fortran_integer, ompi_fortran_integer_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC(sum, fortran_integer1, ompi_fortran_integer1_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC(sum, fortran_integer2, ompi_fortran_integer2_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC(sum, fortran_integer4, ompi_fortran_integer4_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC(sum, fortran_integer8, ompi_fortran_integer8_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC(sum, fortran_integer16, ompi_fortran_integer16_t, +=)
#endif
/* Floating point */
OP_FUNC(sum, float, float, +=)
OP_FUNC(sum, double, double, +=)
#if HAVE_LONG_DOUBLE
OP_FUNC(sum, long_double, long double, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC(sum, fortran_real, ompi_fortran_real_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC(sum, fortran_double_precision, ompi_fortran_double_precision_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC(sum, fortran_real2, ompi_fortran_real2_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC(sum, fortran_real4, ompi_fortran_real4_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC(sum, fortran_real8, ompi_fortran_real8_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC(sum, fortran_real16, ompi_fortran_real16_t, +=)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_SUM(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_SUM(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_SUM(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC(prod, signed_char, signed char, *=)
OP_FUNC(prod, unsigned_char, unsigned char, *=)
OP_FUNC(prod, int, int, *=)
OP_FUNC(prod, long, long, *=)
OP_FUNC(prod, short, short, *=)
OP_FUNC(prod, unsigned_short, unsigned short, *=)
OP_FUNC(prod, unsigned, unsigned, *=)
OP_FUNC(prod, unsigned_long, unsigned long, *=)
#if HAVE_LONG_LONG
OP_FUNC(prod, long_long_int, long long int, *=)
OP_FUNC(prod, unsigned_long_long, unsigned long long, *=)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC(prod, fortran_integer, ompi_fortran_integer_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC(prod, fortran_integer1, ompi_fortran_integer1_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC(prod, fortran_integer2, ompi_fortran_integer2_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC(prod, fortran_integer4, ompi_fortran_integer4_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC(prod, fortran_integer8, ompi_fortran_integer8_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC(prod, fortran_integer16, ompi_fortran_integer16_t, *=)
#endif
/* Floating point */
OP_FUNC(prod, float, float, *=)
OP_FUNC(prod, double, double, *=)
#if HAVE_LONG_DOUBLE
OP_FUNC(prod, long_double, long double, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC(prod, fortran_real, ompi_fortran_real_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC(prod, fortran_double_precision, ompi_fortran_double_precision_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC(prod, fortran_real2, ompi_fortran_real2_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC(prod, fortran_real4, ompi_fortran_real4_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC(prod, fortran_real8, ompi_fortran_real8_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC(prod, fortran_real16, ompi_fortran_real16_t, *=)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_PROD(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_PROD(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_PROD(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC(land, unsigned_char, unsigned char)
FUNC_FUNC(land, signed_char, signed char)
FUNC_FUNC(land, int, int)
FUNC_FUNC(land, long, long)
FUNC_FUNC(land, short, short)
FUNC_FUNC(land, unsigned_short, unsigned short)
FUNC_FUNC(land, unsigned, unsigned)
FUNC_FUNC(land, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(land, long_long_int, long long int)
FUNC_FUNC(land, unsigned_long_long, unsigned long long)
#endif
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC(land, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC(land, bool, bool)

/*************************************************************************
 * Logical OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) || (b))
/* C integer */
FUNC_FUNC(lor, unsigned_char, unsigned char)
FUNC_FUNC(lor, signed_char, signed char)
FUNC_FUNC(lor, int, int)
FUNC_FUNC(lor, long, long)
FUNC_FUNC(lor, short, short)
FUNC_FUNC(lor, unsigned_short, unsigned short)
FUNC_FUNC(lor, unsigned, unsigned)
FUNC_FUNC(lor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(lor, long_long_int, long long int)
FUNC_FUNC(lor, unsigned_long_long, unsigned long long)
#endif
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC(lor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC(lor, bool, bool)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a ? 1 : 0) ^ (b ? 1: 0))
/* C integer */
FUNC_FUNC(lxor, unsigned_char, unsigned char)
FUNC_FUNC(lxor, signed_char, signed char)
FUNC_FUNC(lxor, int, int)
FUNC_FUNC(lxor, long, long)
FUNC_FUNC(lxor, short, short)
FUNC_FUNC(lxor, unsigned_short, unsigned short)
FUNC_FUNC(lxor, unsigned, unsigned)
FUNC_FUNC(lxor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(lxor, long_long_int, long long int)
FUNC_FUNC(lxor, unsigned_long_long, unsigned long long)
#endif
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC(lxor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC(lxor, bool, bool)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) & (b))
/* C integer */
FUNC_FUNC(band, unsigned_char, unsigned char)
FUNC_FUNC(band, signed_char, signed char)
FUNC_FUNC(band, int, int)
FUNC_FUNC(band, long, long)
FUNC_FUNC(band, short, short)
FUNC_FUNC(band, unsigned_short, unsigned short)
FUNC_FUNC(band, unsigned, unsigned)
FUNC_FUNC(band, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(band, long_long_int, long long int)
FUNC_FUNC(band, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(band, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(band, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(band, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(band, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(band, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(band, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC(band, byte, char)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) | (b))
/* C integer */
FUNC_FUNC(bor, unsigned_char, unsigned char)
FUNC_FUNC(bor, signed_char, signed char)
FUNC_FUNC(bor, int, int)
FUNC_FUNC(bor, long, long)
FUNC_FUNC(bor, short, short)
FUNC_FUNC(bor, unsigned_short, unsigned short)
FUNC_FUNC(bor, unsigned, unsigned)
FUNC_FUNC(bor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(bor, long_long_int, long long int)
FUNC_FUNC(bor, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(bor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(bor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(bor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(bor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(bor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(bor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC(bor, byte, char)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) ^ (b))
/* C integer */
FUNC_FUNC(bxor, unsigned_char, unsigned char)
FUNC_FUNC(bxor, signed_char, signed char)
FUNC_FUNC(bxor, int, int)
FUNC_FUNC(bxor, long, long)
FUNC_FUNC(bxor, short, short)
FUNC_FUNC(bxor, unsigned_short, unsigned short)
FUNC_FUNC(bxor, unsigned, unsigned)
FUNC_FUNC(bxor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(bxor, long_long_int, long long int)
FUNC_FUNC(bxor, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(bxor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(bxor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(bxor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(bxor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(bxor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(bxor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC(bxor, byte, char)

/*************************************************************************
 * Min and max location "pair" datatypes
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_STRUCT(2real, ompi_fortran_real_t, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_STRUCT(2double_precision, ompi_fortran_double_precision_t, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_STRUCT(2integer, ompi_fortran_integer_t, ompi_fortran_integer_t)
#endif
LOC_STRUCT(float_int, float, int)
LOC_STRUCT(double_int, double, int)
LOC_STRUCT(long_int, long, int)
LOC_STRUCT(2int, int, int)
LOC_STRUCT(short_int, short, int)
#if HAVE_LONG_DOUBLE
LOC_STRUCT(long_double_int, long double, int)
#endif

/*************************************************************************
 * Max location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC(maxloc, 2real, >)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC(maxloc, 2double_precision, >)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC(maxloc, 2integer, >)
#endif
LOC_FUNC(maxloc, float_int, >)
LOC_FUNC(maxloc, double_int, >)
LOC_FUNC(maxloc, long_int, >)
LOC_FUNC(maxloc, 2int, >)
LOC_FUNC(maxloc, short_int, >)
#if HAVE_LONG_DOUBLE
LOC_FUNC(maxloc, long_double_int, >)
#endif

/*************************************************************************
 * Min location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC(minloc, 2real, <)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC(minloc, 2double_precision, <)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC(minloc, 2integer, <)
#endif
LOC_FUNC(minloc, float_int, <)
LOC_FUNC(minloc, double_int, <)
LOC_FUNC(minloc, long_int, <)
LOC_FUNC(minloc, 2int, <)
LOC_FUNC(minloc, short_int, <)
#if HAVE_LONG_DOUBLE
LOC_FUNC(minloc, long_double_int, <)
#endif


/*
 *  This is a three buffer (2 input and 1 output) version of the reduction
 *    routines, needed for some optimizations.
 */
#define OP_FUNC_3BUF(name, type_name, type, op) \
  void ompi_mpi_op_three_buff_##name##_##type_name(void * restrict in1, \
          void * restrict in2, void * restrict out, int *count,            \
                                        MPI_Datatype *dtype)             \
  {                                                                      \
    int i;                                                               \
    type *a1 = (type *) in1;                                             \
    type *a2 = (type *) in2;                                             \
    type *b = (type *) out;                                              \
    for (i = 0; i < *count; ++i) {                                       \
      *(b++) =  *(a1++) op *(a2++);                                      \
    }                                                                    \
  }

#define COMPLEX_OP_FUNC_SUM_3BUF(type_name, type) \
  void ompi_mpi_op_three_buff_sum_##type_name(void * restrict in1,             \
          void * restrict in2, void * restrict out, int *count,           \
                                   MPI_Datatype *dtype)                  \
  {                                                                      \
    int i;                                                               \
    type *a1 = (type *) in1;                                             \
    type *a2 = (type *) in2;                                             \
    type *b = (type *) out;                                              \
    for (i = 0; i < *count; ++i, ++b, ++a1, ++a2) {                      \
      b->real = a1->real + a2->real;                                     \
      b->imag = a1->imag + a2->imag;                                     \
    }                                                                    \
  }

#define COMPLEX_OP_FUNC_PROD_3BUF(type_name, type) \
  void ompi_mpi_op_three_buff_prod_##type_name(void * restrict in1,      \
          void * restrict in2, void * restrict out, int *count,          \
                                   MPI_Datatype *dtype)                  \
  {                                                                      \
    int i;                                                               \
    type *a1 = (type *) in1;                                             \
    type *a2 = (type *) in2;                                             \
    type *b = (type *) out;                                              \
    for (i = 0; i < *count; ++i, ++b, ++a1, ++a2) {                      \
      b->real = a1->real * a2->real - a1->imag * a2->imag;               \
      b->imag = a1->imag * a2->real + a1->real * a2->imag;               \
    }                                                                    \
  }


/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out = op(in1, in2))
 */
#define FUNC_FUNC_3BUF(name, type_name, type) \
  void ompi_mpi_op_three_buff_##name##_##type_name(void * restrict in1, \
          void * restrict in2, void * restrict out, int *count,          \
                                        MPI_Datatype *dtype)             \
  {                                                                      \
    int i;                                                               \
    type *a1 = (type *) in1;                                             \
    type *a2 = (type *) in2;                                             \
    type *b = (type *) out;                                              \
    for (i = 0; i < *count; ++i) {                                       \
      *(b) = current_func(*(a1), *(a2));                                 \
      ++b;                                                               \
      ++a1;                                                              \
      ++a2;                                                              \
    }                                                                    \
  }

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */
/*
#define LOC_STRUCT(type_name, type1, type2) \
  typedef struct { \
    type1 v; \
    type2 k; \
  } ompi_op_predefined_##type_name##_t;
*/

#define LOC_FUNC_3BUF(name, type_name, op) \
  void ompi_mpi_op_three_buff_##name##_##type_name(void * restrict in1,  \
          void * restrict in2, void * restrict out, int *count,          \
                                        MPI_Datatype *dtype)             \
  { \
    int i; \
    ompi_op_predefined_##type_name##_t *a1 = (ompi_op_predefined_##type_name##_t*) in1; \
    ompi_op_predefined_##type_name##_t *a2 = (ompi_op_predefined_##type_name##_t*) in2; \
    ompi_op_predefined_##type_name##_t *b = (ompi_op_predefined_##type_name##_t*) out; \
    for (i = 0; i < *count; ++i, ++a1, ++a2, ++b ) { \
      if (a1->v op a2->v) { \
        b->v = a1->v; \
        b->k = a1->k; \
      } else if (a1->v == a2->v) { \
        b->v = a1->v; \
        b->k = (a2->k < a1->k ? a2->k : a1->k); \
      } else {  \
        b->v = a2->v; \
        b->k = a2->k; \
      }  \
    } \
  }

/*************************************************************************
 * Max
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
/* C integer */
FUNC_FUNC_3BUF(max, signed_char, signed char)
FUNC_FUNC_3BUF(max, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(max, int, int)
FUNC_FUNC_3BUF(max, long, long)
FUNC_FUNC_3BUF(max, short, short)
FUNC_FUNC_3BUF(max, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(max, unsigned, unsigned)
FUNC_FUNC_3BUF(max, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(max, long_long_int, long long int)
FUNC_FUNC_3BUF(max, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(max, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(max, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(max, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(max, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(max, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(max, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC_3BUF(max, float, float)
FUNC_FUNC_3BUF(max, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC_3BUF(max, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC_3BUF(max, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC_3BUF(max, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC_3BUF(max, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC_3BUF(max, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC_3BUF(max, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC_3BUF(max, fortran_real16, ompi_fortran_real16_t)
#endif


/*************************************************************************
 * Min
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
/* C integer */
FUNC_FUNC_3BUF(min, signed_char, signed char)
FUNC_FUNC_3BUF(min, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(min, int, int)
FUNC_FUNC_3BUF(min, long, long)
FUNC_FUNC_3BUF(min, short, short)
FUNC_FUNC_3BUF(min, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(min, unsigned, unsigned)
FUNC_FUNC_3BUF(min, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(min, long_long_int, long long int)
FUNC_FUNC_3BUF(min, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(min, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(min, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(min, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(min, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(min, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(min, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC_3BUF(min, float, float)
FUNC_FUNC_3BUF(min, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC_3BUF(min, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC_3BUF(min, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC_3BUF(min, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC_3BUF(min, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC_3BUF(min, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC_3BUF(min, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC_3BUF(min, fortran_real16, ompi_fortran_real16_t)
#endif

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF(sum, signed_char, signed char, +)
OP_FUNC_3BUF(sum, unsigned_char, unsigned char, +)
OP_FUNC_3BUF(sum, int, int, +)
OP_FUNC_3BUF(sum, long, long, +)
OP_FUNC_3BUF(sum, short, short, +)
OP_FUNC_3BUF(sum, unsigned_short, unsigned short, +)
OP_FUNC_3BUF(sum, unsigned, unsigned, +)
OP_FUNC_3BUF(sum, unsigned_long, unsigned long, +)
#if HAVE_LONG_LONG
OP_FUNC_3BUF(sum, long_long_int, long long int, +)
OP_FUNC_3BUF(sum, unsigned_long_long, unsigned long long, +)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC_3BUF(sum, fortran_integer, ompi_fortran_integer_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC_3BUF(sum, fortran_integer1, ompi_fortran_integer1_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC_3BUF(sum, fortran_integer2, ompi_fortran_integer2_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC_3BUF(sum, fortran_integer4, ompi_fortran_integer4_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC_3BUF(sum, fortran_integer8, ompi_fortran_integer8_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC_3BUF(sum, fortran_integer16, ompi_fortran_integer16_t, +)
#endif
/* Floating point */
OP_FUNC_3BUF(sum, float, float, +)
OP_FUNC_3BUF(sum, double, double, +)
#if HAVE_LONG_DOUBLE
OP_FUNC_3BUF(sum, long_double, long double, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC_3BUF(sum, fortran_real, ompi_fortran_real_t, +)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC_3BUF(sum, fortran_double_precision, ompi_fortran_double_precision_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC_3BUF(sum, fortran_real2, ompi_fortran_real2_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC_3BUF(sum, fortran_real4, ompi_fortran_real4_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC_3BUF(sum, fortran_real8, ompi_fortran_real8_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC_3BUF(sum, fortran_real16, ompi_fortran_real16_t, +)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM_3BUF(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF(prod, signed_char, signed char, *)
OP_FUNC_3BUF(prod, unsigned_char, unsigned char, *)
OP_FUNC_3BUF(prod, int, int, *)
OP_FUNC_3BUF(prod, long, long, *)
OP_FUNC_3BUF(prod, short, short, *)
OP_FUNC_3BUF(prod, unsigned_short, unsigned short, *)
OP_FUNC_3BUF(prod, unsigned, unsigned, *)
OP_FUNC_3BUF(prod, unsigned_long, unsigned long, *)
#if HAVE_LONG_LONG
OP_FUNC_3BUF(prod, long_long_int, long long int, *)
OP_FUNC_3BUF(prod, unsigned_long_long, unsigned long long, *)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC_3BUF(prod, fortran_integer, ompi_fortran_integer_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC_3BUF(prod, fortran_integer1, ompi_fortran_integer1_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC_3BUF(prod, fortran_integer2, ompi_fortran_integer2_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC_3BUF(prod, fortran_integer4, ompi_fortran_integer4_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC_3BUF(prod, fortran_integer8, ompi_fortran_integer8_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC_3BUF(prod, fortran_integer16, ompi_fortran_integer16_t, *)
#endif
/* Floating point */
OP_FUNC_3BUF(prod, float, float, *)
OP_FUNC_3BUF(prod, double, double, *)
#if HAVE_LONG_DOUBLE
OP_FUNC_3BUF(prod, long_double, long double, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC_3BUF(prod, fortran_real, ompi_fortran_real_t, *)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC_3BUF(prod, fortran_double_precision, ompi_fortran_double_precision_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC_3BUF(prod, fortran_real2, ompi_fortran_real2_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC_3BUF(prod, fortran_real4, ompi_fortran_real4_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC_3BUF(prod, fortran_real8, ompi_fortran_real8_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC_3BUF(prod, fortran_real16, ompi_fortran_real16_t, *)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD_3BUF(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC_3BUF(land, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(land, signed_char, signed char)
FUNC_FUNC_3BUF(land, int, int)
FUNC_FUNC_3BUF(land, long, long)
FUNC_FUNC_3BUF(land, short, short)
FUNC_FUNC_3BUF(land, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(land, unsigned, unsigned)
FUNC_FUNC_3BUF(land, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(land, long_long_int, long long int)
FUNC_FUNC_3BUF(land, unsigned_long_long, unsigned long long)
#endif
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF(land, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF(land, bool, bool)

/*************************************************************************
 * Logical OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) || (b))
/* C integer */
FUNC_FUNC_3BUF(lor, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(lor, signed_char, signed char)
FUNC_FUNC_3BUF(lor, int, int)
FUNC_FUNC_3BUF(lor, long, long)
FUNC_FUNC_3BUF(lor, short, short)
FUNC_FUNC_3BUF(lor, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(lor, unsigned, unsigned)
FUNC_FUNC_3BUF(lor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(lor, long_long_int, long long int)
FUNC_FUNC_3BUF(lor, unsigned_long_long, unsigned long long)
#endif
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF(lor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF(lor, bool, bool)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a ? 1 : 0) ^ (b ? 1: 0))
/* C integer */
FUNC_FUNC_3BUF(lxor, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(lxor, signed_char, signed char)
FUNC_FUNC_3BUF(lxor, int, int)
FUNC_FUNC_3BUF(lxor, long, long)
FUNC_FUNC_3BUF(lxor, short, short)
FUNC_FUNC_3BUF(lxor, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(lxor, unsigned, unsigned)
FUNC_FUNC_3BUF(lxor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(lxor, long_long_int, long long int)
FUNC_FUNC_3BUF(lxor, unsigned_long_long, unsigned long long)
#endif
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF(lxor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF(lxor, bool, bool)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) & (b))
/* C integer */
FUNC_FUNC_3BUF(band, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(band, signed_char, signed char)
FUNC_FUNC_3BUF(band, int, int)
FUNC_FUNC_3BUF(band, long, long)
FUNC_FUNC_3BUF(band, short, short)
FUNC_FUNC_3BUF(band, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(band, unsigned, unsigned)
FUNC_FUNC_3BUF(band, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(band, long_long_int, long long int)
FUNC_FUNC_3BUF(band, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(band, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(band, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(band, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(band, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(band, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(band, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF(band, byte, char)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) | (b))
/* C integer */
FUNC_FUNC_3BUF(bor, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(bor, signed_char, signed char)
FUNC_FUNC_3BUF(bor, int, int)
FUNC_FUNC_3BUF(bor, long, long)
FUNC_FUNC_3BUF(bor, short, short)
FUNC_FUNC_3BUF(bor, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(bor, unsigned, unsigned)
FUNC_FUNC_3BUF(bor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(bor, long_long_int, long long int)
FUNC_FUNC_3BUF(bor, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(bor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(bor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(bor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(bor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(bor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(bor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF(bor, byte, char)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) ^ (b))
/* C integer */
FUNC_FUNC_3BUF(bxor, unsigned_char, unsigned char)
FUNC_FUNC_3BUF(bxor, signed_char, signed char)
FUNC_FUNC_3BUF(bxor, int, int)
FUNC_FUNC_3BUF(bxor, long, long)
FUNC_FUNC_3BUF(bxor, short, short)
FUNC_FUNC_3BUF(bxor, unsigned_short, unsigned short)
FUNC_FUNC_3BUF(bxor, unsigned, unsigned)
FUNC_FUNC_3BUF(bxor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC_3BUF(bxor, long_long_int, long long int)
FUNC_FUNC_3BUF(bxor, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(bxor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(bxor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(bxor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(bxor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(bxor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(bxor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF(bxor, byte, char)

/*************************************************************************
 * Min and max location "pair" datatypes
 *************************************************************************/

/*
#if OMPI_HAVE_FORTRAN_REAL
LOC_STRUCT_3BUF(2real, ompi_fortran_real_t, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_STRUCT_3BUF(2double_precision, ompi_fortran_double_precision_t, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_STRUCT_3BUF(2integer, ompi_fortran_integer_t, ompi_fortran_integer_t)
#endif
LOC_STRUCT_3BUF(float_int, float, int)
LOC_STRUCT_3BUF(double_int, double, int)
LOC_STRUCT_3BUF(long_int, long, int)
LOC_STRUCT_3BUF(2int, int, int)
LOC_STRUCT_3BUF(short_int, short, int)
#if HAVE_LONG_DOUBLE
LOC_STRUCT_3BUF(long_double_int, long double, int)
#endif
*/

/*************************************************************************
 * Max location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC_3BUF(maxloc, 2real, >)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC_3BUF(maxloc, 2double_precision, >)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC_3BUF(maxloc, 2integer, >)
#endif
LOC_FUNC_3BUF(maxloc, float_int, >)
LOC_FUNC_3BUF(maxloc, double_int, >)
LOC_FUNC_3BUF(maxloc, long_int, >)
LOC_FUNC_3BUF(maxloc, 2int, >)
LOC_FUNC_3BUF(maxloc, short_int, >)
#if HAVE_LONG_DOUBLE
LOC_FUNC_3BUF(maxloc, long_double_int, >)
#endif

/*************************************************************************
 * Min location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC_3BUF(minloc, 2real, <)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC_3BUF(minloc, 2double_precision, <)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC_3BUF(minloc, 2integer, <)
#endif
LOC_FUNC_3BUF(minloc, float_int, <)
LOC_FUNC_3BUF(minloc, double_int, <)
LOC_FUNC_3BUF(minloc, long_int, <)
LOC_FUNC_3BUF(minloc, 2int, <)
LOC_FUNC_3BUF(minloc, short_int, <)
#if HAVE_LONG_DOUBLE
LOC_FUNC_3BUF(minloc, long_double_int, <)
#endif

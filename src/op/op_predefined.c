/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "op/op.h"
#include "op/op_predefined.h"


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
FUNC_FUNC(max, int, int)
FUNC_FUNC(max, long, long)
FUNC_FUNC(max, short, short)
FUNC_FUNC(max, unsigned_short, unsigned short)
FUNC_FUNC(max, unsigned, unsigned)
FUNC_FUNC(max, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(max, long_long_int, long long int)
FUNC_FUNC(max, long_long, long long)
FUNC_FUNC(max, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
FUNC_FUNC(max, fortran_integer, MPI_Fint)
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
FUNC_FUNC(max, fortran_real, ompi_fortran_real_t)
FUNC_FUNC(max, fortran_double_precision, ompi_fortran_dblprec_t)
FUNC_FUNC(max, long_double, long double)
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
FUNC_FUNC(min, int, int)
FUNC_FUNC(min, long, long)
FUNC_FUNC(min, short, short)
FUNC_FUNC(min, unsigned_short, unsigned short)
FUNC_FUNC(min, unsigned, unsigned)
FUNC_FUNC(min, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(min, long_long_int, long long int)
FUNC_FUNC(min, long_long, long long)
FUNC_FUNC(min, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
FUNC_FUNC(min, fortran_integer, MPI_Fint)
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
FUNC_FUNC(min, fortran_real, ompi_fortran_real_t)
FUNC_FUNC(min, fortran_double_precision, ompi_fortran_dblprec_t)
FUNC_FUNC(min, long_double, long double)
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
OP_FUNC(sum, int, int, +=)
OP_FUNC(sum, long, long, +=)
OP_FUNC(sum, short, short, +=)
OP_FUNC(sum, unsigned_short, unsigned short, +=)
OP_FUNC(sum, unsigned, unsigned, +=)
OP_FUNC(sum, unsigned_long, unsigned long, +=)
#if HAVE_LONG_LONG
OP_FUNC(sum, long_long_int, long long int, +=)
OP_FUNC(sum, long_long, long long, +=)
OP_FUNC(sum, unsigned_long_long, unsigned long long, +=)
#endif
/* Fortran integer */
OP_FUNC(sum, fortran_integer, MPI_Fint, +=)
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
OP_FUNC(sum, fortran_real, ompi_fortran_real_t, +=)
OP_FUNC(sum, fortran_double_precision, ompi_fortran_dblprec_t, +=)
OP_FUNC(sum, long_double, long double, +=)
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
COMPLEX_OP_FUNC_SUM(fortran_complex, ompi_fortran_complex_t)
#if OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_SUM(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_SUM(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_SUM(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC(prod, int, int, *=)
OP_FUNC(prod, long, long, *=)
OP_FUNC(prod, short, short, *=)
OP_FUNC(prod, unsigned_short, unsigned short, *=)
OP_FUNC(prod, unsigned, unsigned, *=)
OP_FUNC(prod, unsigned_long, unsigned long, *=)
#if HAVE_LONG_LONG
OP_FUNC(prod, long_long_int, long long int, +=)
OP_FUNC(prod, long_long, long long, +=)
OP_FUNC(prod, unsigned_long_long, unsigned long long, +=)
#endif
/* Fortran integer */
OP_FUNC(prod, fortran_integer, MPI_Fint, *=)
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC(prod, fortran_integer1, ompi_fortran_integer1_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC(prod, fortran_integer2, ompi_fortran_integer2_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC(prod, fortran_integer4, ompi_fortran_integer4_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC(prod, fortran_integer8, ompi_fortran_integer8_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC(prod, fortran_integer16, ompi_fortran_integer16_t, +=)
#endif
/* Floating point */
OP_FUNC(prod, float, float, *=)
OP_FUNC(prod, double, double, *=)
OP_FUNC(prod, fortran_real, ompi_fortran_real_t, *=)
OP_FUNC(prod, fortran_double_precision, ompi_fortran_dblprec_t, *=)
OP_FUNC(prod, long_double, long double, *=)
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC(prod, fortran_real4, ompi_fortran_real4_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC(prod, fortran_real8, ompi_fortran_real8_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC(prod, fortran_real16, ompi_fortran_real16_t, +=)
#endif
/* Complex */
COMPLEX_OP_FUNC_PROD(fortran_complex, ompi_fortran_complex_t)
#if OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_PROD(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_PROD(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_PROD(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC(land, int, int)
FUNC_FUNC(land, long, long)
FUNC_FUNC(land, short, short)
FUNC_FUNC(land, unsigned_short, unsigned short)
FUNC_FUNC(land, unsigned, unsigned)
FUNC_FUNC(land, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(land, long_long_int, long long int)
FUNC_FUNC(land, long_long, long long)
FUNC_FUNC(land, unsigned_long_long, unsigned long long)
#endif
/* Logical */
FUNC_FUNC(land, fortran_logical, ompi_fortran_logical_t)

/*************************************************************************
 * Logical OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) || (b))
/* C integer */
FUNC_FUNC(lor, int, int)
FUNC_FUNC(lor, long, long)
FUNC_FUNC(lor, short, short)
FUNC_FUNC(lor, unsigned_short, unsigned short)
FUNC_FUNC(lor, unsigned, unsigned)
FUNC_FUNC(lor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(lor, long_long_int, long long int)
FUNC_FUNC(lor, long_long, long long)
FUNC_FUNC(lor, unsigned_long_long, unsigned long long)
#endif
/* Logical */
FUNC_FUNC(lor, fortran_logical, ompi_fortran_logical_t)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a ? 1 : 0) ^ (b ? 1: 0))
/* C integer */
FUNC_FUNC(lxor, int, int)
FUNC_FUNC(lxor, long, long)
FUNC_FUNC(lxor, short, short)
FUNC_FUNC(lxor, unsigned_short, unsigned short)
FUNC_FUNC(lxor, unsigned, unsigned)
FUNC_FUNC(lxor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(lxor, long_long_int, long long int)
FUNC_FUNC(lxor, long_long, long long)
FUNC_FUNC(lxor, unsigned_long_long, unsigned long long)
#endif
/* Logical */
FUNC_FUNC(lxor, fortran_logical, ompi_fortran_logical_t)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) & (b))
/* C integer */
FUNC_FUNC(band, int, int)
FUNC_FUNC(band, long, long)
FUNC_FUNC(band, short, short)
FUNC_FUNC(band, unsigned_short, unsigned short)
FUNC_FUNC(band, unsigned, unsigned)
FUNC_FUNC(band, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(band, long_long_int, long long int)
FUNC_FUNC(band, long_long, long long)
FUNC_FUNC(band, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
FUNC_FUNC(band, fortran_integer, MPI_Fint)
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
FUNC_FUNC(bor, int, int)
FUNC_FUNC(bor, long, long)
FUNC_FUNC(bor, short, short)
FUNC_FUNC(bor, unsigned_short, unsigned short)
FUNC_FUNC(bor, unsigned, unsigned)
FUNC_FUNC(bor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(bor, long_long_int, long long int)
FUNC_FUNC(bor, long_long, long long)
FUNC_FUNC(bor, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
FUNC_FUNC(bor, fortran_integer, MPI_Fint)
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
FUNC_FUNC(bxor, int, int)
FUNC_FUNC(bxor, long, long)
FUNC_FUNC(bxor, short, short)
FUNC_FUNC(bxor, unsigned_short, unsigned short)
FUNC_FUNC(bxor, unsigned, unsigned)
FUNC_FUNC(bxor, unsigned_long, unsigned long)
#if HAVE_LONG_LONG
FUNC_FUNC(bxor, long_long_int, long long int)
FUNC_FUNC(bxor, long_long, long long)
FUNC_FUNC(bxor, unsigned_long_long, unsigned long long)
#endif
/* Fortran integer */
FUNC_FUNC(bxor, fortran_integer, MPI_Fint)
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

LOC_STRUCT(2real, ompi_fortran_real_t, ompi_fortran_real_t)
LOC_STRUCT(2double_precision, ompi_fortran_dblprec_t, ompi_fortran_dblprec_t)
LOC_STRUCT(2integer, ompi_fortran_integer_t, ompi_fortran_integer_t)
LOC_STRUCT(float_int, float, int)
LOC_STRUCT(double_int, double, int)
LOC_STRUCT(long_int, long, int)
LOC_STRUCT(2int, int, int)
LOC_STRUCT(short_int, short, int)
LOC_STRUCT(long_double_int, long double, int)

/*************************************************************************
 * Max location
 *************************************************************************/

LOC_FUNC(maxloc, 2real, >)
LOC_FUNC(maxloc, 2double_precision, >)
LOC_FUNC(maxloc, 2integer, >)
LOC_FUNC(maxloc, float_int, >)
LOC_FUNC(maxloc, double_int, >)
LOC_FUNC(maxloc, long_int, >)
LOC_FUNC(maxloc, 2int, >)
LOC_FUNC(maxloc, short_int, >)
LOC_FUNC(maxloc, long_double_int, >)

/*************************************************************************
 * Min location
 *************************************************************************/

LOC_FUNC(minloc, 2real, <)
LOC_FUNC(minloc, 2double_precision, <)
LOC_FUNC(minloc, 2integer, <)
LOC_FUNC(minloc, float_int, <)
LOC_FUNC(minloc, double_int, <)
LOC_FUNC(minloc, long_int, <)
LOC_FUNC(minloc, 2int, <)
LOC_FUNC(minloc, short_int, <)
LOC_FUNC(minloc, long_double_int, <)

/*************************************************************************
 * Replace (for MPI_ACCUMULATE)
 *************************************************************************/

/* C integer */
OP_FUNC(replace, int, int, =)
OP_FUNC(replace, long, long, =)
OP_FUNC(replace, short, short, =)
OP_FUNC(replace, unsigned_short, unsigned short, =)
OP_FUNC(replace, unsigned, unsigned, =)
OP_FUNC(replace, unsigned_long, unsigned long, =)
/* Fortran integer */
OP_FUNC(replace, fortran_integer, MPI_Fint, =)
/* Floating point */
OP_FUNC(replace, float, float, =)
OP_FUNC(replace, double, double, =)
OP_FUNC(replace, fortran_real, ompi_fortran_real_t, =)
OP_FUNC(replace, fortran_double_precision, ompi_fortran_dblprec_t, =)
OP_FUNC(replace, long_double, long double, =)
/* Complex */
OP_FUNC(replace, fortran_complex, ompi_fortran_complex_t, =)
#if OMPI_HAVE_FORTRAN_COMPLEX8
OP_FUNC(replace, fortran_complex8, ompi_fortran_complex8_t, =)
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX16
OP_FUNC(replace, fortran_complex16, ompi_fortran_complex16_t, =)
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX32
OP_FUNC(replace, fortran_complex32, ompi_fortran_complex32_t, =)
#endif
/* Byte */
OP_FUNC(replace, byte, char, =)
/* Byte */
OP_FUNC(replace, fortran_logical, ompi_fortran_logical_t, =)

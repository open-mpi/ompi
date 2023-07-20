/*
 * Copyright (c) 2019-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <sys/types.h>

#include <hip/hip_runtime.h>
#include <hip/hip_complex.h>

#ifndef BEGIN_C_DECLS
#if defined(c_plusplus) || defined(__cplusplus)
#    define BEGIN_C_DECLS extern "C" {
#    define END_C_DECLS   }
#else
#    define BEGIN_C_DECLS /* empty */
#    define END_C_DECLS   /* empty */
#endif
#endif

BEGIN_C_DECLS

#define OP_FUNC_SIG(name, type_name, type, op)                                      \
    void ompi_op_rocm_2buff_##name##_##type_name##_submit(const type *in,           \
                                                   type *inout,                     \
                                                   int count,                       \
                                                   int threads_per_block,           \
                                                   int max_blocks,                  \
                                                   hipStream_t stream);

#define FUNC_FUNC_SIG(name, type_name, type)                                        \
    void ompi_op_rocm_2buff_##name##_##type_name##_submit(const type *in,           \
                                                   type *inout,                     \
                                                   int count,                       \
                                                   int threads_per_block,           \
                                                   int max_blocks,                  \
                                                   hipStream_t stream);

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

#define LOC_FUNC_SIG(name, type_name, op) \
    void ompi_op_rocm_2buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *a,  \
                                            ompi_op_predefined_##type_name##_t *b,                      \
                                            int count,                                                  \
                                            int threads_per_block,                                      \
                                            int max_blocks,                                             \
                                            hipStream_t stream);

/*************************************************************************
 * Max
 *************************************************************************/

/* C integer */
FUNC_FUNC_SIG(max,   int8_t,   int8_t)
FUNC_FUNC_SIG(max,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(max,  int16_t,  int16_t)
FUNC_FUNC_SIG(max, uint16_t, uint16_t)
FUNC_FUNC_SIG(max,  int32_t,  int32_t)
FUNC_FUNC_SIG(max, uint32_t, uint32_t)
FUNC_FUNC_SIG(max,  int64_t,  int64_t)
FUNC_FUNC_SIG(max, uint64_t, uint64_t)
FUNC_FUNC_SIG(max,  long,  long)
FUNC_FUNC_SIG(max,  ulong, unsigned long)

#if 0
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_SIG(max, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_SIG(max, short_float, opal_short_float_t)
#endif
#endif // 0

FUNC_FUNC_SIG(max, float, float)
FUNC_FUNC_SIG(max, double, double)
FUNC_FUNC_SIG(max, long_double, long double)

/*************************************************************************
 * Min
 *************************************************************************/

/* C integer */
FUNC_FUNC_SIG(min,   int8_t,   int8_t)
FUNC_FUNC_SIG(min,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(min,  int16_t,  int16_t)
FUNC_FUNC_SIG(min, uint16_t, uint16_t)
FUNC_FUNC_SIG(min,  int32_t,  int32_t)
FUNC_FUNC_SIG(min, uint32_t, uint32_t)
FUNC_FUNC_SIG(min,  int64_t,  int64_t)
FUNC_FUNC_SIG(min, uint64_t, uint64_t)
FUNC_FUNC_SIG(min,  long,  long)
FUNC_FUNC_SIG(min,  ulong, unsigned long)

#if 0
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_SIG(min, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_SIG(min, short_float, opal_short_float_t)
#endif
#endif // 0

FUNC_FUNC_SIG(min, float, float)
FUNC_FUNC_SIG(min, double, double)
FUNC_FUNC_SIG(min, long_double, long double)

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC_SIG(sum,   int8_t,   int8_t, +=)
OP_FUNC_SIG(sum,  uint8_t,  uint8_t, +=)
OP_FUNC_SIG(sum,  int16_t,  int16_t, +=)
OP_FUNC_SIG(sum, uint16_t, uint16_t, +=)
OP_FUNC_SIG(sum,  int32_t,  int32_t, +=)
OP_FUNC_SIG(sum, uint32_t, uint32_t, +=)
OP_FUNC_SIG(sum,  int64_t,  int64_t, +=)
OP_FUNC_SIG(sum, uint64_t, uint64_t, +=)
OP_FUNC_SIG(sum,  long,  long, +=)
OP_FUNC_SIG(sum,  ulong, unsigned long, +=)

#if 0
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_SIG(sum, short_float, short float, +=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_SIG(sum, short_float, opal_short_float_t, +=)
#endif
#endif // 0

OP_FUNC_SIG(sum, float, float, +=)
OP_FUNC_SIG(sum, double, double, +=)
OP_FUNC_SIG(sum, long_double, long double, +=)

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_SIG(sum, c_short_float_complex, short float _Complex, +=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_SUM_FUNC(c_short_float_complex, opal_short_float_t)
#endif
#endif // 0
FUNC_FUNC_SIG(sum, c_float_complex, hipFloatComplex)
FUNC_FUNC_SIG(sum, c_double_complex, hipDoubleComplex)
//OP_FUNC_SIG(sum, c_float_complex, float _Complex, +=)
//OP_FUNC_SIG(sum, c_double_complex, double _Complex, +=)
//OP_FUNC_SIG(sum, c_long_double_complex, long double _Complex, +=)

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC_SIG(prod,   int8_t,   int8_t, *=)
OP_FUNC_SIG(prod,  uint8_t,  uint8_t, *=)
OP_FUNC_SIG(prod,  int16_t,  int16_t, *=)
OP_FUNC_SIG(prod, uint16_t, uint16_t, *=)
OP_FUNC_SIG(prod,  int32_t,  int32_t, *=)
OP_FUNC_SIG(prod, uint32_t, uint32_t, *=)
OP_FUNC_SIG(prod,  int64_t,  int64_t, *=)
OP_FUNC_SIG(prod, uint64_t, uint64_t, *=)
OP_FUNC_SIG(prod,  long,  long, *=)
OP_FUNC_SIG(prod,  ulong, unsigned long, *=)

#if 0
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_SIG(prod, short_float, short float, *=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_SIG(prod, short_float, opal_short_float_t, *=)
#endif
#endif // 0

OP_FUNC_SIG(prod, float, float, *=)
OP_FUNC_SIG(prod, double, double, *=)
OP_FUNC_SIG(prod, long_double, long double, *=)

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_SIG(prod, c_short_float_complex, short float _Complex, *=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_PROD_FUNC(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC_SIG(prod, c_float_complex, float _Complex, *=)
OP_FUNC_SIG(prod, c_double_complex, double _Complex, *=)
OP_FUNC_SIG(prod, c_long_double_complex, long double _Complex, *=)
#endif // 0

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC_SIG(land,   int8_t,   int8_t)
FUNC_FUNC_SIG(land,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(land,  int16_t,  int16_t)
FUNC_FUNC_SIG(land, uint16_t, uint16_t)
FUNC_FUNC_SIG(land,  int32_t,  int32_t)
FUNC_FUNC_SIG(land, uint32_t, uint32_t)
FUNC_FUNC_SIG(land,  int64_t,  int64_t)
FUNC_FUNC_SIG(land, uint64_t, uint64_t)
FUNC_FUNC_SIG(land,  long,  long)
FUNC_FUNC_SIG(land,  ulong, unsigned long)

/* C++ bool */
FUNC_FUNC_SIG(land, bool, bool)

/*************************************************************************
 * Logical OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) || (b))
/* C integer */
FUNC_FUNC_SIG(lor,   int8_t,   int8_t)
FUNC_FUNC_SIG(lor,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(lor,  int16_t,  int16_t)
FUNC_FUNC_SIG(lor, uint16_t, uint16_t)
FUNC_FUNC_SIG(lor,  int32_t,  int32_t)
FUNC_FUNC_SIG(lor, uint32_t, uint32_t)
FUNC_FUNC_SIG(lor,  int64_t,  int64_t)
FUNC_FUNC_SIG(lor, uint64_t, uint64_t)
FUNC_FUNC_SIG(lor,  long,  long)
FUNC_FUNC_SIG(lor,  ulong, unsigned long)

/* C++ bool */
FUNC_FUNC_SIG(lor, bool, bool)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a ? 1 : 0) ^ (b ? 1: 0))
/* C integer */
FUNC_FUNC_SIG(lxor,   int8_t,   int8_t)
FUNC_FUNC_SIG(lxor,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(lxor,  int16_t,  int16_t)
FUNC_FUNC_SIG(lxor, uint16_t, uint16_t)
FUNC_FUNC_SIG(lxor,  int32_t,  int32_t)
FUNC_FUNC_SIG(lxor, uint32_t, uint32_t)
FUNC_FUNC_SIG(lxor,  int64_t,  int64_t)
FUNC_FUNC_SIG(lxor, uint64_t, uint64_t)
FUNC_FUNC_SIG(lxor,  long,  long)
FUNC_FUNC_SIG(lxor,  ulong, unsigned long)

/* C++ bool */
FUNC_FUNC_SIG(lxor, bool, bool)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) & (b))
/* C integer */
FUNC_FUNC_SIG(band,   int8_t,   int8_t)
FUNC_FUNC_SIG(band,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(band,  int16_t,  int16_t)
FUNC_FUNC_SIG(band, uint16_t, uint16_t)
FUNC_FUNC_SIG(band,  int32_t,  int32_t)
FUNC_FUNC_SIG(band, uint32_t, uint32_t)
FUNC_FUNC_SIG(band,  int64_t,  int64_t)
FUNC_FUNC_SIG(band, uint64_t, uint64_t)
FUNC_FUNC_SIG(band,  long,  long)
FUNC_FUNC_SIG(band,  ulong, unsigned long)

/* Byte */
FUNC_FUNC_SIG(band, byte, char)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) | (b))
/* C integer */
FUNC_FUNC_SIG(bor,   int8_t,   int8_t)
FUNC_FUNC_SIG(bor,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(bor,  int16_t,  int16_t)
FUNC_FUNC_SIG(bor, uint16_t, uint16_t)
FUNC_FUNC_SIG(bor,  int32_t,  int32_t)
FUNC_FUNC_SIG(bor, uint32_t, uint32_t)
FUNC_FUNC_SIG(bor,  int64_t,  int64_t)
FUNC_FUNC_SIG(bor, uint64_t, uint64_t)
FUNC_FUNC_SIG(bor,  long,  long)
FUNC_FUNC_SIG(bor,  ulong, unsigned long)

/* Byte */
FUNC_FUNC_SIG(bor, byte, char)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) ^ (b))
/* C integer */
FUNC_FUNC_SIG(bxor,   int8_t,   int8_t)
FUNC_FUNC_SIG(bxor,  uint8_t,  uint8_t)
FUNC_FUNC_SIG(bxor,  int16_t,  int16_t)
FUNC_FUNC_SIG(bxor, uint16_t, uint16_t)
FUNC_FUNC_SIG(bxor,  int32_t,  int32_t)
FUNC_FUNC_SIG(bxor, uint32_t, uint32_t)
FUNC_FUNC_SIG(bxor,  int64_t,  int64_t)
FUNC_FUNC_SIG(bxor, uint64_t, uint64_t)
FUNC_FUNC_SIG(bxor,  long,  long)
FUNC_FUNC_SIG(bxor,  ulong, unsigned long)

/* Byte */
FUNC_FUNC_SIG(bxor, byte, char)

/*************************************************************************
 * Min and max location "pair" datatypes
 *************************************************************************/

LOC_STRUCT(float_int, float, int)
LOC_STRUCT(double_int, double, int)
LOC_STRUCT(long_int, long, int)
LOC_STRUCT(2int, int, int)
LOC_STRUCT(short_int, short, int)
LOC_STRUCT(long_double_int, long double, int)
LOC_STRUCT(ulong, unsigned long, int)
/* compat types for Fortran */
LOC_STRUCT(2real, float, float)
LOC_STRUCT(2double_precision, double, double)

/*************************************************************************
 * Max location
 *************************************************************************/

LOC_FUNC_SIG(maxloc, float_int, >)
LOC_FUNC_SIG(maxloc, double_int, >)
LOC_FUNC_SIG(maxloc, long_int, >)
LOC_FUNC_SIG(maxloc, 2int, >)
LOC_FUNC_SIG(maxloc, short_int, >)
LOC_FUNC_SIG(maxloc, long_double_int, >)

/*************************************************************************
 * Min location
 *************************************************************************/

LOC_FUNC_SIG(minloc, float_int, <)
LOC_FUNC_SIG(minloc, double_int, <)
LOC_FUNC_SIG(minloc, long_int, <)
LOC_FUNC_SIG(minloc, 2int, <)
LOC_FUNC_SIG(minloc, short_int, <)
LOC_FUNC_SIG(minloc, long_double_int, <)



#define OP_FUNC_3BUF_SIG(name, type_name, type, op)                                        \
    void ompi_op_rocm_3buff_##name##_##type_name##_submit(const type *in1,                 \
                                                          const type *in2,                 \
                                                          type *inout,                     \
                                                          int count,                       \
                                                          int threads_per_block,           \
                                                          int max_blocks,                  \
                                                          hipStream_t stream);

#define FUNC_FUNC_3BUF_SIG(name, type_name, type)                                          \
    void ompi_op_rocm_3buff_##name##_##type_name##_submit(const type *in1,                 \
                                                          const type *in2,                 \
                                                          type *inout,                     \
                                                          int count,                       \
                                                          int threads_per_block,           \
                                                          int max_blocks,                  \
                                                          hipStream_t stream);

#define LOC_FUNC_3BUF_SIG(name, type_name, op) \
    void ompi_op_rocm_3buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *a1, \
                                                          const ompi_op_predefined_##type_name##_t *a2, \
                                                          ompi_op_predefined_##type_name##_t *b,        \
                                                          int count,                                    \
                                                          int threads_per_block,                        \
                                                          int max_blocks,                  \
                                                          hipStream_t stream);


/*************************************************************************
 * Max
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(max,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(max,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(max,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(max, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(max,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(max, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(max,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(max, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(max,  long,  long)
FUNC_FUNC_3BUF_SIG(max,  ulong, unsigned long)

/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF_SIG(max, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF_SIG(max, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF_SIG(max, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF_SIG(max, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF_SIG(max, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF_SIG(max, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_3BUF_SIG(max, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_3BUF_SIG(max, short_float, opal_short_float_t)
#endif
FUNC_FUNC_3BUF_SIG(max, float, float)
FUNC_FUNC_3BUF_SIG(max, double, double)
FUNC_FUNC_3BUF_SIG(max, long_double, long double)
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC_3BUF_SIG(max, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC_3BUF_SIG(max, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC_3BUF_SIG(max, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC_3BUF_SIG(max, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC_3BUF_SIG(max, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
FUNC_FUNC_3BUF_SIG(max, fortran_real16, ompi_fortran_real16_t)
#endif


/*************************************************************************
 * Min
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(min,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(min,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(min,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(min, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(min,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(min, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(min,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(min, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(min,  long,  long)
FUNC_FUNC_3BUF_SIG(min,  ulong, unsigned long)

/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF_SIG(min, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF_SIG(min, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF_SIG(min, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF_SIG(min, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF_SIG(min, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF_SIG(min, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_3BUF_SIG(min, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_3BUF_SIG(min, short_float, opal_short_float_t)
#endif
FUNC_FUNC_3BUF_SIG(min, float, float)
FUNC_FUNC_3BUF_SIG(min, double, double)
FUNC_FUNC_3BUF_SIG(min, long_double, long double)
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC_3BUF_SIG(min, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC_3BUF_SIG(min, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC_3BUF_SIG(min, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC_3BUF_SIG(min, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC_3BUF_SIG(min, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
FUNC_FUNC_3BUF_SIG(min, fortran_real16, ompi_fortran_real16_t)
#endif

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF_SIG(sum,   int8_t,   int8_t, +)
OP_FUNC_3BUF_SIG(sum,  uint8_t,  uint8_t, +)
OP_FUNC_3BUF_SIG(sum,  int16_t,  int16_t, +)
OP_FUNC_3BUF_SIG(sum, uint16_t, uint16_t, +)
OP_FUNC_3BUF_SIG(sum,  int32_t,  int32_t, +)
OP_FUNC_3BUF_SIG(sum, uint32_t, uint32_t, +)
OP_FUNC_3BUF_SIG(sum,  int64_t,  int64_t, +)
OP_FUNC_3BUF_SIG(sum, uint64_t, uint64_t, +)
OP_FUNC_3BUF_SIG(sum,  long,  long, +)
OP_FUNC_3BUF_SIG(sum,  ulong, unsigned long, +)

/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC_3BUF_SIG(sum, fortran_integer, ompi_fortran_integer_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC_3BUF_SIG(sum, fortran_integer1, ompi_fortran_integer1_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC_3BUF_SIG(sum, fortran_integer2, ompi_fortran_integer2_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC_3BUF_SIG(sum, fortran_integer4, ompi_fortran_integer4_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC_3BUF_SIG(sum, fortran_integer8, ompi_fortran_integer8_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC_3BUF_SIG(sum, fortran_integer16, ompi_fortran_integer16_t, +)
#endif
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_3BUF_SIG(sum, short_float, short float, +)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_3BUF_SIG(sum, short_float, opal_short_float_t, +)
#endif
OP_FUNC_3BUF_SIG(sum, float, float, +)
OP_FUNC_3BUF_SIG(sum, double, double, +)
OP_FUNC_3BUF_SIG(sum, long_double, long double, +)
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC_3BUF_SIG(sum, fortran_real, ompi_fortran_real_t, +)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC_3BUF_SIG(sum, fortran_double_precision, ompi_fortran_double_precision_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC_3BUF_SIG(sum, fortran_real2, ompi_fortran_real2_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC_3BUF_SIG(sum, fortran_real4, ompi_fortran_real4_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC_3BUF_SIG(sum, fortran_real8, ompi_fortran_real8_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
OP_FUNC_3BUF_SIG(sum, fortran_real16, ompi_fortran_real16_t, +)
#endif
/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_3BUF_SIG(sum, c_short_float_complex, short float _Complex, +)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_SUM_FUNC_3BUF(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC_3BUF_SIG(sum, c_float_complex, float _Complex, +)
OP_FUNC_3BUF_SIG(sum, c_double_complex, double _Complex, +)
OP_FUNC_3BUF_SIG(sum, c_long_double_complex, long double _Complex, +)
#endif // 0

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF_SIG(prod,   int8_t,   int8_t, *)
OP_FUNC_3BUF_SIG(prod,  uint8_t,  uint8_t, *)
OP_FUNC_3BUF_SIG(prod,  int16_t,  int16_t, *)
OP_FUNC_3BUF_SIG(prod, uint16_t, uint16_t, *)
OP_FUNC_3BUF_SIG(prod,  int32_t,  int32_t, *)
OP_FUNC_3BUF_SIG(prod, uint32_t, uint32_t, *)
OP_FUNC_3BUF_SIG(prod,  int64_t,  int64_t, *)
OP_FUNC_3BUF_SIG(prod, uint64_t, uint64_t, *)
OP_FUNC_3BUF_SIG(prod,  long,  long, *)
OP_FUNC_3BUF_SIG(prod,  ulong, unsigned long, *)

/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC_3BUF_SIG(prod, fortran_integer, ompi_fortran_integer_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC_3BUF_SIG(prod, fortran_integer1, ompi_fortran_integer1_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC_3BUF_SIG(prod, fortran_integer2, ompi_fortran_integer2_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC_3BUF_SIG(prod, fortran_integer4, ompi_fortran_integer4_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC_3BUF_SIG(prod, fortran_integer8, ompi_fortran_integer8_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC_3BUF_SIG(prod, fortran_integer16, ompi_fortran_integer16_t, *)
#endif
/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_3BUF_SIG(prod, short_float, short float, *)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_3BUF_SIG(prod, short_float, opal_short_float_t, *)
#endif
OP_FUNC_3BUF_SIG(prod, float, float, *)
OP_FUNC_3BUF_SIG(prod, double, double, *)
OP_FUNC_3BUF_SIG(prod, long_double, long double, *)
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC_3BUF_SIG(prod, fortran_real, ompi_fortran_real_t, *)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC_3BUF_SIG(prod, fortran_double_precision, ompi_fortran_double_precision_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC_3BUF_SIG(prod, fortran_real2, ompi_fortran_real2_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC_3BUF_SIG(prod, fortran_real4, ompi_fortran_real4_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC_3BUF_SIG(prod, fortran_real8, ompi_fortran_real8_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
OP_FUNC_3BUF_SIG(prod, fortran_real16, ompi_fortran_real16_t, *)
#endif
/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_3BUF_SIG(prod, c_short_float_complex, short float _Complex, *)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_PROD_FUNC_3BUF(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC_3BUF_SIG(prod, c_float_complex, float _Complex, *)
OP_FUNC_3BUF_SIG(prod, c_double_complex, double _Complex, *)
OP_FUNC_3BUF_SIG(prod, c_long_double_complex, long double _Complex, *)
#endif // 0

/*************************************************************************
 * Logical AND
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(land,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(land,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(land,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(land, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(land,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(land, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(land,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(land, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(land,  long,  long)
FUNC_FUNC_3BUF_SIG(land,  ulong, unsigned long)

/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF_SIG(land, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF_SIG(land, bool, bool)

/*************************************************************************
 * Logical OR
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(lor,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(lor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(lor,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(lor, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(lor,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(lor, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(lor,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(lor, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(lor,  long,  long)
FUNC_FUNC_3BUF_SIG(lor,  ulong, unsigned long)

/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF_SIG(lor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF_SIG(lor, bool, bool)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(lxor,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(lxor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(lxor,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(lxor, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(lxor,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(lxor, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(lxor,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(lxor, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(lxor,  long,  long)
FUNC_FUNC_3BUF_SIG(lxor,  ulong, unsigned long)

/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF_SIG(lxor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF_SIG(lxor, bool, bool)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(band,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(band,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(band,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(band, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(band,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(band, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(band,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(band, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(band,  long,  long)
FUNC_FUNC_3BUF_SIG(band,  ulong, unsigned long)

/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF_SIG(band, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF_SIG(band, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF_SIG(band, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF_SIG(band, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF_SIG(band, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF_SIG(band, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF_SIG(band, byte, char)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(bor,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(bor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(bor,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(bor, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(bor,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(bor, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(bor,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(bor, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(bor,  long,  long)
FUNC_FUNC_3BUF_SIG(bor,  ulong, unsigned long)

/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF_SIG(bor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF_SIG(bor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF_SIG(bor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF_SIG(bor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF_SIG(bor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF_SIG(bor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF_SIG(bor, byte, char)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

/* C integer */
FUNC_FUNC_3BUF_SIG(bxor,   int8_t,   int8_t)
FUNC_FUNC_3BUF_SIG(bxor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF_SIG(bxor,  int16_t,  int16_t)
FUNC_FUNC_3BUF_SIG(bxor, uint16_t, uint16_t)
FUNC_FUNC_3BUF_SIG(bxor,  int32_t,  int32_t)
FUNC_FUNC_3BUF_SIG(bxor, uint32_t, uint32_t)
FUNC_FUNC_3BUF_SIG(bxor,  int64_t,  int64_t)
FUNC_FUNC_3BUF_SIG(bxor, uint64_t, uint64_t)
FUNC_FUNC_3BUF_SIG(bxor,  long,  long)
FUNC_FUNC_3BUF_SIG(bxor,  ulong, unsigned long)

/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF_SIG(bxor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF_SIG(bxor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF_SIG(bxor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF_SIG(bxor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF_SIG(bxor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF_SIG(bxor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF_SIG(bxor, byte, char)

/*************************************************************************
 * Max location
 *************************************************************************/

#if 0
#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC_3BUF_SIG(maxloc, 2real, >)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC_3BUF_SIG(maxloc, 2double_precision, >)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC_3BUF_SIG(maxloc, 2integer, >)
#endif
#endif // 0
LOC_FUNC_3BUF_SIG(maxloc, float_int, >)
LOC_FUNC_3BUF_SIG(maxloc, double_int, >)
LOC_FUNC_3BUF_SIG(maxloc, long_int, >)
LOC_FUNC_3BUF_SIG(maxloc, 2int, >)
LOC_FUNC_3BUF_SIG(maxloc, short_int, >)
LOC_FUNC_3BUF_SIG(maxloc, long_double_int, >)

/*************************************************************************
 * Min location
 *************************************************************************/

#if 0
#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC_3BUF_SIG(minloc, 2real, <)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC_3BUF_SIG(minloc, 2double_precision, <)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC_3BUF_SIG(minloc, 2integer, <)
#endif
#endif // 0
LOC_FUNC_3BUF_SIG(minloc, float_int, <)
LOC_FUNC_3BUF_SIG(minloc, double_int, <)
LOC_FUNC_3BUF_SIG(minloc, long_int, <)
LOC_FUNC_3BUF_SIG(minloc, 2int, <)
LOC_FUNC_3BUF_SIG(minloc, short_int, <)
LOC_FUNC_3BUF_SIG(minloc, long_double_int, <)

END_C_DECLS

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

#include <cuda.h>
#include <cuComplex.h>
#include <cuda_fp16.h>

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

#define OP_FUNC_SIG(name, type_name, type)                                  \
    void ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,   \
                                                   type *inout,             \
                                                   int count,               \
                                                   int threads_per_block,   \
                                                   int max_blocks,          \
                                                   CUstream stream);

#define FUNC_FUNC_SIG(name, type_name, type)                                \
    void ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,   \
                                                   type *inout,             \
                                                   int count,               \
                                                   int threads_per_block,   \
                                                   int max_blocks,          \
                                                   CUstream stream);

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */
#define LOC_STRUCT(type_name, type1, type2) \
  typedef struct {                          \
      type1 v;                              \
      type2 k;                              \
  } ompi_op_predefined_##type_name##_t;

#define LOC_FUNC_SIG(name, type_name)                                                                   \
    void ompi_op_cuda_2buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *a,  \
                                            ompi_op_predefined_##type_name##_t *b,                      \
                                            int count,                                                  \
                                            int threads_per_block,                                      \
                                            int max_blocks,                                             \
                                            CUstream stream);

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

/* Floating point */
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

/* Floating point */
FUNC_FUNC_SIG(min, float, float)
FUNC_FUNC_SIG(min, double, double)
FUNC_FUNC_SIG(min, long_double, long double)

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC_SIG(sum,   int8_t,   int8_t)
OP_FUNC_SIG(sum,  uint8_t,  uint8_t)
OP_FUNC_SIG(sum,  int16_t,  int16_t)
OP_FUNC_SIG(sum, uint16_t, uint16_t)
OP_FUNC_SIG(sum,  int32_t,  int32_t)
OP_FUNC_SIG(sum, uint32_t, uint32_t)
OP_FUNC_SIG(sum,  int64_t,  int64_t)
OP_FUNC_SIG(sum, uint64_t, uint64_t)
OP_FUNC_SIG(sum,  long,  long)
OP_FUNC_SIG(sum,  ulong, unsigned long)


/* Floating point */
OP_FUNC_SIG(sum, float, float)
OP_FUNC_SIG(sum, double, double)
OP_FUNC_SIG(sum, long_double, long double)

/* Complex */
FUNC_FUNC_SIG(sum, c_float_complex, cuFloatComplex)
FUNC_FUNC_SIG(sum, c_double_complex, cuDoubleComplex)

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC_SIG(prod,   int8_t,   int8_t)
OP_FUNC_SIG(prod,  uint8_t,  uint8_t)
OP_FUNC_SIG(prod,  int16_t,  int16_t)
OP_FUNC_SIG(prod, uint16_t, uint16_t)
OP_FUNC_SIG(prod,  int32_t,  int32_t)
OP_FUNC_SIG(prod, uint32_t, uint32_t)
OP_FUNC_SIG(prod,  int64_t,  int64_t)
OP_FUNC_SIG(prod, uint64_t, uint64_t)
OP_FUNC_SIG(prod,  long,  long)
OP_FUNC_SIG(prod,  ulong, unsigned long)

/* Floating point */
OP_FUNC_SIG(prod, float, float)
OP_FUNC_SIG(prod, double, double)
OP_FUNC_SIG(prod, long_double, long double)

FUNC_FUNC_SIG(prod, c_float_complex, cuFloatComplex)
FUNC_FUNC_SIG(prod, c_double_complex, cuDoubleComplex)

/*************************************************************************
 * Logical AND
 *************************************************************************/

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
LOC_STRUCT(2float, float, float)
LOC_STRUCT(2double, double, double)
LOC_STRUCT(2int8, int8_t, int8_t)
LOC_STRUCT(2int16, int16_t, int16_t)
LOC_STRUCT(2int32, int32_t, int32_t)
LOC_STRUCT(2int64, int64_t, int64_t)

/*************************************************************************
 * Max location
 *************************************************************************/

LOC_FUNC_SIG(maxloc, 2float)
LOC_FUNC_SIG(maxloc, 2double)
LOC_FUNC_SIG(maxloc, 2int8)
LOC_FUNC_SIG(maxloc, 2int16)
LOC_FUNC_SIG(maxloc, 2int32)
LOC_FUNC_SIG(maxloc, 2int64)

LOC_FUNC_SIG(maxloc, float_int)
LOC_FUNC_SIG(maxloc, double_int)
LOC_FUNC_SIG(maxloc, long_int)
LOC_FUNC_SIG(maxloc, 2int)
LOC_FUNC_SIG(maxloc, short_int)
LOC_FUNC_SIG(maxloc, long_double_int)

/*************************************************************************
 * Min location
 *************************************************************************/

LOC_FUNC_SIG(minloc, 2float)
LOC_FUNC_SIG(minloc, 2double)
LOC_FUNC_SIG(minloc, 2int8)
LOC_FUNC_SIG(minloc, 2int16)
LOC_FUNC_SIG(minloc, 2int32)
LOC_FUNC_SIG(minloc, 2int64)

LOC_FUNC_SIG(minloc, float_int)
LOC_FUNC_SIG(minloc, double_int)
LOC_FUNC_SIG(minloc, long_int)
LOC_FUNC_SIG(minloc, 2int)
LOC_FUNC_SIG(minloc, short_int)
LOC_FUNC_SIG(minloc, long_double_int)



#define OP_FUNC_3BUF_SIG(name, type_name, type)                                             \
    void ompi_op_cuda_3buff_##name##_##type_name##_submit(const type *in1,                  \
                                                          const type *in2,                  \
                                                          type *inout,                      \
                                                          int count,                        \
                                                          int threads_per_block,            \
                                                          int max_blocks,                   \
                                                          CUstream stream);

#define FUNC_FUNC_3BUF_SIG(name, type_name, type)                                           \
    void ompi_op_cuda_3buff_##name##_##type_name##_submit(const type *in1,                  \
                                                          const type *in2,                  \
                                                          type *inout,                      \
                                                          int count,                        \
                                                          int threads_per_block,            \
                                                          int max_blocks,                   \
                                                          CUstream stream);

#define LOC_FUNC_3BUF_SIG(name, type_name)                                                              \
    void ompi_op_cuda_3buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *a1, \
                                                          const ompi_op_predefined_##type_name##_t *a2, \
                                                          ompi_op_predefined_##type_name##_t *b,        \
                                                          int count,                                    \
                                                          int threads_per_block,                        \
                                                          int max_blocks,                               \
                                                          CUstream stream);


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

/* Floating point */
FUNC_FUNC_3BUF_SIG(max, float, float)
FUNC_FUNC_3BUF_SIG(max, double, double)
FUNC_FUNC_3BUF_SIG(max, long_double, long double)

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

/* Floating point */
FUNC_FUNC_3BUF_SIG(min, float, float)
FUNC_FUNC_3BUF_SIG(min, double, double)
FUNC_FUNC_3BUF_SIG(min, long_double, long double)

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF_SIG(sum,   int8_t,   int8_t)
OP_FUNC_3BUF_SIG(sum,  uint8_t,  uint8_t)
OP_FUNC_3BUF_SIG(sum,  int16_t,  int16_t)
OP_FUNC_3BUF_SIG(sum, uint16_t, uint16_t)
OP_FUNC_3BUF_SIG(sum,  int32_t,  int32_t)
OP_FUNC_3BUF_SIG(sum, uint32_t, uint32_t)
OP_FUNC_3BUF_SIG(sum,  int64_t,  int64_t)
OP_FUNC_3BUF_SIG(sum, uint64_t, uint64_t)
OP_FUNC_3BUF_SIG(sum,  long,  long)
OP_FUNC_3BUF_SIG(sum,  ulong, unsigned long)

/* Floating point */
OP_FUNC_3BUF_SIG(sum, float, float)
OP_FUNC_3BUF_SIG(sum, double, double)
OP_FUNC_3BUF_SIG(sum, long_double, long double)

/* Complex */
FUNC_FUNC_3BUF_SIG(sum, c_float_complex, cuFloatComplex)
FUNC_FUNC_3BUF_SIG(sum, c_double_complex, cuDoubleComplex)

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF_SIG(prod,   int8_t,   int8_t)
OP_FUNC_3BUF_SIG(prod,  uint8_t,  uint8_t)
OP_FUNC_3BUF_SIG(prod,  int16_t,  int16_t)
OP_FUNC_3BUF_SIG(prod, uint16_t, uint16_t)
OP_FUNC_3BUF_SIG(prod,  int32_t,  int32_t)
OP_FUNC_3BUF_SIG(prod, uint32_t, uint32_t)
OP_FUNC_3BUF_SIG(prod,  int64_t,  int64_t)
OP_FUNC_3BUF_SIG(prod, uint64_t, uint64_t)
OP_FUNC_3BUF_SIG(prod,  long,  long)
OP_FUNC_3BUF_SIG(prod,  ulong, unsigned long)

/* Floating point */
OP_FUNC_3BUF_SIG(prod, float, float)
OP_FUNC_3BUF_SIG(prod, double, double)
OP_FUNC_3BUF_SIG(prod, long_double, long double)

/* Complex */
FUNC_FUNC_3BUF_SIG(prod, c_float_complex, cuFloatComplex)
FUNC_FUNC_3BUF_SIG(prod, c_double_complex, cuDoubleComplex)

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

/* Byte */
FUNC_FUNC_3BUF_SIG(bxor, byte, char)

/*************************************************************************
 * Max location
 *************************************************************************/

LOC_FUNC_3BUF_SIG(maxloc, float_int)
LOC_FUNC_3BUF_SIG(maxloc, double_int)
LOC_FUNC_3BUF_SIG(maxloc, long_int)
LOC_FUNC_3BUF_SIG(maxloc, 2int)
LOC_FUNC_3BUF_SIG(maxloc, short_int)
LOC_FUNC_3BUF_SIG(maxloc, long_double_int)

LOC_FUNC_3BUF_SIG(maxloc, 2float)
LOC_FUNC_3BUF_SIG(maxloc, 2double)
LOC_FUNC_3BUF_SIG(maxloc, 2int8)
LOC_FUNC_3BUF_SIG(maxloc, 2int16)
LOC_FUNC_3BUF_SIG(maxloc, 2int32)
LOC_FUNC_3BUF_SIG(maxloc, 2int64)

/*************************************************************************
 * Min location
 *************************************************************************/

LOC_FUNC_3BUF_SIG(minloc, float_int)
LOC_FUNC_3BUF_SIG(minloc, double_int)
LOC_FUNC_3BUF_SIG(minloc, long_int)
LOC_FUNC_3BUF_SIG(minloc, 2int)
LOC_FUNC_3BUF_SIG(minloc, short_int)
LOC_FUNC_3BUF_SIG(minloc, long_double_int)

LOC_FUNC_3BUF_SIG(minloc, 2float)
LOC_FUNC_3BUF_SIG(minloc, 2double)
LOC_FUNC_3BUF_SIG(minloc, 2int8)
LOC_FUNC_3BUF_SIG(minloc, 2int16)
LOC_FUNC_3BUF_SIG(minloc, 2int32)
LOC_FUNC_3BUF_SIG(minloc, 2int64)


END_C_DECLS

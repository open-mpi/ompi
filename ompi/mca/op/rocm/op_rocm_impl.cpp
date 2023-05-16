#include "hip/hip_runtime.h"
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

#include <hip/hip_complex.h>

#include "op_rocm_impl.h"

/* TODO: missing support for
 * - short float (conditional on whether short float is available)
 * - complex
 * - 3buff implementation
 */

#define THREADS_PER_BLOCK 512

#define OP_FUNC(name, type_name, type, op)                                                          \
    static __global__ void                                                                          \
    ompi_op_rocm_2buff_##name##_##type_name##_kernel(const type *in, type *inout, int n) {          \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            inout[i] = inout[i] op in[i];                                                           \
        }                                                                                           \
    }                                                                                               \
    void ompi_op_rocm_2buff_##name##_##type_name##_submit(const type *in,                           \
                                                   type *inout,                                     \
                                                   int count,                                       \
                                                   int threads_per_block,                           \
                                                   hipStream_t stream) {                               \
        int threads = threads_per_block;                                                            \
        int blocks  = (count + threads-1) / threads;                                                \
        int n = count;                                                                              \
        hipStream_t s = stream;                                                                        \
        hipLaunchKernelGGL(ompi_op_rocm_2buff_##name##_##type_name##_kernel, \
                dim3(blocks), dim3(threads), 0, s,\
                in, inout, n);  \
    }


#define FUNC_FUNC(name, type_name, type)                                                            \
    static __global__ void                                                                          \
    ompi_op_rocm_2buff_##name##_##type_name##_kernel(const type *in, type *inout, int n) {          \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            inout[i] = current_func(inout[i], in[i]);                                               \
        }                                                                                           \
    }                                                                                               \
    void                                                                                            \
    ompi_op_rocm_2buff_##name##_##type_name##_submit(const type *in,                                \
                                              type *inout,                                          \
                                              int count,                                            \
                                              int threads_per_block,                                \
                                              hipStream_t stream) {                                    \
        int threads = threads_per_block;                                                            \
        int blocks  = (count + threads-1) / threads;                                                \
        int n = count;                                                                              \
        hipStream_t s = stream;                                                                        \
        hipLaunchKernelGGL(ompi_op_rocm_2buff_##name##_##type_name##_kernel, \
                dim3(blocks), dim3(threads), 0, s, \
                in, inout, n);  \
    }

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */

#define LOC_FUNC(name, type_name, op)                                                               \
    static __global__ void                                                                          \
    ompi_op_rocm_2buff_##name##_##type_name##_kernel(const ompi_op_predefined_##type_name##_t *in,  \
                                                     ompi_op_predefined_##type_name##_t *inout,     \
                                                     int n)                                         \
    {                                                                                               \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            const ompi_op_predefined_##type_name##_t *a = &in[i];                                   \
            ompi_op_predefined_##type_name##_t *b = &inout[i];                                      \
            if (a->v op b->v) {                                                                     \
                b->v = a->v;                                                                        \
                b->k = a->k;                                                                        \
            } else if (a->v == b->v) {                                                              \
                b->k = (b->k < a->k ? b->k : a->k);                                                 \
            }                                                                                       \
        }                                                                                           \
    }                                                                                               \
    void                                                                                            \
    ompi_op_rocm_2buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *a,   \
                                            ompi_op_predefined_##type_name##_t *b,                  \
                                            int count,                                              \
                                            int threads_per_block,                                  \
                                            hipStream_t stream) {                                      \
        int threads = threads_per_block;                                                            \
        int blocks  = (count + threads-1) / threads;                                                \
        hipStream_t s = stream;                                                                        \
        hipLaunchKernelGGL(ompi_op_rocm_2buff_##name##_##type_name##_kernel, \
                dim3(blocks), dim3(threads), 0, s, \
                a, b, count);   \
    }

/*************************************************************************
 * Max
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
/* C integer */
FUNC_FUNC(max,   int8_t,   int8_t)
FUNC_FUNC(max,  uint8_t,  uint8_t)
FUNC_FUNC(max,  int16_t,  int16_t)
FUNC_FUNC(max, uint16_t, uint16_t)
FUNC_FUNC(max,  int32_t,  int32_t)
FUNC_FUNC(max, uint32_t, uint32_t)
FUNC_FUNC(max,  int64_t,  int64_t)
FUNC_FUNC(max, uint64_t, uint64_t)
FUNC_FUNC(max,  long,  long)
FUNC_FUNC(max,  unsigned_long, unsigned long)

FUNC_FUNC(max, float, float)
FUNC_FUNC(max, double, double)
FUNC_FUNC(max, long_double, long double)

/*************************************************************************
 * Min
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
/* C integer */
FUNC_FUNC(min,   int8_t,   int8_t)
FUNC_FUNC(min,  uint8_t,  uint8_t)
FUNC_FUNC(min,  int16_t,  int16_t)
FUNC_FUNC(min, uint16_t, uint16_t)
FUNC_FUNC(min,  int32_t,  int32_t)
FUNC_FUNC(min, uint32_t, uint32_t)
FUNC_FUNC(min,  int64_t,  int64_t)
FUNC_FUNC(min, uint64_t, uint64_t)
FUNC_FUNC(min,  long,  long)
FUNC_FUNC(min,  unsigned_long, unsigned long)


FUNC_FUNC(min, float, float)
FUNC_FUNC(min, double, double)
FUNC_FUNC(min, long_double, long double)

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC(sum,   int8_t,   int8_t, +=)
OP_FUNC(sum,  uint8_t,  uint8_t, +=)
OP_FUNC(sum,  int16_t,  int16_t, +=)
OP_FUNC(sum, uint16_t, uint16_t, +=)
OP_FUNC(sum,  int32_t,  int32_t, +=)
OP_FUNC(sum, uint32_t, uint32_t, +=)
OP_FUNC(sum,  int64_t,  int64_t, +=)
OP_FUNC(sum, uint64_t, uint64_t, +=)
OP_FUNC(sum,  long,  long, +=)
OP_FUNC(sum,  unsigned_long, unsigned long, +=)

OP_FUNC(sum, float, float, +=)
OP_FUNC(sum, double, double, +=)
OP_FUNC(sum, long_double, long double, +=)

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC(sum, c_short_float_complex, short float _Complex, +=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_SUM_FUNC(c_short_float_complex, opal_short_float_t)
#endif
#endif // 0
#undef current_func
#define current_func(a, b) (hipCmulf(a,b))
FUNC_FUNC(sum, c_float_complex, hipFloatComplex)
#undef current_func
#define current_func(a, b) (hipCmul(a,b))
FUNC_FUNC(sum, c_double_complex, hipDoubleComplex)
//OP_FUNC(sum, c_long_double_complex, cuLongDoubleComplex, +=)

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC(prod,   int8_t,   int8_t, *=)
OP_FUNC(prod,  uint8_t,  uint8_t, *=)
OP_FUNC(prod,  int16_t,  int16_t, *=)
OP_FUNC(prod, uint16_t, uint16_t, *=)
OP_FUNC(prod,  int32_t,  int32_t, *=)
OP_FUNC(prod, uint32_t, uint32_t, *=)
OP_FUNC(prod,  int64_t,  int64_t, *=)
OP_FUNC(prod, uint64_t, uint64_t, *=)
OP_FUNC(prod,  long,  long, *=)
OP_FUNC(prod,  unsigned_long, unsigned long, *=)

OP_FUNC(prod, float, float, *=)
OP_FUNC(prod, double, double, *=)
OP_FUNC(prod, long_double, long double, *=)

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC(prod, c_short_float_complex, short float _Complex, *=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_PROD_FUNC(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC(prod, c_float_complex, float _Complex, *=)
OP_FUNC(prod, c_double_complex, double _Complex, *=)
OP_FUNC(prod, c_long_double_complex, long double _Complex, *=)
#endif // 0

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC(land,   int8_t,   int8_t)
FUNC_FUNC(land,  uint8_t,  uint8_t)
FUNC_FUNC(land,  int16_t,  int16_t)
FUNC_FUNC(land, uint16_t, uint16_t)
FUNC_FUNC(land,  int32_t,  int32_t)
FUNC_FUNC(land, uint32_t, uint32_t)
FUNC_FUNC(land,  int64_t,  int64_t)
FUNC_FUNC(land, uint64_t, uint64_t)
FUNC_FUNC(land,  long,  long)
FUNC_FUNC(land,  unsigned_long, unsigned long)

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
FUNC_FUNC(lor,   int8_t,   int8_t)
FUNC_FUNC(lor,  uint8_t,  uint8_t)
FUNC_FUNC(lor,  int16_t,  int16_t)
FUNC_FUNC(lor, uint16_t, uint16_t)
FUNC_FUNC(lor,  int32_t,  int32_t)
FUNC_FUNC(lor, uint32_t, uint32_t)
FUNC_FUNC(lor,  int64_t,  int64_t)
FUNC_FUNC(lor, uint64_t, uint64_t)
FUNC_FUNC(lor,  long,  long)
FUNC_FUNC(lor,  unsigned_long, unsigned long)

/* C++ bool */
FUNC_FUNC(lor, bool, bool)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a ? 1 : 0) ^ (b ? 1: 0))
/* C integer */
FUNC_FUNC(lxor,   int8_t,   int8_t)
FUNC_FUNC(lxor,  uint8_t,  uint8_t)
FUNC_FUNC(lxor,  int16_t,  int16_t)
FUNC_FUNC(lxor, uint16_t, uint16_t)
FUNC_FUNC(lxor,  int32_t,  int32_t)
FUNC_FUNC(lxor, uint32_t, uint32_t)
FUNC_FUNC(lxor,  int64_t,  int64_t)
FUNC_FUNC(lxor, uint64_t, uint64_t)
FUNC_FUNC(lxor,  long,  long)
FUNC_FUNC(lxor,  unsigned_long, unsigned long)

/* C++ bool */
FUNC_FUNC(lxor, bool, bool)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) & (b))
/* C integer */
FUNC_FUNC(band,   int8_t,   int8_t)
FUNC_FUNC(band,  uint8_t,  uint8_t)
FUNC_FUNC(band,  int16_t,  int16_t)
FUNC_FUNC(band, uint16_t, uint16_t)
FUNC_FUNC(band,  int32_t,  int32_t)
FUNC_FUNC(band, uint32_t, uint32_t)
FUNC_FUNC(band,  int64_t,  int64_t)
FUNC_FUNC(band, uint64_t, uint64_t)
FUNC_FUNC(band,  long,  long)
FUNC_FUNC(band,  unsigned_long, unsigned long)

/* Byte */
FUNC_FUNC(band, byte, char)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) | (b))
/* C integer */
FUNC_FUNC(bor,   int8_t,   int8_t)
FUNC_FUNC(bor,  uint8_t,  uint8_t)
FUNC_FUNC(bor,  int16_t,  int16_t)
FUNC_FUNC(bor, uint16_t, uint16_t)
FUNC_FUNC(bor,  int32_t,  int32_t)
FUNC_FUNC(bor, uint32_t, uint32_t)
FUNC_FUNC(bor,  int64_t,  int64_t)
FUNC_FUNC(bor, uint64_t, uint64_t)
FUNC_FUNC(bor,  long,  long)
FUNC_FUNC(bor,  unsigned_long, unsigned long)

/* Byte */
FUNC_FUNC(bor, byte, char)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) ^ (b))
/* C integer */
FUNC_FUNC(bxor,   int8_t,   int8_t)
FUNC_FUNC(bxor,  uint8_t,  uint8_t)
FUNC_FUNC(bxor,  int16_t,  int16_t)
FUNC_FUNC(bxor, uint16_t, uint16_t)
FUNC_FUNC(bxor,  int32_t,  int32_t)
FUNC_FUNC(bxor, uint32_t, uint32_t)
FUNC_FUNC(bxor,  int64_t,  int64_t)
FUNC_FUNC(bxor, uint64_t, uint64_t)
FUNC_FUNC(bxor,  long,  long)
FUNC_FUNC(bxor,  unsigned_long, unsigned long)

/* Byte */
FUNC_FUNC(bxor, byte, char)

/*************************************************************************
 * Max location
 *************************************************************************/

LOC_FUNC(maxloc, float_int, >)
LOC_FUNC(maxloc, double_int, >)
LOC_FUNC(maxloc, long_int, >)
LOC_FUNC(maxloc, 2int, >)
LOC_FUNC(maxloc, short_int, >)
LOC_FUNC(maxloc, long_double_int, >)

/*************************************************************************
 * Min location
 *************************************************************************/

LOC_FUNC(minloc, float_int, <)
LOC_FUNC(minloc, double_int, <)
LOC_FUNC(minloc, long_int, <)
LOC_FUNC(minloc, 2int, <)
LOC_FUNC(minloc, short_int, <)
LOC_FUNC(minloc, long_double_int, <)


/*
 *  This is a three buffer (2 input and 1 output) version of the reduction
 *    routines, needed for some optimizations.
 */
#define OP_FUNC_3BUF(name, type_name, type, op)                                                     \
    static __global__ void                                                                          \
    ompi_op_rocm_3buff_##name##_##type_name##_kernel(const type *in1, const type* in2,              \
                                                     type *out, int n) {                            \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            out[i] = in1[i] op in2[i];                                                              \
        }                                                                                           \
    }                                                                                               \
    void ompi_op_rocm_3buff_##name##_##type_name##_submit(const type *in1, const type *in2,         \
                                                          type *out, int count,                     \
                                                          int threads_per_block,                    \
                                                          hipStream_t stream) {                        \
        int threads = threads_per_block;                                                            \
        int blocks  = (count+threads-1) / threads;                                                  \
        hipLaunchKernelGGL(ompi_op_rocm_3buff_##name##_##type_name##_kernel, \
                dim3(blocks), dim3(threads), 0, stream, \
                in1, in2, out, count);      \
    }


/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out = op(in1, in2))
 */
#define FUNC_FUNC_3BUF(name, type_name, type)                                                       \
    static __global__ void                                                                          \
    ompi_op_rocm_3buff_##name##_##type_name##_kernel(const type *in1, const type *in2,              \
                                                     type *out, int n) {                            \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            out[i] = current_func(in1[i], in2[i]);                                                  \
        }                                                                                           \
    }                                                                                               \
    void                                                                                            \
    ompi_op_rocm_3buff_##name##_##type_name##_submit(const type *in1, const type *in2,              \
                                                     type *out, int count,                          \
                                                     int threads_per_block,                         \
                                                     hipStream_t stream) {                             \
        int threads = threads_per_block;                                                            \
        int blocks  = (count+threads-1) / threads;                                                  \
        hipLaunchKernelGGL(ompi_op_rocm_3buff_##name##_##type_name##_kernel, \
                dim3(blocks), dim3(threads), 0, stream,  \
                in1, in2, out, count);      \
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

#define LOC_FUNC_3BUF(name, type_name, op)                                                          \
    static __global__ void                                                                          \
    ompi_op_rocm_3buff_##name##_##type_name##_kernel(const ompi_op_predefined_##type_name##_t *in1, \
                                                     const ompi_op_predefined_##type_name##_t *in2, \
                                                     ompi_op_predefined_##type_name##_t *out,       \
                                                     int n)                                         \
    {                                                                                               \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            const ompi_op_predefined_##type_name##_t *a1 = &in1[i];                                 \
            const ompi_op_predefined_##type_name##_t *a2 = &in2[i];                                 \
            ompi_op_predefined_##type_name##_t *b = &out[i];                                        \
            if (a1->v op a2->v) {                                                                   \
                b->v = a1->v;                                                                       \
                b->k = a1->k;                                                                       \
            } else if (a1->v == a2->v) {                                                            \
                b->v = a1->v;                                                                       \
                b->k = (a2->k < a1->k ? a2->k : a1->k);                                             \
            } else {                                                                                \
                b->v = a2->v;                                                                       \
                b->k = a2->k;                                                                       \
            }                                                                                       \
        }                                                                                           \
    }                                                                                               \
    void                                                                                            \
    ompi_op_rocm_3buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *in1, \
                                                     const ompi_op_predefined_##type_name##_t *in2, \
                                                     ompi_op_predefined_##type_name##_t *out,       \
                                                     int count,                                     \
                                                     int threads_per_block,                         \
                                                     hipStream_t stream)                               \
    {                                                                                               \
        int threads = threads_per_block;                                                            \
        int blocks  = (count+threads-1) / threads;                                                  \
        hipLaunchKernelGGL(ompi_op_rocm_3buff_##name##_##type_name##_kernel,   \
                dim3(blocks), dim3(threads), 0, stream, \
                in1, in2, out, count);      \
    }


/*************************************************************************
 * Max
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
/* C integer */
FUNC_FUNC_3BUF(max,   int8_t,   int8_t)
FUNC_FUNC_3BUF(max,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(max,  int16_t,  int16_t)
FUNC_FUNC_3BUF(max, uint16_t, uint16_t)
FUNC_FUNC_3BUF(max,  int32_t,  int32_t)
FUNC_FUNC_3BUF(max, uint32_t, uint32_t)
FUNC_FUNC_3BUF(max,  int64_t,  int64_t)
FUNC_FUNC_3BUF(max, uint64_t, uint64_t)
FUNC_FUNC_3BUF(max,  long,  long)
FUNC_FUNC_3BUF(max,  unsigned_long, unsigned long)

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
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_3BUF(max, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_3BUF(max, short_float, opal_short_float_t)
#endif
FUNC_FUNC_3BUF(max, float, float)
FUNC_FUNC_3BUF(max, double, double)
FUNC_FUNC_3BUF(max, long_double, long double)
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
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
FUNC_FUNC_3BUF(max, fortran_real16, ompi_fortran_real16_t)
#endif


/*************************************************************************
 * Min
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
/* C integer */
FUNC_FUNC_3BUF(min,   int8_t,   int8_t)
FUNC_FUNC_3BUF(min,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(min,  int16_t,  int16_t)
FUNC_FUNC_3BUF(min, uint16_t, uint16_t)
FUNC_FUNC_3BUF(min,  int32_t,  int32_t)
FUNC_FUNC_3BUF(min, uint32_t, uint32_t)
FUNC_FUNC_3BUF(min,  int64_t,  int64_t)
FUNC_FUNC_3BUF(min, uint64_t, uint64_t)
FUNC_FUNC_3BUF(min,  long,  long)
FUNC_FUNC_3BUF(min,  unsigned_long, unsigned long)

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
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_3BUF(min, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_3BUF(min, short_float, opal_short_float_t)
#endif
FUNC_FUNC_3BUF(min, float, float)
FUNC_FUNC_3BUF(min, double, double)
FUNC_FUNC_3BUF(min, long_double, long double)
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
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
FUNC_FUNC_3BUF(min, fortran_real16, ompi_fortran_real16_t)
#endif

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF(sum,   int8_t,   int8_t, +)
OP_FUNC_3BUF(sum,  uint8_t,  uint8_t, +)
OP_FUNC_3BUF(sum,  int16_t,  int16_t, +)
OP_FUNC_3BUF(sum, uint16_t, uint16_t, +)
OP_FUNC_3BUF(sum,  int32_t,  int32_t, +)
OP_FUNC_3BUF(sum, uint32_t, uint32_t, +)
OP_FUNC_3BUF(sum,  int64_t,  int64_t, +)
OP_FUNC_3BUF(sum, uint64_t, uint64_t, +)
OP_FUNC_3BUF(sum,  long,  long, +)
OP_FUNC_3BUF(sum,  unsigned_long, unsigned long, +)

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
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_3BUF(sum, short_float, short float, +)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_3BUF(sum, short_float, opal_short_float_t, +)
#endif
OP_FUNC_3BUF(sum, float, float, +)
OP_FUNC_3BUF(sum, double, double, +)
OP_FUNC_3BUF(sum, long_double, long double, +)
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
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
OP_FUNC_3BUF(sum, fortran_real16, ompi_fortran_real16_t, +)
#endif
/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_3BUF(sum, c_short_float_complex, short float _Complex, +)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_SUM_FUNC_3BUF(c_short_float_complex, opal_short_float_t)
#endif
#endif // 0
#undef current_func
#define current_func(a, b) (hipCmulf(a,b))
FUNC_FUNC_3BUF(sum, c_float_complex, hipFloatComplex)
#undef current_func
#define current_func(a, b) (hipCmul(a,b))
FUNC_FUNC_3BUF(sum, c_double_complex, hipDoubleComplex)
//OP_FUNC_3BUF(sum, c_long_double_complex, cuLongDoubleComplex, +)

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF(prod,   int8_t,   int8_t, *)
OP_FUNC_3BUF(prod,  uint8_t,  uint8_t, *)
OP_FUNC_3BUF(prod,  int16_t,  int16_t, *)
OP_FUNC_3BUF(prod, uint16_t, uint16_t, *)
OP_FUNC_3BUF(prod,  int32_t,  int32_t, *)
OP_FUNC_3BUF(prod, uint32_t, uint32_t, *)
OP_FUNC_3BUF(prod,  int64_t,  int64_t, *)
OP_FUNC_3BUF(prod, uint64_t, uint64_t, *)
OP_FUNC_3BUF(prod,  long,  long, *)
OP_FUNC_3BUF(prod,  unsigned_long, unsigned long, *)

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
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_3BUF(prod, short_float, short float, *)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_3BUF(prod, short_float, opal_short_float_t, *)
#endif
OP_FUNC_3BUF(prod, float, float, *)
OP_FUNC_3BUF(prod, double, double, *)
OP_FUNC_3BUF(prod, long_double, long double, *)
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
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
OP_FUNC_3BUF(prod, fortran_real16, ompi_fortran_real16_t, *)
#endif
/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_3BUF(prod, c_short_float_complex, short float _Complex, *)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_PROD_FUNC_3BUF(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC_3BUF(prod, c_float_complex, float _Complex, *)
OP_FUNC_3BUF(prod, c_double_complex, double _Complex, *)
OP_FUNC_3BUF(prod, c_long_double_complex, long double _Complex, *)
#endif // 0

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC_3BUF(land,   int8_t,   int8_t)
FUNC_FUNC_3BUF(land,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(land,  int16_t,  int16_t)
FUNC_FUNC_3BUF(land, uint16_t, uint16_t)
FUNC_FUNC_3BUF(land,  int32_t,  int32_t)
FUNC_FUNC_3BUF(land, uint32_t, uint32_t)
FUNC_FUNC_3BUF(land,  int64_t,  int64_t)
FUNC_FUNC_3BUF(land, uint64_t, uint64_t)
FUNC_FUNC_3BUF(land,  long,  long)
FUNC_FUNC_3BUF(land,  unsigned_long, unsigned long)

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
FUNC_FUNC_3BUF(lor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(lor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(lor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(lor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(lor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(lor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(lor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(lor, uint64_t, uint64_t)
FUNC_FUNC_3BUF(lor,  long,  long)
FUNC_FUNC_3BUF(lor,  unsigned_long, unsigned long)

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
FUNC_FUNC_3BUF(lxor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(lxor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(lxor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(lxor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(lxor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(lxor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(lxor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(lxor, uint64_t, uint64_t)
FUNC_FUNC_3BUF(lxor,  long,  long)
FUNC_FUNC_3BUF(lxor,  unsigned_long, unsigned long)

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
FUNC_FUNC_3BUF(band,   int8_t,   int8_t)
FUNC_FUNC_3BUF(band,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(band,  int16_t,  int16_t)
FUNC_FUNC_3BUF(band, uint16_t, uint16_t)
FUNC_FUNC_3BUF(band,  int32_t,  int32_t)
FUNC_FUNC_3BUF(band, uint32_t, uint32_t)
FUNC_FUNC_3BUF(band,  int64_t,  int64_t)
FUNC_FUNC_3BUF(band, uint64_t, uint64_t)
FUNC_FUNC_3BUF(band,  long,  long)
FUNC_FUNC_3BUF(band,  unsigned_long, unsigned long)

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
FUNC_FUNC_3BUF(bor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(bor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(bor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(bor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(bor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(bor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(bor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(bor, uint64_t, uint64_t)
FUNC_FUNC_3BUF(bor,  long,  long)
FUNC_FUNC_3BUF(bor,  unsigned_long, unsigned long)

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
FUNC_FUNC_3BUF(bxor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(bxor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(bxor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(bxor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(bxor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(bxor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(bxor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(bxor, uint64_t, uint64_t)
FUNC_FUNC_3BUF(bxor,  long,  long)
FUNC_FUNC_3BUF(bxor,  unsigned_long, unsigned long)

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
LOC_STRUCT_3BUF(long_double_int, long double, int)
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
LOC_FUNC_3BUF(maxloc, long_double_int, >)

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
LOC_FUNC_3BUF(minloc, long_double_int, <)

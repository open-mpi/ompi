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

#include "op_cuda_impl.h"

#include <limits.h>

#include <type_traits>

#define ISSIGNED(x) std::is_signed_v<x>

template<typename T>
static inline __device__ constexpr T tmax(T a, T b) {
    return (a > b) ? a : b;
}

template<typename T>
static inline __device__ constexpr T tmin(T a, T b) {
    return (a < b) ? a : b;
}

template<typename T>
static inline __device__ constexpr T tsum(T a, T b) {
    return a+b;
}

template<typename T>
static inline __device__ constexpr T tprod(T a, T b) {
    return a*b;
}

template<typename T>
static inline __device__ T vmax(const T& a, const T& b) {
    return T{tmax(a.x, b.x), tmax(a.y, b.y), tmax(a.z, b.z), tmax(a.w, b.w)};
}

template<typename T>
static inline __device__ T vmin(const T& a, const T& b) {
    return T{tmin(a.x, b.x), tmin(a.y, b.y), tmin(a.z, b.z), tmin(a.w, b.w)};
}

template<typename T>
static inline __device__ T vsum(const T& a, const T& b) {
    return T{tsum(a.x, b.x), tsum(a.y, b.y), tsum(a.z, b.z), tsum(a.w, b.w)};
}

template<typename T>
static inline __device__ T vprod(const T& a, const T& b) {
    return T{(a.x * b.x), (a.y * b.y), (a.z * b.z), (a.w * b.w)};
}


/* TODO: missing support for
 * - short float (conditional on whether short float is available)
 * - some Fortran types
 * - some complex types
 */

#define USE_VECTORS 1

#define OP_FUNC(name, type_name, type, op)                                                          \
    static __global__ void                                                                          \
    ompi_op_cuda_2buff_##name##_##type_name##_kernel(const type *__restrict__ in,                   \
                                                     type *__restrict__ inout, int n) {             \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
        /*if (index < n) { int i = index;*/ \
            inout[i] = inout[i] op in[i];                                                           \
        }                                                                                           \
    }                                                                                               \
    void ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,                           \
                                                   type *inout,                                     \
                                                   int count,                                       \
                                                   int threads_per_block,                           \
                                                   int max_blocks,                                  \
                                                   CUstream stream) {                               \
        int threads = min(count, threads_per_block);                                                \
        int blocks  = min((count + threads-1) / threads, max_blocks);                               \
        int n = count;                                                                              \
        CUstream s = stream;                                                                        \
        ompi_op_cuda_2buff_##name##_##type_name##_kernel<<<blocks, threads, 0, s>>>(in, inout, n);  \
    }


#if defined(USE_VECTORS)
#define OPV_FUNC(name, type_name, type, vtype, vlen, op)                                            \
    static __global__ void                                                                          \
    ompi_op_cuda_2buff_##name##_##type_name##_kernel(const type *__restrict__ in,                   \
                                                     type *__restrict__ inout, int n) {             \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n/vlen; i += stride) {                                              \
            vtype vin = ((vtype*)in)[i];                                                            \
            vtype vinout = ((vtype*)inout)[i];                                                      \
            vinout.x = vinout.x op vin.x;                                                         \
            vinout.y = vinout.y op vin.y;                                                         \
            vinout.z = vinout.z op vin.z;                                                         \
            vinout.w = vinout.w op vin.w;                                                         \
            ((vtype*)inout)[i] = vinout;                                                            \
        }                                                                                           \
        int remainder = n%vlen;                                                                     \
        if (index == (n/vlen) && remainder != 0) {                                                  \
            while(remainder) {                                                                      \
                int idx = n - remainder--;                                                          \
                inout[idx] = inout[idx] op in[idx];                                                 \
            }                                                                                       \
        }                                                                                           \
    }                                                                                               \
    void ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,                           \
                                                   type *inout,                                     \
                                                   int count,                                       \
                                                   int threads_per_block,                           \
                                                   int max_blocks,                                  \
                                                   CUstream stream) {                               \
        int vcount  = (count + vlen-1)/vlen;                                                        \
        int threads = min(threads_per_block, vcount);                                               \
        int blocks  = min((vcount + threads-1) / threads, max_blocks);                              \
        int n = count;                                                                              \
        CUstream s = stream;                                                                        \
        ompi_op_cuda_2buff_##name##_##type_name##_kernel<<<blocks, threads, 0, s>>>(in, inout, n);  \
    }
#else // USE_VECTORS
#define OPV_FUNC(name, type_name, type, vtype, vlen, op) OP_FUNC(name, type_name, type, op)
#endif // USE_VECTORS

#define FUNC_FUNC_FN(name, type_name, type, fn)                                                     \
    static __global__ void                                                                          \
    ompi_op_cuda_2buff_##name##_##type_name##_kernel(const type *__restrict__ in,                   \
                                                     type *__restrict__ inout, int n) {             \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            inout[i] = fn(inout[i], in[i]);                                                         \
        }                                                                                           \
    }                                                                                               \
    void                                                                                            \
    ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,                                \
                                              type *inout,                                          \
                                              int count,                                            \
                                              int threads_per_block,                                \
                                              int max_blocks,                                       \
                                              CUstream stream) {                                    \
        int threads = min(count, threads_per_block);                                                \
        int blocks  = min((count + threads-1) / threads, max_blocks);                               \
        int n = count;                                                                              \
        CUstream s = stream;                                                                        \
        ompi_op_cuda_2buff_##name##_##type_name##_kernel<<<blocks, threads, 0, s>>>(in, inout, n);  \
    }

#define FUNC_FUNC(name, type_name, type) FUNC_FUNC_FN(name, type_name, type, current_func)

#if defined(USE_VECTORS)
#define VFUNC_FUNC(name, type_name, type, vtype, vlen, vfn, fn)                                     \
    static __global__ void                                                                          \
    ompi_op_cuda_2buff_##name##_##type_name##_kernel(const type *__restrict__ in,                   \
                                                     type *__restrict__ inout, int n) {             \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n/vlen; i += stride) {                                              \
            ((vtype*)inout)[i] = vfn(((vtype*)inout)[i], ((vtype*)in)[i]);                          \
        }                                                                                           \
        int remainder = n%vlen;                                                                     \
        if (index == (n/vlen) && remainder != 0) {                                                  \
            while(remainder) {                                                                      \
                int idx = n - remainder--;                                                          \
                inout[idx] = fn(inout[idx], in[idx]);                                               \
            }                                                                                       \
        }                                                                                           \
    }                                                                                               \
    static void                                                                                     \
    ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,                                \
                                              type *inout,                                          \
                                              int count,                                            \
                                              int threads_per_block,                                \
                                              int max_blocks,                                       \
                                              CUstream stream) {                                    \
        int vcount  = (count + vlen-1)/vlen;                                                        \
        int threads = min(threads_per_block, vcount);                                               \
        int blocks  = min((vcount + threads-1) / threads, max_blocks);                              \
        int n = count;                                                                              \
        CUstream s = stream;                                                                        \
        ompi_op_cuda_2buff_##name##_##type_name##_kernel<<<blocks, threads, 0, s>>>(in, inout, n);  \
    }
#else
#define VFUNC_FUNC(name, type_name, type, vtype, vlen, vfn, fn) FUNC_FUNC_FN(name, type_name, type, fn)
#endif // defined(USE_VECTORS)

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */

#define LOC_FUNC(name, type_name, op)                                                               \
    static __global__ void                                                                          \
    ompi_op_cuda_2buff_##name##_##type_name##_kernel(const ompi_op_predefined_##type_name##_t *__restrict__ in,  \
                                                     ompi_op_predefined_##type_name##_t *__restrict__ inout,     \
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
    ompi_op_cuda_2buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *a,   \
                                            ompi_op_predefined_##type_name##_t *b,                  \
                                            int count,                                              \
                                            int threads_per_block,                                  \
                                            int max_blocks,                                         \
                                            CUstream stream) {                                      \
        int threads = min(count, threads_per_block);                                                \
        int blocks  = min((count + threads-1) / threads, max_blocks);                               \
        CUstream s = stream;                                                                        \
        ompi_op_cuda_2buff_##name##_##type_name##_kernel<<<blocks, threads, 0, s>>>(a, b, count);   \
    }

#define OPV_DISPATCH(name, type_name, type)                                                         \
    void ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,                           \
                                                   type *inout,                                     \
                                                   int count,                                       \
                                                   int threads_per_block,                           \
                                                   int max_blocks,                                  \
                                                   CUstream stream) {                               \
        static_assert(sizeof(type_name) <= sizeof(unsigned long long), "Unknown size type");        \
        if constexpr(!ISSIGNED(type)) {                                                                      \
            if constexpr(sizeof(type_name) == sizeof(unsigned char)) {                                               \
                ompi_op_cuda_2buff_##name##_uchar_submit((const unsigned char*)in, (unsigned char*)inout, count,    \
                                                         threads_per_block,                         \
                                                         max_blocks, stream);                        \
            } else if constexpr(sizeof(type_name) == sizeof(unsigned short)) {                                       \
                ompi_op_cuda_2buff_##name##_ushort_submit((const unsigned short*)in, (unsigned short*)inout, count, \
                                                          threads_per_block,                        \
                                                          max_blocks, stream);                       \
            } else if constexpr(sizeof(type_name) == sizeof(unsigned int)) {                                 \
                ompi_op_cuda_2buff_##name##_uint_submit((const unsigned int*)in, (unsigned int*)inout, count,         \
                                                        threads_per_block,                          \
                                                        max_blocks, stream);                         \
            } else if constexpr(sizeof(type_name) == sizeof(unsigned long)) {                                \
                ompi_op_cuda_2buff_##name##_ulong_submit((const unsigned long*)in, (unsigned long*)inout, count, \
                                                         threads_per_block,                         \
                                                         max_blocks, stream);                        \
            } else if constexpr(sizeof(type_name) == sizeof(unsigned long long)) {                           \
                ompi_op_cuda_2buff_##name##_ulonglong_submit((const unsigned long long*)in, (unsigned long long*)inout, count, \
                                                             threads_per_block,                     \
                                                             max_blocks, stream);                    \
            }                                                                                       \
        } else {                                                                                    \
            if constexpr(sizeof(type_name) == sizeof(char)) {                                                \
                ompi_op_cuda_2buff_##name##_char_submit((const char*)in, (char*)inout, count,       \
                                                        threads_per_block,                          \
                                                        max_blocks, stream);                         \
            } else if constexpr(sizeof(type_name) == sizeof(short)) {                                        \
                ompi_op_cuda_2buff_##name##_short_submit((const short*)in, (short*)inout, count,    \
                                                          threads_per_block,                        \
                                                          max_blocks, stream);                       \
            } else if constexpr(sizeof(type_name) == sizeof(int)) {                                          \
                ompi_op_cuda_2buff_##name##_int_submit((const int*)in, (int*)inout, count,          \
                                                        threads_per_block,                          \
                                                        max_blocks, stream);                         \
            } else if constexpr(sizeof(type_name) == sizeof(long)) {                                         \
                ompi_op_cuda_2buff_##name##_long_submit((const long*)in, (long*)inout, count,       \
                                                         threads_per_block,                         \
                                                         max_blocks, stream);                        \
            } else if constexpr(sizeof(type_name) == sizeof(long long)) {                                    \
                ompi_op_cuda_2buff_##name##_longlong_submit((const long long*)in, (long long*)inout, count,\
                                                             threads_per_block,                     \
                                                             max_blocks, stream);                    \
            }                                                                                       \
        }                                                                                           \
    }

/*************************************************************************
 * Max
 *************************************************************************/

/* C integer */
VFUNC_FUNC(max, char, char, char4, 4, vmax, max)
VFUNC_FUNC(max, uchar, unsigned char, uchar4, 4, vmax, max)
VFUNC_FUNC(max, short, short, short4, 4, vmax, max)
VFUNC_FUNC(max, ushort, unsigned short, ushort4, 4, vmax, max)
VFUNC_FUNC(max, int, int, int4, 4, vmax, max)
VFUNC_FUNC(max, uint, unsigned int, uint4, 4, vmax, max)

#undef current_func
#define current_func(a, b) max(a, b)
FUNC_FUNC(max,  long,  long)
FUNC_FUNC(max,  ulong, unsigned long)
FUNC_FUNC(max,  longlong, long long)
FUNC_FUNC(max,  ulonglong, unsigned long long)

/* dispatch fixed-size types */
OPV_DISPATCH(max,   int8_t,   int8_t)
OPV_DISPATCH(max,  uint8_t,  uint8_t)
OPV_DISPATCH(max,  int16_t,  int16_t)
OPV_DISPATCH(max, uint16_t, uint16_t)
OPV_DISPATCH(max,  int32_t,  int32_t)
OPV_DISPATCH(max, uint32_t, uint32_t)
OPV_DISPATCH(max,  int64_t,  int64_t)
OPV_DISPATCH(max, uint64_t, uint64_t)

#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
FUNC_FUNC(max, long_double, long double)

#if !defined(DO_NOT_USE_INTRINSICS)
#undef current_func
#define current_func(a, b) fmaxf(a, b)
#endif // DO_NOT_USE_INTRINSICS
FUNC_FUNC(max, float, float)

#if !defined(DO_NOT_USE_INTRINSICS)
#undef current_func
#define current_func(a, b) fmax(a, b)
#endif // DO_NOT_USE_INTRINSICS
FUNC_FUNC(max, double, double)

// __CUDA_ARCH__ is only defined when compiling device code
#if !defined(__CUDA_ARCH__) || __CUDA_ARCH__ >= 530
#undef current_func
#define current_func(a, b) __hmax2(a, b)
//VFUNC_FUNC(max, halfx, half, half2, 2, __hmax2, __hmax)
#endif // __CUDA_ARCH__

/*************************************************************************
 * Min
 *************************************************************************/

/* C integer */
VFUNC_FUNC(min, char, char, char4, 4, vmin, min)
VFUNC_FUNC(min, uchar, unsigned char, uchar4, 4, vmin, min)
VFUNC_FUNC(min, short, short, short4, 4, vmin, min)
VFUNC_FUNC(min, ushort, unsigned short, ushort4, 4, vmin, min)
VFUNC_FUNC(min, int, int, int4, 4, vmin, min)
VFUNC_FUNC(min, uint, unsigned int, uint4, 4, vmin, min)

#undef current_func
#define current_func(a, b) min(a, b)
FUNC_FUNC(min,  long,  long)
FUNC_FUNC(min,  ulong, unsigned long)
FUNC_FUNC(min,  longlong, long long)
FUNC_FUNC(min,  ulonglong, unsigned long long)
OPV_DISPATCH(min,   int8_t,   int8_t)
OPV_DISPATCH(min,  uint8_t,  uint8_t)
OPV_DISPATCH(min,  int16_t,  int16_t)
OPV_DISPATCH(min, uint16_t, uint16_t)
OPV_DISPATCH(min,  int32_t,  int32_t)
OPV_DISPATCH(min, uint32_t, uint32_t)
OPV_DISPATCH(min,  int64_t,  int64_t)
OPV_DISPATCH(min, uint64_t, uint64_t)



#if !defined(DO_NOT_USE_INTRINSICS)
#undef current_func
#define current_func(a, b) fminf(a, b)
#endif // DO_NOT_USE_INTRINSICS
FUNC_FUNC(min, float, float)

#if !defined(DO_NOT_USE_INTRINSICS)
#undef current_func
#define current_func(a, b) fmin(a, b)
#endif // DO_NOT_USE_INTRINSICS
FUNC_FUNC(min, double, double)

#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
FUNC_FUNC(min, long_double, long double)

// __CUDA_ARCH__ is only defined when compiling device code
#if !defined(__CUDA_ARCH__) || __CUDA_ARCH__ >= 530
#undef current_func
#define current_func(a, b) __hmin2(a, b)
//VFUNC_FUNC(min, half, half, half2, 2, __hmin2, __hmin)
#endif // __CUDA_ARCH__

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
VFUNC_FUNC(sum, char, char, char4, 4, vsum, tsum)
VFUNC_FUNC(sum, uchar, unsigned char, uchar4, 4, vsum, tsum)
VFUNC_FUNC(sum, short, short, short4, 4, vsum, tsum)
VFUNC_FUNC(sum, ushort, unsigned short, ushort4, 4, vsum, tsum)
VFUNC_FUNC(sum, int, int, int4, 4, vsum, tsum)
VFUNC_FUNC(sum, uint, unsigned int, uint4, 4, vsum, tsum)

#undef current_func
#define current_func(a, b) tsum(a, b)
FUNC_FUNC(sum,  long,  long)
FUNC_FUNC(sum,  ulong, unsigned long)
FUNC_FUNC(sum,  longlong, long long)
FUNC_FUNC(sum,  ulonglong, unsigned long long)

OPV_DISPATCH(sum,   int8_t,   int8_t)
OPV_DISPATCH(sum,  uint8_t,  uint8_t)
OPV_DISPATCH(sum,  int16_t,  int16_t)
OPV_DISPATCH(sum, uint16_t, uint16_t)
OPV_DISPATCH(sum,  int32_t,  int32_t)
OPV_DISPATCH(sum, uint32_t, uint32_t)
OPV_DISPATCH(sum,  int64_t,  int64_t)
OPV_DISPATCH(sum, uint64_t, uint64_t)

OPV_FUNC(sum, float, float, float4, 4, +)
OPV_FUNC(sum, double, double, double4, 4, +)
OP_FUNC(sum, long_double, long double, +)

// __CUDA_ARCH__ is only defined when compiling device code
#if !defined(__CUDA_ARCH__) || __CUDA_ARCH__ >= 530
#undef current_func
#define current_func(a, b) __hadd2(a, b)
//VFUNC_FUNC(sum, half, half, half2, 2, __hadd2, __hadd)
#endif // __CUDA_ARCH__

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC(sum, c_short_float_complex, short float _Complex, +=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_SUM_FUNC(c_short_float_complex, opal_short_float_t)
OP_FUNC(sum, c_long_double_complex, cuLongDoubleComplex, +=)
#endif
#endif // 0
#undef current_func
#define current_func(a, b) (cuCaddf(a,b))
FUNC_FUNC(sum, c_float_complex, cuFloatComplex)
#undef current_func
#define current_func(a, b) (cuCadd(a,b))
FUNC_FUNC(sum, c_double_complex, cuDoubleComplex)

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
#undef current_func
#define current_func(a, b) tprod(a, b)
FUNC_FUNC(prod, char, char)
FUNC_FUNC(prod, uchar, unsigned char)
FUNC_FUNC(prod, short, short)
FUNC_FUNC(prod, ushort, unsigned short)
FUNC_FUNC(prod, int, int)
FUNC_FUNC(prod, uint, unsigned int)
FUNC_FUNC(prod,  long,  long)
FUNC_FUNC(prod,  ulong, unsigned long)
FUNC_FUNC(prod,  longlong, long long)
FUNC_FUNC(prod,  ulonglong, unsigned long long)

OPV_DISPATCH(prod,   int8_t,   int8_t)
OPV_DISPATCH(prod,  uint8_t,  uint8_t)
OPV_DISPATCH(prod,  int16_t,  int16_t)
OPV_DISPATCH(prod, uint16_t, uint16_t)
OPV_DISPATCH(prod,  int32_t,  int32_t)
OPV_DISPATCH(prod, uint32_t, uint32_t)
OPV_DISPATCH(prod,  int64_t,  int64_t)
OPV_DISPATCH(prod, uint64_t, uint64_t)


OPV_FUNC(prod, float, float, float4, 4, *)
OPV_FUNC(prod, double, double, double4, 4, *)
OP_FUNC(prod, long_double, long double, *)

// __CUDA_ARCH__ is only defined when compiling device code
#if !defined(__CUDA_ARCH__) || __CUDA_ARCH__ >= 530
#undef current_func
#define current_func(a, b) __hmul2(a, b)
//VFUNC_FUNC(prod, half, half, half2, 2, __hmul2, __hmul)
#endif // __CUDA_ARCH__

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC(prod, c_short_float_complex, short float _Complex, *=)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_PROD_FUNC(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC(prod, c_long_double_complex, long double _Complex, *=)
#endif // 0
#undef current_func
#define current_func(a, b) (cuCmulf(a,b))
FUNC_FUNC(prod, c_float_complex, cuFloatComplex)
#undef current_func
#define current_func(a, b) (cuCmul(a,b))
FUNC_FUNC(prod, c_double_complex, cuDoubleComplex)

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
FUNC_FUNC(land,  ulong, unsigned long)

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
FUNC_FUNC(lor,  ulong, unsigned long)

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
FUNC_FUNC(lxor,  ulong, unsigned long)

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
FUNC_FUNC(band,  ulong, unsigned long)

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
FUNC_FUNC(bor,  ulong, unsigned long)

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
FUNC_FUNC(bxor,  ulong, unsigned long)

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

/* Fortran compat types */
LOC_FUNC(maxloc, 2float, >)
LOC_FUNC(maxloc, 2double, >)
LOC_FUNC(maxloc, 2int8, >)
LOC_FUNC(maxloc, 2int16, >)
LOC_FUNC(maxloc, 2int32, >)
LOC_FUNC(maxloc, 2int64, >)

/*************************************************************************
 * Min location
 *************************************************************************/

LOC_FUNC(minloc, float_int, <)
LOC_FUNC(minloc, double_int, <)
LOC_FUNC(minloc, long_int, <)
LOC_FUNC(minloc, 2int, <)
LOC_FUNC(minloc, short_int, <)
LOC_FUNC(minloc, long_double_int, <)

/* Fortran compat types */
LOC_FUNC(minloc, 2float, <)
LOC_FUNC(minloc, 2double, <)
LOC_FUNC(minloc, 2int8, <)
LOC_FUNC(minloc, 2int16, <)
LOC_FUNC(minloc, 2int32, <)
LOC_FUNC(minloc, 2int64, <)

/*
 *  This is a three buffer (2 input and 1 output) version of the reduction
 *    routines, needed for some optimizations.
 */
#define OP_FUNC_3BUF(name, type_name, type, op)                                                     \
    static __global__ void                                                                          \
    ompi_op_cuda_3buff_##name##_##type_name##_kernel(const type *__restrict__ in1,                  \
                                                     const type *__restrict__ in2,                  \
                                                     type *__restrict__ out, int n) {               \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            out[i] = in1[i] op in2[i];                                                              \
        }                                                                                           \
    }                                                                                               \
    void ompi_op_cuda_3buff_##name##_##type_name##_submit(const type *in1, const type *in2,         \
                                                          type *out, int count,                     \
                                                          int threads_per_block,                    \
                                                          int max_blocks,                           \
                                                          CUstream stream) {                        \
        int threads = min(count, threads_per_block);                                                \
        int blocks  = min((count + threads-1) / threads, max_blocks);                               \
        ompi_op_cuda_3buff_##name##_##type_name##_kernel<<<blocks, threads,                         \
                                                           0, stream>>>(in1, in2, out, count);      \
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
    ompi_op_cuda_3buff_##name##_##type_name##_kernel(const type *__restrict__ in1,                  \
                                                     const type *__restrict__ in2,                  \
                                                     type *__restrict__ out, int n) {               \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                    \
        const int stride = blockDim.x * gridDim.x;                                                  \
        for (int i = index; i < n; i += stride) {                                                   \
            out[i] = current_func(in1[i], in2[i]);                                                  \
        }                                                                                           \
    }                                                                                               \
    void                                                                                            \
    ompi_op_cuda_3buff_##name##_##type_name##_submit(const type *in1, const type *in2,              \
                                                     type *out, int count,                          \
                                                     int threads_per_block,                         \
                                                     int max_blocks,                                \
                                                     CUstream stream) {                             \
        int threads = min(count, threads_per_block);                                                \
        int blocks  = min((count + threads-1) / threads, max_blocks);                               \
        ompi_op_cuda_3buff_##name##_##type_name##_kernel<<<blocks, threads,                         \
                                                           0, stream>>>(in1, in2, out, count);      \
    }

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */
#define LOC_FUNC_3BUF(name, type_name, op)                                                          \
    static __global__ void                                                                          \
    ompi_op_cuda_3buff_##name##_##type_name##_kernel(const ompi_op_predefined_##type_name##_t *__restrict__ in1, \
                                                     const ompi_op_predefined_##type_name##_t *__restrict__ in2, \
                                                     ompi_op_predefined_##type_name##_t *__restrict__ out,       \
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
    ompi_op_cuda_3buff_##name##_##type_name##_submit(const ompi_op_predefined_##type_name##_t *in1, \
                                                     const ompi_op_predefined_##type_name##_t *in2, \
                                                     ompi_op_predefined_##type_name##_t *out,       \
                                                     int count,                                     \
                                                     int threads_per_block,                         \
                                                     int max_blocks,                                \
                                                     CUstream stream)                               \
    {                                                                                               \
        int threads = min(count, threads_per_block);                                                \
        int blocks  = min((count + threads-1) / threads, max_blocks);                               \
        ompi_op_cuda_3buff_##name##_##type_name##_kernel<<<blocks, threads,                         \
                                                           0, stream>>>(in1, in2, out, count);      \
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
FUNC_FUNC_3BUF(max,  ulong, unsigned long)

/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_3BUF(max, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_3BUF(max, short_float, opal_short_float_t)
#endif
FUNC_FUNC_3BUF(max, float, float)
FUNC_FUNC_3BUF(max, double, double)
FUNC_FUNC_3BUF(max, long_double, long double)


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
FUNC_FUNC_3BUF(min,  ulong, unsigned long)

/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
FUNC_FUNC_3BUF(min, short_float, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
FUNC_FUNC_3BUF(min, short_float, opal_short_float_t)
#endif
FUNC_FUNC_3BUF(min, float, float)
FUNC_FUNC_3BUF(min, double, double)
FUNC_FUNC_3BUF(min, long_double, long double)

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
OP_FUNC_3BUF(sum,  ulong, unsigned long, +)

/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_3BUF(sum, short_float, short float, +)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_3BUF(sum, short_float, opal_short_float_t, +)
#endif
OP_FUNC_3BUF(sum, float, float, +)
OP_FUNC_3BUF(sum, double, double, +)
OP_FUNC_3BUF(sum, long_double, long double, +)

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_3BUF(sum, c_short_float_complex, short float _Complex, +)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_SUM_FUNC_3BUF(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC_3BUF(sum, c_long_double_complex, cuLongDoubleComplex, +)
#endif // 0
#undef current_func
#define current_func(a, b) (cuCaddf(a,b))
FUNC_FUNC_3BUF(sum, c_float_complex, cuFloatComplex)
#undef current_func
#define current_func(a, b) (cuCadd(a,b))
FUNC_FUNC_3BUF(sum, c_double_complex, cuDoubleComplex)

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
OP_FUNC_3BUF(prod,  ulong, unsigned long, *)

/* Floating point */
#if defined(HAVE_SHORT_FLOAT)
OP_FUNC_3BUF(prod, short_float, short float, *)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
OP_FUNC_3BUF(prod, short_float, opal_short_float_t, *)
#endif
OP_FUNC_3BUF(prod, float, float, *)
OP_FUNC_3BUF(prod, double, double, *)
OP_FUNC_3BUF(prod, long_double, long double, *)

/* Complex */
#if 0
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
OP_FUNC_3BUF(prod, c_short_float_complex, short float _Complex, *)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COMPLEX_PROD_FUNC_3BUF(c_short_float_complex, opal_short_float_t)
#endif
OP_FUNC_3BUF(prod, c_long_double_complex, long double _Complex, *)
#endif // 0
#undef current_func
#define current_func(a, b) (cuCmulf(a,b))
FUNC_FUNC_3BUF(prod, c_float_complex, cuFloatComplex)
#undef current_func
#define current_func(a, b) (cuCmul(a,b))
FUNC_FUNC_3BUF(prod, c_double_complex, cuDoubleComplex)

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
FUNC_FUNC_3BUF(land,  ulong, unsigned long)

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
FUNC_FUNC_3BUF(lor,  ulong, unsigned long)

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
FUNC_FUNC_3BUF(lxor,  ulong, unsigned long)

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
FUNC_FUNC_3BUF(band,  ulong, unsigned long)

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
FUNC_FUNC_3BUF(bor,  ulong, unsigned long)

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
FUNC_FUNC_3BUF(bxor,  ulong, unsigned long)

/* Byte */
FUNC_FUNC_3BUF(bxor, byte, char)

/*************************************************************************
 * Max location
 *************************************************************************/

LOC_FUNC_3BUF(maxloc, float_int, >)
LOC_FUNC_3BUF(maxloc, double_int, >)
LOC_FUNC_3BUF(maxloc, long_int, >)
LOC_FUNC_3BUF(maxloc, 2int, >)
LOC_FUNC_3BUF(maxloc, short_int, >)
LOC_FUNC_3BUF(maxloc, long_double_int, >)

/* Fortran compat types */
LOC_FUNC_3BUF(maxloc, 2float, >)
LOC_FUNC_3BUF(maxloc, 2double, >)
LOC_FUNC_3BUF(maxloc, 2int8, >)
LOC_FUNC_3BUF(maxloc, 2int16, >)
LOC_FUNC_3BUF(maxloc, 2int32, >)
LOC_FUNC_3BUF(maxloc, 2int64, >)

/*************************************************************************
 * Min location
 *************************************************************************/

LOC_FUNC_3BUF(minloc, float_int, <)
LOC_FUNC_3BUF(minloc, double_int, <)
LOC_FUNC_3BUF(minloc, long_int, <)
LOC_FUNC_3BUF(minloc, 2int, <)
LOC_FUNC_3BUF(minloc, short_int, <)
LOC_FUNC_3BUF(minloc, long_double_int, <)

/* Fortran compat types */
LOC_FUNC_3BUF(minloc, 2float, <)
LOC_FUNC_3BUF(minloc, 2double, <)
LOC_FUNC_3BUF(minloc, 2int8, <)
LOC_FUNC_3BUF(minloc, 2int16, <)
LOC_FUNC_3BUF(minloc, 2int32, <)
LOC_FUNC_3BUF(minloc, 2int64, <)

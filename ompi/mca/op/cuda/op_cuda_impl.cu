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
#define ALIGN(x,a,t)            (((x)+((t)(a)-1)) & ~(((t)(a)-1)))
#define ALIGN_PTR(x,a,t)        ((t)ALIGN((uintptr_t)x, a, uintptr_t))
#define ALIGN_PAD_AMOUNT(x,s)   ((~((uintptr_t)(x))+1) & ((uintptr_t)(s)+(!(uintptr_t)(s))-1))

template<typename T, size_t N>
struct __align__(sizeof(T)*N) Vec {
    T v[N];

    template<typename... S>
    __device__ Vec(S... l)
    : v{std::forward<S>(l)...}
    { }

    __device__
    T& operator[](size_t i) { return v[i]; }
    __device__
    const T& operator[](size_t i) const { return v[i]; }
};

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
static inline __device__ constexpr T tband(T a, T b) {
    return a&b;
}

template<typename T>
static inline __device__ constexpr T tbor(T a, T b) {
    return a|b;
}

template<typename T>
static inline __device__ constexpr T tbxor(T a, T b) {
    return a^b;
}

template<typename T>
static inline __device__ constexpr T tland(T a, T b) {
    return a&&b;
}

template<typename T>
static inline __device__ constexpr T tlor(T a, T b) {
    return a||b;
}

template<typename T>
static inline __device__ constexpr T tlxor(T a, T b) {
    return ((!!a) ^ (!!b)) ? 1 : 0;
}

template<typename V, typename Fn, size_t ... Ns>
__device__
static inline V apply(const V& a, const V& b, Fn&& fn, std::index_sequence<Ns...>) {
    /* apply fn to all members of the vector and return a new vector */
    return {fn(a[Ns], b[Ns])...};
}


template<typename T, size_t N>
static inline __device__ Vec<T, N> vmax(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return (a > b) ? a : b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vmin(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return (a < b) ? a : b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vsum(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return a + b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vprod(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return a * b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vband(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return a & b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vbor(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return a | b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vbxor(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return a ^ b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vland(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return a && b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vlor(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return a || b; }, std::make_index_sequence<N>{});
}

template<typename T, size_t N>
static inline __device__ Vec<T, N> vlxor(const Vec<T, N>& a, const Vec<T, N>& b) {
    return apply(a, b, [](const T&a, const T&b) -> T { return ((!!a) ^ (!!b)) ? 1 : 0; }, std::make_index_sequence<N>{});
}


/* TODO: missing support for
 * - short float (conditional on whether short float is available)
 */

#define USE_VECTORS 1

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
#define VFUNC_FUNC(name, type_name, type, vlen, vfn, fn)                                                    \
    static __global__ void                                                                                  \
    ompi_op_cuda_2buff_##name##_##type_name##_kernel_v(const type *__restrict__ in,                         \
                                                       type *__restrict__ inout, int n) {                   \
        using vtype = Vec<type, vlen>;                                                                      \
        constexpr const size_t alignment = sizeof(type)*vlen;                                               \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                            \
        const int stride = blockDim.x * gridDim.x;                                                          \
        size_t in_pad = ALIGN_PAD_AMOUNT(in, alignment);                                                    \
        const vtype * inv = ALIGN_PTR(in, alignment, const vtype*);                                         \
        vtype * inoutv = ALIGN_PTR(inout, alignment, vtype*);                                               \
        for (int i = index; i < (n/vlen - in_pad/sizeof(type)); i += stride) {                              \
            inoutv[i] = vfn(inoutv[i], inv[i]);                                                             \
        }                                                                                                   \
        if (in_pad > 0) {                                                                                   \
            /* manage front values */                                                                       \
            if (index < ((in_pad/sizeof(type)) - 1)) {                                                      \
                inout[index] = fn(inout[index], in[index]);                                                 \
            }                                                                                               \
        }                                                                                                   \
        int remainder = (n%vlen);                                                                           \
        if (remainder > 0) {                                                                                \
            /* manage back values */                                                                        \
            if (index < (remainder-1)) {                                                                    \
                size_t idx = n - remainder + index;                                                         \
                inout[idx] = fn(inout[idx], in[idx]);                                                       \
            }                                                                                               \
        }                                                                                                   \
    }                                                                                                       \
    static __global__ void                                                                                  \
    ompi_op_cuda_2buff_##name##_##type_name##_kernel(const type *__restrict__ in,                           \
                                                     type *__restrict__ inout, int n) {                     \
        /* non-vectorized version (e.g., due to mismatching alignment) */                                   \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                            \
        const int stride = blockDim.x * gridDim.x;                                                          \
        for (int i = index; i < n; i += stride) {                                                           \
            inout[i] = fn(inout[i], in[i]);                                                                 \
        }                                                                                                   \
    }                                                                                                       \
    void                                                                                                    \
    ompi_op_cuda_2buff_##name##_##type_name##_submit(const type *in,                                        \
                                              type *inout,                                                  \
                                              int count,                                                    \
                                              int threads_per_block,                                        \
                                              int max_blocks,                                               \
                                              CUstream stream) {                                            \
        int vcount  = (count + vlen-1)/vlen;                                                                \
        int threads = min(threads_per_block, vcount);                                                       \
        int blocks  = min((vcount + threads-1) / threads, max_blocks);                                      \
        int n = count;                                                                                      \
        CUstream s = stream;                                                                                \
        constexpr const size_t alignment = sizeof(type)*vlen;                                               \
        size_t in_pad = ALIGN_PAD_AMOUNT(in, alignment);                                                    \
        size_t inout_pad = ALIGN_PAD_AMOUNT(inout, alignment);                                              \
        if (in_pad == inout_pad) {                                                                          \
            ompi_op_cuda_2buff_##name##_##type_name##_kernel_v<<<blocks, threads, 0, s>>>(in, inout, n);    \
        } else {                                                                                            \
            ompi_op_cuda_2buff_##name##_##type_name##_kernel<<<blocks, threads, 0, s>>>(in, inout, n);      \
        }   \
    }
#else
#define VFUNC_FUNC(name, type_name, type, vlen, vfn, fn) FUNC_FUNC_FN(name, type_name, type, fn)
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

/*************************************************************************
 * Max
 *************************************************************************/

/* C integer */

/* fixed-size types: 16B vector sizes
 * TODO: should this be fine-tuned to the architecture? */
VFUNC_FUNC(max,   int8_t,   int8_t, 16, vmax, tmax)
VFUNC_FUNC(max,  uint8_t,  uint8_t, 16, vmax, tmax)
VFUNC_FUNC(max,  int16_t,  int16_t, 8, vmax, tmax)
VFUNC_FUNC(max, uint16_t, uint16_t, 8, vmax, tmax)
VFUNC_FUNC(max,  int32_t,  int32_t, 4, vmax, tmax)
VFUNC_FUNC(max, uint32_t, uint32_t, 4, vmax, tmax)
VFUNC_FUNC(max,  int64_t,  int64_t, 2, vmax, tmax)
VFUNC_FUNC(max, uint64_t, uint64_t, 2, vmax, tmax)

VFUNC_FUNC(max,  long,  long, 2, vmax, tmax)
VFUNC_FUNC(max, ulong, unsigned long, 2, vmax, tmax)

/* float */
VFUNC_FUNC(max, float, float, 4, vmax, tmax)
VFUNC_FUNC(max, double, double, 2, vmax, tmax)
VFUNC_FUNC(max, long_double, long double, 1, vmax, tmax)

/*************************************************************************
 * Min
 *************************************************************************/

/* C integer */
VFUNC_FUNC(min,   int8_t,   int8_t, 16, vmin, tmin)
VFUNC_FUNC(min,  uint8_t,  uint8_t, 16, vmin, tmin)
VFUNC_FUNC(min,  int16_t,  int16_t, 8, vmin, tmin)
VFUNC_FUNC(min, uint16_t, uint16_t, 8, vmin, tmin)
VFUNC_FUNC(min,  int32_t,  int32_t, 4, vmin, tmin)
VFUNC_FUNC(min, uint32_t, uint32_t, 4, vmin, tmin)
VFUNC_FUNC(min,  int64_t,  int64_t, 2, vmin, tmin)
VFUNC_FUNC(min, uint64_t, uint64_t, 2, vmin, tmin)
VFUNC_FUNC(min,  long,  long, 2, vmin, tmin)
VFUNC_FUNC(min, ulong, unsigned long, 2, vmin, tmin)

/* float */
VFUNC_FUNC(min, float, float, 4, vmin, tmin)
VFUNC_FUNC(min, double, double, 2, vmin, tmin)
VFUNC_FUNC(min, long_double, long double, 1, vmin, tmin)

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
VFUNC_FUNC(sum,   int8_t,   int8_t, 16, vsum, tsum)
VFUNC_FUNC(sum,  uint8_t,  uint8_t, 16, vsum, tsum)
VFUNC_FUNC(sum,  int16_t,  int16_t, 8, vsum, tsum)
VFUNC_FUNC(sum, uint16_t, uint16_t, 8, vsum, tsum)
VFUNC_FUNC(sum,  int32_t,  int32_t, 4, vsum, tsum)
VFUNC_FUNC(sum, uint32_t, uint32_t, 4, vsum, tsum)
VFUNC_FUNC(sum,  int64_t,  int64_t, 2, vsum, tsum)
VFUNC_FUNC(sum, uint64_t, uint64_t, 2, vsum, tsum)
VFUNC_FUNC(sum,  long,  long, 2, vsum, tsum)
VFUNC_FUNC(sum, ulong, unsigned long, 2, vsum, tsum)

/* float */
VFUNC_FUNC(sum, float, float, 4, vsum, tsum)
VFUNC_FUNC(sum, double, double, 2, vsum, tsum)
VFUNC_FUNC(sum, long_double, long double, 1, vsum, tsum)

/* Complex */
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
VFUNC_FUNC(prod,   int8_t,   int8_t, 16, vprod, tprod)
VFUNC_FUNC(prod,  uint8_t,  uint8_t, 16, vprod, tprod)
VFUNC_FUNC(prod,  int16_t,  int16_t, 8, vprod, tprod)
VFUNC_FUNC(prod, uint16_t, uint16_t, 8, vprod, tprod)
VFUNC_FUNC(prod,  int32_t,  int32_t, 4, vprod, tprod)
VFUNC_FUNC(prod, uint32_t, uint32_t, 4, vprod, tprod)
VFUNC_FUNC(prod,  int64_t,  int64_t, 2, vprod, tprod)
VFUNC_FUNC(prod, uint64_t, uint64_t, 2, vprod, tprod)
VFUNC_FUNC(prod,  long,  long, 2, vprod, tprod)
VFUNC_FUNC(prod, ulong, unsigned long, 2, vprod, tprod)

/* float */
VFUNC_FUNC(prod, float, float, 4, vprod, tprod)
VFUNC_FUNC(prod, double, double, 2, vprod, tprod)
VFUNC_FUNC(prod, long_double, long double, 1, vprod, tprod)

/* Complex */
#undef current_func
#define current_func(a, b) (cuCmulf(a,b))
FUNC_FUNC(prod, c_float_complex, cuFloatComplex)
#undef current_func
#define current_func(a, b) (cuCmul(a,b))
FUNC_FUNC(prod, c_double_complex, cuDoubleComplex)

/*************************************************************************
 * Logical AND
 *************************************************************************/

/* C integer */
VFUNC_FUNC(land,   int8_t,   int8_t, 16, vland, tland)
VFUNC_FUNC(land,  uint8_t,  uint8_t, 16, vland, tland)
VFUNC_FUNC(land,  int16_t,  int16_t, 8, vland, tland)
VFUNC_FUNC(land, uint16_t, uint16_t, 8, vland, tland)
VFUNC_FUNC(land,  int32_t,  int32_t, 4, vland, tland)
VFUNC_FUNC(land, uint32_t, uint32_t, 4, vland, tland)
VFUNC_FUNC(land,  int64_t,  int64_t, 2, vland, tland)
VFUNC_FUNC(land, uint64_t, uint64_t, 2, vland, tland)
VFUNC_FUNC(land,  long,  long, 2, vland, tland)
VFUNC_FUNC(land, ulong, unsigned long, 2, vland, tland)

/* C++ bool */
VFUNC_FUNC(land, bool, bool, 16, vland, tland)

/*************************************************************************
 * Logical OR
 *************************************************************************/

/* C integer */
VFUNC_FUNC(lor,   int8_t,   int8_t, 16, vlor, tlor)
VFUNC_FUNC(lor,  uint8_t,  uint8_t, 16, vlor, tlor)
VFUNC_FUNC(lor,  int16_t,  int16_t, 8, vlor, tlor)
VFUNC_FUNC(lor, uint16_t, uint16_t, 8, vlor, tlor)
VFUNC_FUNC(lor,  int32_t,  int32_t, 4, vlor, tlor)
VFUNC_FUNC(lor, uint32_t, uint32_t, 4, vlor, tlor)
VFUNC_FUNC(lor,  int64_t,  int64_t, 2, vlor, tlor)
VFUNC_FUNC(lor, uint64_t, uint64_t, 2, vlor, tlor)
VFUNC_FUNC(lor,  long,  long, 2, vlor, tlor)
VFUNC_FUNC(lor, ulong, unsigned long, 2, vlor, tlor)

/* C++ bool */
VFUNC_FUNC(lor, bool, bool, 16, vlor, tlor)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

/* C integer */
VFUNC_FUNC(lxor,   int8_t,   int8_t, 16, vlxor, tlxor)
VFUNC_FUNC(lxor,  uint8_t,  uint8_t, 16, vlxor, tlxor)
VFUNC_FUNC(lxor,  int16_t,  int16_t, 8, vlxor, tlxor)
VFUNC_FUNC(lxor, uint16_t, uint16_t, 8, vlxor, tlxor)
VFUNC_FUNC(lxor,  int32_t,  int32_t, 4, vlxor, tlxor)
VFUNC_FUNC(lxor, uint32_t, uint32_t, 4, vlxor, tlxor)
VFUNC_FUNC(lxor,  int64_t,  int64_t, 2, vlxor, tlxor)
VFUNC_FUNC(lxor, uint64_t, uint64_t, 2, vlxor, tlxor)
VFUNC_FUNC(lxor,  long,  long, 2, vlxor, tlxor)
VFUNC_FUNC(lxor, ulong, unsigned long, 2, vlxor, tlxor)

/* C++ bool */
VFUNC_FUNC(lxor, bool, bool, 16, vlxor, tlxor)


/*************************************************************************
 * Bitwise AND
 *************************************************************************/

/* C integer */
VFUNC_FUNC(band,   int8_t,   int8_t, 16, vband, tband)
VFUNC_FUNC(band,  uint8_t,  uint8_t, 16, vband, tband)
VFUNC_FUNC(band,  int16_t,  int16_t, 8, vband, tband)
VFUNC_FUNC(band, uint16_t, uint16_t, 8, vband, tband)
VFUNC_FUNC(band,  int32_t,  int32_t, 4, vband, tband)
VFUNC_FUNC(band, uint32_t, uint32_t, 4, vband, tband)
VFUNC_FUNC(band,  int64_t,  int64_t, 2, vband, tband)
VFUNC_FUNC(band, uint64_t, uint64_t, 2, vband, tband)
VFUNC_FUNC(band,  long,  long, 2, vband, tband)
VFUNC_FUNC(band, ulong, unsigned long, 2, vband, tband)

/* C++ byte */
VFUNC_FUNC(band, byte, char, 16, vband, tband)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

/* C integer */
VFUNC_FUNC(bor,   int8_t,   int8_t, 16, vbor, tbor)
VFUNC_FUNC(bor,  uint8_t,  uint8_t, 16, vbor, tbor)
VFUNC_FUNC(bor,  int16_t,  int16_t, 8, vbor, tbor)
VFUNC_FUNC(bor, uint16_t, uint16_t, 8, vbor, tbor)
VFUNC_FUNC(bor,  int32_t,  int32_t, 4, vbor, tbor)
VFUNC_FUNC(bor, uint32_t, uint32_t, 4, vbor, tbor)
VFUNC_FUNC(bor,  int64_t,  int64_t, 2, vbor, tbor)
VFUNC_FUNC(bor, uint64_t, uint64_t, 2, vbor, tbor)
VFUNC_FUNC(bor,  long,  long, 2, vbor, tbor)
VFUNC_FUNC(bor, ulong, unsigned long, 2, vbor, tbor)

/* C++ byte */
VFUNC_FUNC(bor, byte, char, 16, vbor, tbor)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

/* C integer */
VFUNC_FUNC(bxor,   int8_t,   int8_t, 16, vbxor, tbxor)
VFUNC_FUNC(bxor,  uint8_t,  uint8_t, 16, vbxor, tbxor)
VFUNC_FUNC(bxor,  int16_t,  int16_t, 8, vbxor, tbxor)
VFUNC_FUNC(bxor, uint16_t, uint16_t, 8, vbxor, tbxor)
VFUNC_FUNC(bxor,  int32_t,  int32_t, 4, vbxor, tbxor)
VFUNC_FUNC(bxor, uint32_t, uint32_t, 4, vbxor, tbxor)
VFUNC_FUNC(bxor,  int64_t,  int64_t, 2, vbxor, tbxor)
VFUNC_FUNC(bxor, uint64_t, uint64_t, 2, vbxor, tbxor)
VFUNC_FUNC(bxor,  long,  long, 2, vbxor, tbxor)
VFUNC_FUNC(bxor, ulong, unsigned long, 2, vbxor, tbxor)

/* C++ byte */
VFUNC_FUNC(bxor, byte, char, 16, vbxor, tbxor)

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


#if defined(USE_VECTORS)
#define VFUNC_FUNC_3BUF(name, type_name, type, vlen, vfn, fn)                                               \
    static __global__ void                                                                                  \
    ompi_op_cuda_3buff_##name##_##type_name##_kernel_v(const type *__restrict__ in1,                        \
                                                       const type *__restrict__ in2,                        \
                                                       type *__restrict__ out, int n) {                     \
        using vtype = Vec<type, vlen>;                                                                      \
        constexpr const size_t alignment = sizeof(type)*vlen;                                               \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                            \
        const int stride = blockDim.x * gridDim.x;                                                          \
        size_t in_pad = ALIGN_PAD_AMOUNT(in1, alignment);                                                   \
        const vtype * in1v = ALIGN_PTR(in1, alignment, const vtype*);                                       \
        const vtype * in2v = ALIGN_PTR(in2, alignment, const vtype*);                                       \
              vtype * outv = ALIGN_PTR(out, alignment, vtype*);                                             \
        for (int i = index; i < (n/vlen - in_pad/sizeof(type)); i += stride) {                              \
            outv[i] = vfn(in1v[i], in2v[i]);                                                                \
        }                                                                                                   \
        if (in_pad > 0) {                                                                                   \
            /* manage front values */                                                                       \
            if (index < ((in_pad/sizeof(type)) - 1)) {                                                      \
                out[index] = fn(in1[index], in2[index]);                                                    \
            }                                                                                               \
        }                                                                                                   \
        int remainder = (n%vlen);                                                                           \
        if (remainder > 0) {                                                                                \
            /* manage back values */                                                                        \
            if (index < (remainder-1)) {                                                                    \
                size_t idx = n - remainder + index;                                                         \
                out[idx] = fn(in1[idx], in2[idx]);                                                          \
            }                                                                                               \
        }                                                                                                   \
    }                                                                                                       \
    static __global__ void                                                                                  \
    ompi_op_cuda_3buff_##name##_##type_name##_kernel(const type *__restrict__ in1,                          \
                                                     const type *__restrict__ in2,                          \
                                                     type *__restrict__ out, int n) {                       \
        /* non-vectorized version (e.g., due to mismatching alignment) */                                   \
        const int index = blockIdx.x * blockDim.x + threadIdx.x;                                            \
        const int stride = blockDim.x * gridDim.x;                                                          \
        for (int i = index; i < n; i += stride) {                                                           \
            out[i] = fn(in1[i], in2[i]);                                                                    \
        }                                                                                                   \
    }                                                                                                       \
    void                                                                                                    \
    ompi_op_cuda_3buff_##name##_##type_name##_submit(const type *in1,                                       \
                                                     const type *in2,                                       \
                                                     type *out,                                             \
                                                     int count,                                             \
                                                     int threads_per_block,                                 \
                                                     int max_blocks,                                        \
                                                     CUstream stream) {                                     \
        int vcount  = (count + vlen-1)/vlen;                                                                \
        int threads = min(threads_per_block, vcount);                                                       \
        int blocks  = min((vcount + threads-1) / threads, max_blocks);                                      \
        int n = count;                                                                                      \
        CUstream s = stream;                                                                                \
        constexpr const size_t alignment = sizeof(type)*vlen;                                               \
        size_t in1_pad = ALIGN_PAD_AMOUNT(in1, alignment);                                                  \
        size_t in2_pad = ALIGN_PAD_AMOUNT(in2, alignment);                                                  \
        size_t out_pad = ALIGN_PAD_AMOUNT(out, alignment);                                                  \
        if (in1_pad == in2_pad && in1_pad == out_pad) {                                                     \
            ompi_op_cuda_3buff_##name##_##type_name##_kernel_v<<<blocks, threads, 0, s>>>(in1, in2, out, n);\
        } else {                                                                                            \
            ompi_op_cuda_3buff_##name##_##type_name##_kernel<<<blocks, threads, 0, s>>>(in1, in2, out, n);  \
        }                                                                                                   \
    }
#else
#define VFUNC_FUNC(name, type_name, type, vlen, vfn, fn) FUNC_FUNC_FN(name, type_name, type, fn)
#endif // defined(USE_VECTORS)
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

/* fixed-size types: 16B vector sizes
 * TODO: should this be fine-tuned to the architecture? */
 VFUNC_FUNC_3BUF(max,   int8_t,   int8_t, 16, vmax, tmax)
 VFUNC_FUNC_3BUF(max,  uint8_t,  uint8_t, 16, vmax, tmax)
 VFUNC_FUNC_3BUF(max,  int16_t,  int16_t, 8, vmax, tmax)
 VFUNC_FUNC_3BUF(max, uint16_t, uint16_t, 8, vmax, tmax)
 VFUNC_FUNC_3BUF(max,  int32_t,  int32_t, 4, vmax, tmax)
 VFUNC_FUNC_3BUF(max, uint32_t, uint32_t, 4, vmax, tmax)
 VFUNC_FUNC_3BUF(max,  int64_t,  int64_t, 2, vmax, tmax)
 VFUNC_FUNC_3BUF(max, uint64_t, uint64_t, 2, vmax, tmax)

 /* float */
 VFUNC_FUNC_3BUF(max, float, float, 4, vmax, tmax)
 VFUNC_FUNC_3BUF(max, double, double, 2, vmax, tmax)
 VFUNC_FUNC_3BUF(max, long_double, long double, 1, vmax, tmax)


/*************************************************************************
 * Min
 *************************************************************************/

/* C integer */
VFUNC_FUNC_3BUF(min,   int8_t,   int8_t, 16, vmin, tmin)
VFUNC_FUNC_3BUF(min,  uint8_t,  uint8_t, 16, vmin, tmin)
VFUNC_FUNC_3BUF(min,  int16_t,  int16_t, 8, vmin, tmin)
VFUNC_FUNC_3BUF(min, uint16_t, uint16_t, 8, vmin, tmin)
VFUNC_FUNC_3BUF(min,  int32_t,  int32_t, 4, vmin, tmin)
VFUNC_FUNC_3BUF(min, uint32_t, uint32_t, 4, vmin, tmin)
VFUNC_FUNC_3BUF(min,  int64_t,  int64_t, 2, vmin, tmin)
VFUNC_FUNC_3BUF(min, uint64_t, uint64_t, 2, vmin, tmin)
VFUNC_FUNC_3BUF(min,  long,  long, 2, vmin, tmin)
VFUNC_FUNC_3BUF(min, ulong, unsigned long, 2, vmin, tmin)

/* float */
VFUNC_FUNC_3BUF(min, float, float, 4, vmin, tmin)
VFUNC_FUNC_3BUF(min, double, double, 2, vmin, tmin)
VFUNC_FUNC_3BUF(min, long_double, long double, 1, vmin, tmin)

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
VFUNC_FUNC_3BUF(sum,   int8_t,   int8_t, 16, vsum, tsum)
VFUNC_FUNC_3BUF(sum,  uint8_t,  uint8_t, 16, vsum, tsum)
VFUNC_FUNC_3BUF(sum,  int16_t,  int16_t, 8, vsum, tsum)
VFUNC_FUNC_3BUF(sum, uint16_t, uint16_t, 8, vsum, tsum)
VFUNC_FUNC_3BUF(sum,  int32_t,  int32_t, 4, vsum, tsum)
VFUNC_FUNC_3BUF(sum, uint32_t, uint32_t, 4, vsum, tsum)
VFUNC_FUNC_3BUF(sum,  int64_t,  int64_t, 2, vsum, tsum)
VFUNC_FUNC_3BUF(sum, uint64_t, uint64_t, 2, vsum, tsum)
VFUNC_FUNC_3BUF(sum,  long,  long, 2, vsum, tsum)
VFUNC_FUNC_3BUF(sum, ulong, unsigned long, 2, vsum, tsum)

/* float */
VFUNC_FUNC_3BUF(sum, float, float, 4, vsum, tsum)
VFUNC_FUNC_3BUF(sum, double, double, 2, vsum, tsum)
VFUNC_FUNC_3BUF(sum, long_double, long double, 1, vsum, tsum)

/* Complex */
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
VFUNC_FUNC_3BUF(prod,   int8_t,   int8_t, 16, vprod, tprod)
VFUNC_FUNC_3BUF(prod,  uint8_t,  uint8_t, 16, vprod, tprod)
VFUNC_FUNC_3BUF(prod,  int16_t,  int16_t, 8, vprod, tprod)
VFUNC_FUNC_3BUF(prod, uint16_t, uint16_t, 8, vprod, tprod)
VFUNC_FUNC_3BUF(prod,  int32_t,  int32_t, 4, vprod, tprod)
VFUNC_FUNC_3BUF(prod, uint32_t, uint32_t, 4, vprod, tprod)
VFUNC_FUNC_3BUF(prod,  int64_t,  int64_t, 2, vprod, tprod)
VFUNC_FUNC_3BUF(prod, uint64_t, uint64_t, 2, vprod, tprod)
VFUNC_FUNC_3BUF(prod,  long,  long, 2, vprod, tprod)
VFUNC_FUNC_3BUF(prod, ulong, unsigned long, 2, vprod, tprod)

/* float */
VFUNC_FUNC_3BUF(prod, float, float, 4, vprod, tprod)
VFUNC_FUNC_3BUF(prod, double, double, 2, vprod, tprod)
VFUNC_FUNC_3BUF(prod, long_double, long double, 1, vprod, tprod)

/* Complex */
#undef current_func
#define current_func(a, b) (cuCmulf(a,b))
FUNC_FUNC_3BUF(prod, c_float_complex, cuFloatComplex)
#undef current_func
#define current_func(a, b) (cuCmul(a,b))
FUNC_FUNC_3BUF(prod, c_double_complex, cuDoubleComplex)

/*************************************************************************
 * Logical AND
 *************************************************************************/

/* C integer */
VFUNC_FUNC_3BUF(land,   int8_t,   int8_t, 16, vland, tland)
VFUNC_FUNC_3BUF(land,  uint8_t,  uint8_t, 16, vland, tland)
VFUNC_FUNC_3BUF(land,  int16_t,  int16_t, 8, vland, tland)
VFUNC_FUNC_3BUF(land, uint16_t, uint16_t, 8, vland, tland)
VFUNC_FUNC_3BUF(land,  int32_t,  int32_t, 4, vland, tland)
VFUNC_FUNC_3BUF(land, uint32_t, uint32_t, 4, vland, tland)
VFUNC_FUNC_3BUF(land,  int64_t,  int64_t, 2, vland, tland)
VFUNC_FUNC_3BUF(land, uint64_t, uint64_t, 2, vland, tland)
VFUNC_FUNC_3BUF(land,  long,  long, 2, vland, tland)
VFUNC_FUNC_3BUF(land, ulong, unsigned long, 2, vland, tland)

/* C++ bool */
VFUNC_FUNC_3BUF(land, bool, bool, 16, vland, tland)

/*************************************************************************
 * Logical OR
 *************************************************************************/

/* C integer */
VFUNC_FUNC_3BUF(lor,   int8_t,   int8_t, 16, vlor, tlor)
VFUNC_FUNC_3BUF(lor,  uint8_t,  uint8_t, 16, vlor, tlor)
VFUNC_FUNC_3BUF(lor,  int16_t,  int16_t, 8, vlor, tlor)
VFUNC_FUNC_3BUF(lor, uint16_t, uint16_t, 8, vlor, tlor)
VFUNC_FUNC_3BUF(lor,  int32_t,  int32_t, 4, vlor, tlor)
VFUNC_FUNC_3BUF(lor, uint32_t, uint32_t, 4, vlor, tlor)
VFUNC_FUNC_3BUF(lor,  int64_t,  int64_t, 2, vlor, tlor)
VFUNC_FUNC_3BUF(lor, uint64_t, uint64_t, 2, vlor, tlor)
VFUNC_FUNC_3BUF(lor,  long,  long, 2, vlor, tlor)
VFUNC_FUNC_3BUF(lor, ulong, unsigned long, 2, vlor, tlor)

/* C++ bool */
VFUNC_FUNC_3BUF(lor, bool, bool, 16, vlor, tlor)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

/* C integer */
VFUNC_FUNC_3BUF(lxor,   int8_t,   int8_t, 16, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor,  uint8_t,  uint8_t, 16, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor,  int16_t,  int16_t, 8, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor, uint16_t, uint16_t, 8, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor,  int32_t,  int32_t, 4, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor, uint32_t, uint32_t, 4, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor,  int64_t,  int64_t, 2, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor, uint64_t, uint64_t, 2, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor,  long,  long, 2, vlxor, tlxor)
VFUNC_FUNC_3BUF(lxor, ulong, unsigned long, 2, vlxor, tlxor)

/* C++ bool */
VFUNC_FUNC_3BUF(lxor, bool, bool, 16, vlxor, tlxor)


/*************************************************************************
 * Bitwise AND
 *************************************************************************/

/* C integer */
VFUNC_FUNC_3BUF(band,   int8_t,   int8_t, 16, vband, tband)
VFUNC_FUNC_3BUF(band,  uint8_t,  uint8_t, 16, vband, tband)
VFUNC_FUNC_3BUF(band,  int16_t,  int16_t, 8, vband, tband)
VFUNC_FUNC_3BUF(band, uint16_t, uint16_t, 8, vband, tband)
VFUNC_FUNC_3BUF(band,  int32_t,  int32_t, 4, vband, tband)
VFUNC_FUNC_3BUF(band, uint32_t, uint32_t, 4, vband, tband)
VFUNC_FUNC_3BUF(band,  int64_t,  int64_t, 2, vband, tband)
VFUNC_FUNC_3BUF(band, uint64_t, uint64_t, 2, vband, tband)
VFUNC_FUNC_3BUF(band,  long,  long, 2, vband, tband)
VFUNC_FUNC_3BUF(band, ulong, unsigned long, 2, vband, tband)

/* C++ byte */
VFUNC_FUNC_3BUF(band, byte, char, 16, vband, tband)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

/* C integer */
VFUNC_FUNC_3BUF(bor,   int8_t,   int8_t, 16, vbor, tbor)
VFUNC_FUNC_3BUF(bor,  uint8_t,  uint8_t, 16, vbor, tbor)
VFUNC_FUNC_3BUF(bor,  int16_t,  int16_t, 8, vbor, tbor)
VFUNC_FUNC_3BUF(bor, uint16_t, uint16_t, 8, vbor, tbor)
VFUNC_FUNC_3BUF(bor,  int32_t,  int32_t, 4, vbor, tbor)
VFUNC_FUNC_3BUF(bor, uint32_t, uint32_t, 4, vbor, tbor)
VFUNC_FUNC_3BUF(bor,  int64_t,  int64_t, 2, vbor, tbor)
VFUNC_FUNC_3BUF(bor, uint64_t, uint64_t, 2, vbor, tbor)
VFUNC_FUNC_3BUF(bor,  long,  long, 2, vbor, tbor)
VFUNC_FUNC_3BUF(bor, ulong, unsigned long, 2, vbor, tbor)

/* C++ byte */
VFUNC_FUNC_3BUF(bor, byte, char, 16, vbor, tbor)

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

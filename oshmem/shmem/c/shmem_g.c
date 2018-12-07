/*
 * Copyright (c) 2013-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/spml/spml.h"

/*
 * These routines provide a low latency mechanism to retrieve basic types (short, int, float,
 * double, long) from symmetric data objects on remote PEs.
 * Retrieves the value at the symmetric address addr of the remote PE pe.
 */
#define DO_SHMEM_TYPE_G(ctx, type, addr, pe, out_value) do {        \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(addr);                                   \
                                                                    \
        size = sizeof(out_value);                                   \
        rc = MCA_SPML_CALL(get(                                     \
            ctx,                                                    \
            (void*)addr,                                            \
            size,                                                   \
            (void*)&out_value,                                      \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_G(type_name, type, prefix)                   \
    type prefix##_ctx##type_name##_g(shmem_ctx_t ctx, const type *addr, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_G(ctx, type, addr, pe, out_value);            \
        return out_value;                                           \
    }

#define SHMEM_TYPE_G(type_name, type, prefix)                       \
    type prefix##type_name##_g(const type *addr, int pe)            \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_G(oshmem_ctx_default, type, addr, pe, out_value); \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_g       = pshmem_ctx_char_g
#pragma weak shmem_ctx_short_g      = pshmem_ctx_short_g
#pragma weak shmem_ctx_int_g        = pshmem_ctx_int_g
#pragma weak shmem_ctx_long_g       = pshmem_ctx_long_g
#pragma weak shmem_ctx_float_g      = pshmem_ctx_float_g
#pragma weak shmem_ctx_double_g     = pshmem_ctx_double_g
#pragma weak shmem_ctx_longlong_g   = pshmem_ctx_longlong_g
#pragma weak shmem_ctx_schar_g      = pshmem_ctx_schar_g
#pragma weak shmem_ctx_uchar_g      = pshmem_ctx_uchar_g
#pragma weak shmem_ctx_ushort_g     = pshmem_ctx_ushort_g
#pragma weak shmem_ctx_uint_g       = pshmem_ctx_uint_g
#pragma weak shmem_ctx_ulong_g      = pshmem_ctx_ulong_g
#pragma weak shmem_ctx_ulonglong_g  = pshmem_ctx_ulonglong_g
#pragma weak shmem_ctx_longdouble_g = pshmem_ctx_longdouble_g
#pragma weak shmem_ctx_int8_g       = pshmem_ctx_int8_g
#pragma weak shmem_ctx_int16_g      = pshmem_ctx_int16_g
#pragma weak shmem_ctx_int32_g      = pshmem_ctx_int32_g
#pragma weak shmem_ctx_int64_g      = pshmem_ctx_int64_g
#pragma weak shmem_ctx_uint8_g      = pshmem_ctx_uint8_g
#pragma weak shmem_ctx_uint16_g     = pshmem_ctx_uint16_g
#pragma weak shmem_ctx_uint32_g     = pshmem_ctx_uint32_g
#pragma weak shmem_ctx_uint64_g     = pshmem_ctx_uint64_g
#pragma weak shmem_ctx_size_g       = pshmem_ctx_size_g
#pragma weak shmem_ctx_ptrdiff_g    = pshmem_ctx_ptrdiff_g

#pragma weak shmem_char_g           = pshmem_char_g
#pragma weak shmem_short_g          = pshmem_short_g
#pragma weak shmem_int_g            = pshmem_int_g
#pragma weak shmem_long_g           = pshmem_long_g
#pragma weak shmem_float_g          = pshmem_float_g
#pragma weak shmem_double_g         = pshmem_double_g
#pragma weak shmem_longlong_g       = pshmem_longlong_g
#pragma weak shmem_schar_g          = pshmem_schar_g
#pragma weak shmem_uchar_g          = pshmem_uchar_g
#pragma weak shmem_ushort_g         = pshmem_ushort_g
#pragma weak shmem_uint_g           = pshmem_uint_g
#pragma weak shmem_ulong_g          = pshmem_ulong_g
#pragma weak shmem_ulonglong_g      = pshmem_ulonglong_g
#pragma weak shmem_longdouble_g     = pshmem_longdouble_g
#pragma weak shmem_int8_g           = pshmem_int8_g
#pragma weak shmem_int16_g          = pshmem_int16_g
#pragma weak shmem_int32_g          = pshmem_int32_g
#pragma weak shmem_int64_g          = pshmem_int64_g
#pragma weak shmem_uint8_g          = pshmem_uint8_g
#pragma weak shmem_uint16_g         = pshmem_uint16_g
#pragma weak shmem_uint32_g         = pshmem_uint32_g
#pragma weak shmem_uint64_g         = pshmem_uint64_g
#pragma weak shmem_size_g           = pshmem_size_g
#pragma weak shmem_ptrdiff_g        = pshmem_ptrdiff_g

#pragma weak shmemx_int16_g = pshmemx_int16_g
#pragma weak shmemx_int32_g = pshmemx_int32_g
#pragma weak shmemx_int64_g = pshmemx_int64_g
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_G(_char, char, shmem)
SHMEM_CTX_TYPE_G(_short, short, shmem)
SHMEM_CTX_TYPE_G(_int, int, shmem)
SHMEM_CTX_TYPE_G(_long, long, shmem)
SHMEM_CTX_TYPE_G(_longlong, long long, shmem)
SHMEM_CTX_TYPE_G(_schar, signed char, shmem)
SHMEM_CTX_TYPE_G(_uchar, unsigned char, shmem)
SHMEM_CTX_TYPE_G(_ushort, unsigned short, shmem)
SHMEM_CTX_TYPE_G(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_G(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_G(_ulonglong, unsigned long long, shmem)
SHMEM_CTX_TYPE_G(_float, float, shmem)
SHMEM_CTX_TYPE_G(_double, double, shmem)
SHMEM_CTX_TYPE_G(_longdouble, long double, shmem)
SHMEM_CTX_TYPE_G(_int8, int8_t, shmem)
SHMEM_CTX_TYPE_G(_int16, int16_t, shmem)
SHMEM_CTX_TYPE_G(_int32, int32_t, shmem)
SHMEM_CTX_TYPE_G(_int64, int64_t, shmem)
SHMEM_CTX_TYPE_G(_uint8, uint8_t, shmem)
SHMEM_CTX_TYPE_G(_uint16, uint16_t, shmem)
SHMEM_CTX_TYPE_G(_uint32, uint32_t, shmem)
SHMEM_CTX_TYPE_G(_uint64, uint64_t, shmem)
SHMEM_CTX_TYPE_G(_size, size_t, shmem)
SHMEM_CTX_TYPE_G(_ptrdiff, ptrdiff_t, shmem)

SHMEM_TYPE_G(_char, char, shmem)
SHMEM_TYPE_G(_short, short, shmem)
SHMEM_TYPE_G(_int, int, shmem)
SHMEM_TYPE_G(_long, long, shmem)
SHMEM_TYPE_G(_longlong, long long, shmem)
SHMEM_TYPE_G(_schar, signed char, shmem)
SHMEM_TYPE_G(_uchar, unsigned char, shmem)
SHMEM_TYPE_G(_ushort, unsigned short, shmem)
SHMEM_TYPE_G(_uint, unsigned int, shmem)
SHMEM_TYPE_G(_ulong, unsigned long, shmem)
SHMEM_TYPE_G(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_G(_float, float, shmem)
SHMEM_TYPE_G(_double, double, shmem)
SHMEM_TYPE_G(_longdouble, long double, shmem)
SHMEM_TYPE_G(_int8, int8_t, shmem)
SHMEM_TYPE_G(_int16, int16_t, shmem)
SHMEM_TYPE_G(_int32, int32_t, shmem)
SHMEM_TYPE_G(_int64, int64_t, shmem)
SHMEM_TYPE_G(_uint8, uint8_t, shmem)
SHMEM_TYPE_G(_uint16, uint16_t, shmem)
SHMEM_TYPE_G(_uint32, uint32_t, shmem)
SHMEM_TYPE_G(_uint64, uint64_t, shmem)
SHMEM_TYPE_G(_size, size_t, shmem)
SHMEM_TYPE_G(_ptrdiff, ptrdiff_t, shmem)

SHMEM_TYPE_G(_int16, int16_t, shmemx)
SHMEM_TYPE_G(_int32, int32_t, shmemx)
SHMEM_TYPE_G(_int64, int64_t, shmemx)

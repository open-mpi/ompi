/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
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

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/spml/spml.h"

/*
 * These routines copy contiguous data from a local object to an object on the destination PE.
 * These routines transfer nelems elements of the data object at address source on the calling
 * PE, to the data object at address target on the remote PE pe. These routines start the
 * remote transfer and may return before the data is delivered to the remote PE. The delivery
 * of data into the data object on the destination PE from different put calls may occur in any
 * order. Because of this, two successive put operations may deliver data out of order unless a
 * call to shmem_fence() is introduced between the two calls.
 */
#define DO_SHMEM_TYPE_PUT(ctx, type, target, source, len, pe) do {  \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = len * sizeof(type);                                  \
        rc = MCA_SPML_CALL(put(                                     \
            ctx,                                                    \
            (void*)target,                                          \
            size,                                                   \
            (void*)source,                                          \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUT(type_name, type)                         \
    void shmem_ctx##type_name##_put(shmem_ctx_t ctx, type *target, const type *source, size_t len, int pe)\
    {                                                               \
        DO_SHMEM_TYPE_PUT(ctx, type, target, source, len, pe);      \
        return ;                                                    \
    }

#define SHMEM_TYPE_PUT(type_name, type)                             \
    void shmem##type_name##_put(type *target, const type *source, size_t len, int pe)\
    {                                                               \
        DO_SHMEM_TYPE_PUT(oshmem_ctx_default, type, target,          \
                          source, len, pe);                         \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_put       = pshmem_ctx_char_put
#pragma weak shmem_ctx_short_put      = pshmem_ctx_short_put
#pragma weak shmem_ctx_int_put        = pshmem_ctx_int_put
#pragma weak shmem_ctx_long_put       = pshmem_ctx_long_put
#pragma weak shmem_ctx_float_put      = pshmem_ctx_float_put
#pragma weak shmem_ctx_double_put     = pshmem_ctx_double_put
#pragma weak shmem_ctx_longlong_put   = pshmem_ctx_longlong_put
#pragma weak shmem_ctx_schar_put      = pshmem_ctx_schar_put
#pragma weak shmem_ctx_uchar_put      = pshmem_ctx_uchar_put
#pragma weak shmem_ctx_ushort_put     = pshmem_ctx_ushort_put
#pragma weak shmem_ctx_uint_put       = pshmem_ctx_uint_put
#pragma weak shmem_ctx_ulong_put      = pshmem_ctx_ulong_put
#pragma weak shmem_ctx_ulonglong_put  = pshmem_ctx_ulonglong_put
#pragma weak shmem_ctx_longdouble_put = pshmem_ctx_longdouble_put
#pragma weak shmem_ctx_int8_put       = pshmem_ctx_int8_put
#pragma weak shmem_ctx_int16_put      = pshmem_ctx_int16_put
#pragma weak shmem_ctx_int32_put      = pshmem_ctx_int32_put
#pragma weak shmem_ctx_int64_put      = pshmem_ctx_int64_put
#pragma weak shmem_ctx_uint8_put      = pshmem_ctx_uint8_put
#pragma weak shmem_ctx_uint16_put     = pshmem_ctx_uint16_put
#pragma weak shmem_ctx_uint32_put     = pshmem_ctx_uint32_put
#pragma weak shmem_ctx_uint64_put     = pshmem_ctx_uint64_put
#pragma weak shmem_ctx_size_put       = pshmem_ctx_size_put
#pragma weak shmem_ctx_ptrdiff_put    = pshmem_ctx_ptrdiff_put

#pragma weak shmem_char_put           = pshmem_char_put
#pragma weak shmem_short_put          = pshmem_short_put
#pragma weak shmem_int_put            = pshmem_int_put
#pragma weak shmem_long_put           = pshmem_long_put
#pragma weak shmem_float_put          = pshmem_float_put
#pragma weak shmem_double_put         = pshmem_double_put
#pragma weak shmem_longlong_put       = pshmem_longlong_put
#pragma weak shmem_schar_put          = pshmem_schar_put
#pragma weak shmem_uchar_put          = pshmem_uchar_put
#pragma weak shmem_ushort_put         = pshmem_ushort_put
#pragma weak shmem_uint_put           = pshmem_uint_put
#pragma weak shmem_ulong_put          = pshmem_ulong_put
#pragma weak shmem_ulonglong_put      = pshmem_ulonglong_put
#pragma weak shmem_longdouble_put     = pshmem_longdouble_put
#pragma weak shmem_int8_put           = pshmem_int8_put
#pragma weak shmem_int16_put          = pshmem_int16_put
#pragma weak shmem_int32_put          = pshmem_int32_put
#pragma weak shmem_int64_put          = pshmem_int64_put
#pragma weak shmem_uint8_put          = pshmem_uint8_put
#pragma weak shmem_uint16_put         = pshmem_uint16_put
#pragma weak shmem_uint32_put         = pshmem_uint32_put
#pragma weak shmem_uint64_put         = pshmem_uint64_put
#pragma weak shmem_size_put           = pshmem_size_put
#pragma weak shmem_ptrdiff_put        = pshmem_ptrdiff_put

#pragma weak shmem_ctx_putmem = pshmem_ctx_putmem
#pragma weak shmem_ctx_put8 = pshmem_ctx_put8
#pragma weak shmem_ctx_put16 = pshmem_ctx_put16
#pragma weak shmem_ctx_put32 = pshmem_ctx_put32
#pragma weak shmem_ctx_put64 = pshmem_ctx_put64
#pragma weak shmem_ctx_put128 = pshmem_ctx_put128

#pragma weak shmem_putmem = pshmem_putmem
#pragma weak shmem_put8 = pshmem_put8
#pragma weak shmem_put16 = pshmem_put16
#pragma weak shmem_put32 = pshmem_put32
#pragma weak shmem_put64 = pshmem_put64
#pragma weak shmem_put128 = pshmem_put128
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_PUT(_char, char)
SHMEM_CTX_TYPE_PUT(_short, short)
SHMEM_CTX_TYPE_PUT(_int, int)
SHMEM_CTX_TYPE_PUT(_long, long)
SHMEM_CTX_TYPE_PUT(_longlong, long long)
SHMEM_CTX_TYPE_PUT(_schar, signed char)
SHMEM_CTX_TYPE_PUT(_uchar, unsigned char)
SHMEM_CTX_TYPE_PUT(_ushort, unsigned short)
SHMEM_CTX_TYPE_PUT(_uint, unsigned int)
SHMEM_CTX_TYPE_PUT(_ulong, unsigned long)
SHMEM_CTX_TYPE_PUT(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_PUT(_float, float)
SHMEM_CTX_TYPE_PUT(_double, double)
SHMEM_CTX_TYPE_PUT(_longdouble, long double)
SHMEM_CTX_TYPE_PUT(_int8, int8_t)
SHMEM_CTX_TYPE_PUT(_int16, int16_t)
SHMEM_CTX_TYPE_PUT(_int32, int32_t)
SHMEM_CTX_TYPE_PUT(_int64, int64_t)
SHMEM_CTX_TYPE_PUT(_uint8, uint8_t)
SHMEM_CTX_TYPE_PUT(_uint16, uint16_t)
SHMEM_CTX_TYPE_PUT(_uint32, uint32_t)
SHMEM_CTX_TYPE_PUT(_uint64, uint64_t)
SHMEM_CTX_TYPE_PUT(_size, size_t)
SHMEM_CTX_TYPE_PUT(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_PUT(_char, char)
SHMEM_TYPE_PUT(_short, short)
SHMEM_TYPE_PUT(_int, int)
SHMEM_TYPE_PUT(_long, long)
SHMEM_TYPE_PUT(_longlong, long long)
SHMEM_TYPE_PUT(_schar, signed char)
SHMEM_TYPE_PUT(_uchar, unsigned char)
SHMEM_TYPE_PUT(_ushort, unsigned short)
SHMEM_TYPE_PUT(_uint, unsigned int)
SHMEM_TYPE_PUT(_ulong, unsigned long)
SHMEM_TYPE_PUT(_ulonglong, unsigned long long)
SHMEM_TYPE_PUT(_float, float)
SHMEM_TYPE_PUT(_double, double)
SHMEM_TYPE_PUT(_longdouble, long double)
SHMEM_TYPE_PUT(_int8, int8_t)
SHMEM_TYPE_PUT(_int16, int16_t)
SHMEM_TYPE_PUT(_int32, int32_t)
SHMEM_TYPE_PUT(_int64, int64_t)
SHMEM_TYPE_PUT(_uint8, uint8_t)
SHMEM_TYPE_PUT(_uint16, uint16_t)
SHMEM_TYPE_PUT(_uint32, uint32_t)
SHMEM_TYPE_PUT(_uint64, uint64_t)
SHMEM_TYPE_PUT(_size, size_t)
SHMEM_TYPE_PUT(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_PUTMEM(ctx, target, source, element_size, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(put(                                     \
            ctx,                                                    \
            (void*)target,                                          \
            size,                                                   \
            (void*)source,                                          \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUTMEM(name, element_size, prefix)           \
    void prefix##_ctx##name(shmem_ctx_t ctx, void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_PUTMEM(ctx, target, source,                        \
                        element_size, nelems, pe);                  \
        return ;                                                    \
    }

#define SHMEM_TYPE_PUTMEM(name, element_size, prefix)               \
    void prefix##name(void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_PUTMEM(oshmem_ctx_default, target,                  \
                        source, element_size, nelems, pe);          \
        return ;                                                    \
    }

SHMEM_CTX_TYPE_PUTMEM(_putmem, 1, shmem)
SHMEM_CTX_TYPE_PUTMEM(_put8,  1, shmem)
SHMEM_CTX_TYPE_PUTMEM(_put16, 2, shmem)
SHMEM_CTX_TYPE_PUTMEM(_put32, 4, shmem)
SHMEM_CTX_TYPE_PUTMEM(_put64, 8, shmem)
SHMEM_CTX_TYPE_PUTMEM(_put128, 16, shmem)
SHMEM_TYPE_PUTMEM(_putmem, 1, shmem)
SHMEM_TYPE_PUTMEM(_put8,  1, shmem)
SHMEM_TYPE_PUTMEM(_put16, 2, shmem)
SHMEM_TYPE_PUTMEM(_put32, 4, shmem)
SHMEM_TYPE_PUTMEM(_put64, 8, shmem)
SHMEM_TYPE_PUTMEM(_put128, 16, shmem)


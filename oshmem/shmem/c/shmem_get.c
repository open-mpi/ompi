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
 * These routines retrieve data from a contiguous data object on a remote PE.
 * The shmem_get() routines transfer nelems elements of the data object at address source
 * on the remote PE (pe), to the data object at address target on the local PE. These routines
 * return after the data has been copied to address target on the local pe.
 */
#define DO_SHMEM_TYPE_GET(ctx, type, target, source, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * sizeof(type);                               \
        rc = MCA_SPML_CALL(get(                                     \
            ctx,                                                    \
            (void*)source,                                          \
            size,                                                   \
            (void*)target,                                          \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_GET(type_name, type)                         \
    void shmem_ctx##type_name##_get(shmem_ctx_t ctx, type *target, const type *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_GET(ctx, type, target, source, nelems, pe);   \
        return ;                                                    \
    }

#define SHMEM_TYPE_GET(type_name, type)                             \
    void shmem##type_name##_get(type *target, const type *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_GET(oshmem_ctx_default, type, target, source,  \
                          nelems, pe);                              \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_get       = pshmem_ctx_char_get
#pragma weak shmem_ctx_short_get      = pshmem_ctx_short_get
#pragma weak shmem_ctx_int_get        = pshmem_ctx_int_get
#pragma weak shmem_ctx_long_get       = pshmem_ctx_long_get
#pragma weak shmem_ctx_float_get      = pshmem_ctx_float_get
#pragma weak shmem_ctx_double_get     = pshmem_ctx_double_get
#pragma weak shmem_ctx_longlong_get   = pshmem_ctx_longlong_get
#pragma weak shmem_ctx_schar_get      = pshmem_ctx_schar_get
#pragma weak shmem_ctx_uchar_get      = pshmem_ctx_uchar_get
#pragma weak shmem_ctx_ushort_get     = pshmem_ctx_ushort_get
#pragma weak shmem_ctx_uint_get       = pshmem_ctx_uint_get
#pragma weak shmem_ctx_ulong_get      = pshmem_ctx_ulong_get
#pragma weak shmem_ctx_ulonglong_get  = pshmem_ctx_ulonglong_get
#pragma weak shmem_ctx_longdouble_get = pshmem_ctx_longdouble_get
#pragma weak shmem_ctx_int8_get       = pshmem_ctx_int8_get
#pragma weak shmem_ctx_int16_get      = pshmem_ctx_int16_get
#pragma weak shmem_ctx_int32_get      = pshmem_ctx_int32_get
#pragma weak shmem_ctx_int64_get      = pshmem_ctx_int64_get
#pragma weak shmem_ctx_uint8_get      = pshmem_ctx_uint8_get
#pragma weak shmem_ctx_uint16_get     = pshmem_ctx_uint16_get
#pragma weak shmem_ctx_uint32_get     = pshmem_ctx_uint32_get
#pragma weak shmem_ctx_uint64_get     = pshmem_ctx_uint64_get
#pragma weak shmem_ctx_size_get       = pshmem_ctx_size_get
#pragma weak shmem_ctx_ptrdiff_get    = pshmem_ctx_ptrdiff_get

#pragma weak shmem_char_get           = pshmem_char_get
#pragma weak shmem_short_get          = pshmem_short_get
#pragma weak shmem_int_get            = pshmem_int_get
#pragma weak shmem_long_get           = pshmem_long_get
#pragma weak shmem_float_get          = pshmem_float_get
#pragma weak shmem_double_get         = pshmem_double_get
#pragma weak shmem_longlong_get       = pshmem_longlong_get
#pragma weak shmem_schar_get          = pshmem_schar_get
#pragma weak shmem_uchar_get          = pshmem_uchar_get
#pragma weak shmem_ushort_get         = pshmem_ushort_get
#pragma weak shmem_uint_get           = pshmem_uint_get
#pragma weak shmem_ulong_get          = pshmem_ulong_get
#pragma weak shmem_ulonglong_get      = pshmem_ulonglong_get
#pragma weak shmem_longdouble_get     = pshmem_longdouble_get
#pragma weak shmem_int8_get           = pshmem_int8_get
#pragma weak shmem_int16_get          = pshmem_int16_get
#pragma weak shmem_int32_get          = pshmem_int32_get
#pragma weak shmem_int64_get          = pshmem_int64_get
#pragma weak shmem_uint8_get          = pshmem_uint8_get
#pragma weak shmem_uint16_get         = pshmem_uint16_get
#pragma weak shmem_uint32_get         = pshmem_uint32_get
#pragma weak shmem_uint64_get         = pshmem_uint64_get
#pragma weak shmem_size_get           = pshmem_size_get
#pragma weak shmem_ptrdiff_get        = pshmem_ptrdiff_get

#pragma weak shmem_ctx_getmem = pshmem_ctx_getmem
#pragma weak shmem_ctx_get8 = pshmem_ctx_get8
#pragma weak shmem_ctx_get16 = pshmem_ctx_get16
#pragma weak shmem_ctx_get32 = pshmem_ctx_get32
#pragma weak shmem_ctx_get64 = pshmem_ctx_get64
#pragma weak shmem_ctx_get128 = pshmem_ctx_get128

#pragma weak shmem_getmem = pshmem_getmem
#pragma weak shmem_get8 = pshmem_get8
#pragma weak shmem_get16 = pshmem_get16
#pragma weak shmem_get32 = pshmem_get32
#pragma weak shmem_get64 = pshmem_get64
#pragma weak shmem_get128 = pshmem_get128
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_GET(_char, char)
SHMEM_CTX_TYPE_GET(_short, short)
SHMEM_CTX_TYPE_GET(_int, int)
SHMEM_CTX_TYPE_GET(_long, long)
SHMEM_CTX_TYPE_GET(_longlong, long long)
SHMEM_CTX_TYPE_GET(_schar, signed char)
SHMEM_CTX_TYPE_GET(_uchar, unsigned char)
SHMEM_CTX_TYPE_GET(_ushort, unsigned short)
SHMEM_CTX_TYPE_GET(_uint, unsigned int)
SHMEM_CTX_TYPE_GET(_ulong, unsigned long)
SHMEM_CTX_TYPE_GET(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_GET(_float, float)
SHMEM_CTX_TYPE_GET(_double, double)
SHMEM_CTX_TYPE_GET(_longdouble, long double)
SHMEM_CTX_TYPE_GET(_int8, int8_t)
SHMEM_CTX_TYPE_GET(_int16, int16_t)
SHMEM_CTX_TYPE_GET(_int32, int32_t)
SHMEM_CTX_TYPE_GET(_int64, int64_t)
SHMEM_CTX_TYPE_GET(_uint8, uint8_t)
SHMEM_CTX_TYPE_GET(_uint16, uint16_t)
SHMEM_CTX_TYPE_GET(_uint32, uint32_t)
SHMEM_CTX_TYPE_GET(_uint64, uint64_t)
SHMEM_CTX_TYPE_GET(_size, size_t)
SHMEM_CTX_TYPE_GET(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_GET(_char, char)
SHMEM_TYPE_GET(_short, short)
SHMEM_TYPE_GET(_int, int)
SHMEM_TYPE_GET(_long, long)
SHMEM_TYPE_GET(_longlong, long long)
SHMEM_TYPE_GET(_schar, signed char)
SHMEM_TYPE_GET(_uchar, unsigned char)
SHMEM_TYPE_GET(_ushort, unsigned short)
SHMEM_TYPE_GET(_uint, unsigned int)
SHMEM_TYPE_GET(_ulong, unsigned long)
SHMEM_TYPE_GET(_ulonglong, unsigned long long)
SHMEM_TYPE_GET(_float, float)
SHMEM_TYPE_GET(_double, double)
SHMEM_TYPE_GET(_longdouble, long double)
SHMEM_TYPE_GET(_int8, int8_t)
SHMEM_TYPE_GET(_int16, int16_t)
SHMEM_TYPE_GET(_int32, int32_t)
SHMEM_TYPE_GET(_int64, int64_t)
SHMEM_TYPE_GET(_uint8, uint8_t)
SHMEM_TYPE_GET(_uint16, uint16_t)
SHMEM_TYPE_GET(_uint32, uint32_t)
SHMEM_TYPE_GET(_uint64, uint64_t)
SHMEM_TYPE_GET(_size, size_t)
SHMEM_TYPE_GET(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_GETMEM(ctx, target, source, element_size, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(get(                                     \
            ctx,                                                    \
            (void*)source,                                          \
            size,                                                   \
            (void*)target,                                          \
            pe));                                                   \
       RUNTIME_CHECK_RC(rc);                                        \
    } while (0)

#define SHMEM_CTX_TYPE_GETMEM(name, element_size, prefix)           \
    void prefix##_ctx##name(shmem_ctx_t ctx, void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_GETMEM(ctx, target, source, element_size, nelems, pe); \
        return ;                                                    \
    }

#define SHMEM_TYPE_GETMEM(name, element_size, prefix)               \
    void prefix##name(void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_GETMEM(oshmem_ctx_default, target, source,          \
                        element_size, nelems, pe);                  \
        return ;                                                    \
    }

SHMEM_CTX_TYPE_GETMEM(_getmem, 1, shmem)
SHMEM_CTX_TYPE_GETMEM(_get8, 1, shmem)
SHMEM_CTX_TYPE_GETMEM(_get16, 2, shmem)
SHMEM_CTX_TYPE_GETMEM(_get32, 4, shmem)
SHMEM_CTX_TYPE_GETMEM(_get64, 8, shmem)
SHMEM_CTX_TYPE_GETMEM(_get128, 16, shmem)
SHMEM_TYPE_GETMEM(_getmem, 1, shmem)
SHMEM_TYPE_GETMEM(_get8, 1, shmem)
SHMEM_TYPE_GETMEM(_get16, 2, shmem)
SHMEM_TYPE_GETMEM(_get32, 4, shmem)
SHMEM_TYPE_GETMEM(_get64, 8, shmem)
SHMEM_TYPE_GETMEM(_get128, 16, shmem)


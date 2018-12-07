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
 * The strided get routines copy strided data located on a remote PE to a local strided data object.
 * The strided get routines retrieve array data available at address source on remote PE (pe).
 * The elements of the source array are separated by a stride sst. Once the data is received,
 * it is stored at the local memory address target, separated by stride tst. The routines return
 * when the data has been copied into the local target array.
 */
#define DO_SHMEM_TYPE_IGET(ctx, type, target, source, tst, sst, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t element_size = 0;                                    \
        size_t i = 0;                                               \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        element_size = sizeof(type);                                \
        for (i = 0; i < nelems; i++)                                \
        {                                                           \
            rc = MCA_SPML_CALL(get(                                 \
                ctx,                                                \
                (void*)(source + i * sst),                          \
                element_size,                                       \
                (void*)(target + i * tst),                          \
                pe));                                               \
        }                                                           \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_IGET(type_name, type)                        \
    void shmem_ctx##type_name##_iget(shmem_ctx_t ctx, type *target, const type *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_IGET(ctx, type, target, source, tst, sst, nelems, pe); \
        return ;                                                    \
    }

#define SHMEM_TYPE_IGET(type_name, type)                            \
    void shmem##type_name##_iget(type *target, const type *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_IGET(oshmem_ctx_default, type, target, source, \
                           tst, sst, nelems, pe);                   \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_iget       = pshmem_ctx_char_iget
#pragma weak shmem_ctx_short_iget      = pshmem_ctx_short_iget
#pragma weak shmem_ctx_int_iget        = pshmem_ctx_int_iget
#pragma weak shmem_ctx_long_iget       = pshmem_ctx_long_iget
#pragma weak shmem_ctx_float_iget      = pshmem_ctx_float_iget
#pragma weak shmem_ctx_double_iget     = pshmem_ctx_double_iget
#pragma weak shmem_ctx_longlong_iget   = pshmem_ctx_longlong_iget
#pragma weak shmem_ctx_schar_iget      = pshmem_ctx_schar_iget
#pragma weak shmem_ctx_uchar_iget      = pshmem_ctx_uchar_iget
#pragma weak shmem_ctx_ushort_iget     = pshmem_ctx_ushort_iget
#pragma weak shmem_ctx_uint_iget       = pshmem_ctx_uint_iget
#pragma weak shmem_ctx_ulong_iget      = pshmem_ctx_ulong_iget
#pragma weak shmem_ctx_ulonglong_iget  = pshmem_ctx_ulonglong_iget
#pragma weak shmem_ctx_longdouble_iget = pshmem_ctx_longdouble_iget
#pragma weak shmem_ctx_int8_iget       = pshmem_ctx_int8_iget
#pragma weak shmem_ctx_int16_iget      = pshmem_ctx_int16_iget
#pragma weak shmem_ctx_int32_iget      = pshmem_ctx_int32_iget
#pragma weak shmem_ctx_int64_iget      = pshmem_ctx_int64_iget
#pragma weak shmem_ctx_uint8_iget      = pshmem_ctx_uint8_iget
#pragma weak shmem_ctx_uint16_iget     = pshmem_ctx_uint16_iget
#pragma weak shmem_ctx_uint32_iget     = pshmem_ctx_uint32_iget
#pragma weak shmem_ctx_uint64_iget     = pshmem_ctx_uint64_iget
#pragma weak shmem_ctx_size_iget       = pshmem_ctx_size_iget
#pragma weak shmem_ctx_ptrdiff_iget    = pshmem_ctx_ptrdiff_iget

#pragma weak shmem_char_iget           = pshmem_char_iget
#pragma weak shmem_short_iget          = pshmem_short_iget
#pragma weak shmem_int_iget            = pshmem_int_iget
#pragma weak shmem_long_iget           = pshmem_long_iget
#pragma weak shmem_float_iget          = pshmem_float_iget
#pragma weak shmem_double_iget         = pshmem_double_iget
#pragma weak shmem_longlong_iget       = pshmem_longlong_iget
#pragma weak shmem_schar_iget          = pshmem_schar_iget
#pragma weak shmem_uchar_iget          = pshmem_uchar_iget
#pragma weak shmem_ushort_iget         = pshmem_ushort_iget
#pragma weak shmem_uint_iget           = pshmem_uint_iget
#pragma weak shmem_ulong_iget          = pshmem_ulong_iget
#pragma weak shmem_ulonglong_iget      = pshmem_ulonglong_iget
#pragma weak shmem_longdouble_iget     = pshmem_longdouble_iget
#pragma weak shmem_int8_iget           = pshmem_int8_iget
#pragma weak shmem_int16_iget          = pshmem_int16_iget
#pragma weak shmem_int32_iget          = pshmem_int32_iget
#pragma weak shmem_int64_iget          = pshmem_int64_iget
#pragma weak shmem_uint8_iget          = pshmem_uint8_iget
#pragma weak shmem_uint16_iget         = pshmem_uint16_iget
#pragma weak shmem_uint32_iget         = pshmem_uint32_iget
#pragma weak shmem_uint64_iget         = pshmem_uint64_iget
#pragma weak shmem_size_iget           = pshmem_size_iget
#pragma weak shmem_ptrdiff_iget        = pshmem_ptrdiff_iget

#pragma weak shmem_ctx_iget8 = pshmem_ctx_iget8
#pragma weak shmem_ctx_iget16 = pshmem_ctx_iget16
#pragma weak shmem_ctx_iget32 = pshmem_ctx_iget32
#pragma weak shmem_ctx_iget64 = pshmem_ctx_iget64
#pragma weak shmem_ctx_iget128 = pshmem_ctx_iget128

#pragma weak shmem_iget8 = pshmem_iget8
#pragma weak shmem_iget16 = pshmem_iget16
#pragma weak shmem_iget32 = pshmem_iget32
#pragma weak shmem_iget64 = pshmem_iget64
#pragma weak shmem_iget128 = pshmem_iget128
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_IGET(_char, char)
SHMEM_CTX_TYPE_IGET(_short, short)
SHMEM_CTX_TYPE_IGET(_int, int)
SHMEM_CTX_TYPE_IGET(_long, long)
SHMEM_CTX_TYPE_IGET(_longlong, long long)
SHMEM_CTX_TYPE_IGET(_schar, signed char)
SHMEM_CTX_TYPE_IGET(_uchar, unsigned char)
SHMEM_CTX_TYPE_IGET(_ushort, unsigned short)
SHMEM_CTX_TYPE_IGET(_uint, unsigned int)
SHMEM_CTX_TYPE_IGET(_ulong, unsigned long)
SHMEM_CTX_TYPE_IGET(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_IGET(_float, float)
SHMEM_CTX_TYPE_IGET(_double, double)
SHMEM_CTX_TYPE_IGET(_longdouble, long double)
SHMEM_CTX_TYPE_IGET(_int8, int8_t)
SHMEM_CTX_TYPE_IGET(_int16, int16_t)
SHMEM_CTX_TYPE_IGET(_int32, int32_t)
SHMEM_CTX_TYPE_IGET(_int64, int64_t)
SHMEM_CTX_TYPE_IGET(_uint8, uint8_t)
SHMEM_CTX_TYPE_IGET(_uint16, uint16_t)
SHMEM_CTX_TYPE_IGET(_uint32, uint32_t)
SHMEM_CTX_TYPE_IGET(_uint64, uint64_t)
SHMEM_CTX_TYPE_IGET(_size, size_t)
SHMEM_CTX_TYPE_IGET(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_IGET(_char, char)
SHMEM_TYPE_IGET(_short, short)
SHMEM_TYPE_IGET(_int, int)
SHMEM_TYPE_IGET(_long, long)
SHMEM_TYPE_IGET(_longlong, long long)
SHMEM_TYPE_IGET(_schar, signed char)
SHMEM_TYPE_IGET(_uchar, unsigned char)
SHMEM_TYPE_IGET(_ushort, unsigned short)
SHMEM_TYPE_IGET(_uint, unsigned int)
SHMEM_TYPE_IGET(_ulong, unsigned long)
SHMEM_TYPE_IGET(_ulonglong, unsigned long long)
SHMEM_TYPE_IGET(_float, float)
SHMEM_TYPE_IGET(_double, double)
SHMEM_TYPE_IGET(_longdouble, long double)
SHMEM_TYPE_IGET(_int8, int8_t)
SHMEM_TYPE_IGET(_int16, int16_t)
SHMEM_TYPE_IGET(_int32, int32_t)
SHMEM_TYPE_IGET(_int64, int64_t)
SHMEM_TYPE_IGET(_uint8, uint8_t)
SHMEM_TYPE_IGET(_uint16, uint16_t)
SHMEM_TYPE_IGET(_uint32, uint32_t)
SHMEM_TYPE_IGET(_uint64, uint64_t)
SHMEM_TYPE_IGET(_size, size_t)
SHMEM_TYPE_IGET(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_IGETMEM(ctx, target, source, tst, sst, element_size, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t i = 0;                                               \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        for (i = 0; i < nelems; i++)                                \
        {                                                           \
            rc = MCA_SPML_CALL(get(                                 \
                ctx,                                                \
                (void*)((char*)source + i * sst * element_size),    \
                element_size,                                       \
                (void*)((char*)target + i * tst * element_size),    \
                pe));                                               \
        }                                                           \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_IGETMEM(name, element_size, prefix)          \
    void prefix##_ctx##name(shmem_ctx_t ctx, void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_IGETMEM(ctx, target, source, tst, sst,             \
                         element_size, nelems, pe);                 \
        return ;                                                    \
    }

#define SHMEM_TYPE_IGETMEM(name, element_size, prefix)              \
    void prefix##name(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_IGETMEM(oshmem_ctx_default, target, source, tst, sst, \
                         element_size, nelems, pe);                 \
        return ;                                                    \
    }

SHMEM_CTX_TYPE_IGETMEM(_iget8, 1, shmem)
SHMEM_CTX_TYPE_IGETMEM(_iget16, 2, shmem)
SHMEM_CTX_TYPE_IGETMEM(_iget32, 4, shmem)
SHMEM_CTX_TYPE_IGETMEM(_iget64, 8, shmem)
SHMEM_CTX_TYPE_IGETMEM(_iget128, 16, shmem)
SHMEM_TYPE_IGETMEM(_iget8, 1, shmem)
SHMEM_TYPE_IGETMEM(_iget16, 2, shmem)
SHMEM_TYPE_IGETMEM(_iget32, 4, shmem)
SHMEM_TYPE_IGETMEM(_iget64, 8, shmem)
SHMEM_TYPE_IGETMEM(_iget128, 16, shmem)


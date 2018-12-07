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
 * These routines copy strided data from the local PE to a strided data object on the destination PE.
 * The shmem_iput() routines read the elements of a local array (source) and write them to
 * a remote array (target) on the PE indicated by pe. These routines return when the data has
 * been copied out of the source array on the local PE but not necessarily before the data has
 * been delivered to the remote data object.
 */
#define DO_SHMEM_TYPE_IPUT(ctx, type, target, source, tst, sst, nelemes, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t element_size = 0;                                    \
        size_t i = 0;                                               \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        element_size = sizeof(type);                                \
        for (i = 0; i < nelems; i++)                                \
        {                                                           \
            rc = MCA_SPML_CALL(put(                                 \
                ctx,                                                \
                (void*)(target + i * tst),                          \
                element_size,                                       \
                (void*)(source + i * sst),                          \
                pe));                                               \
        }                                                           \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_IPUT(type_name, type)                        \
    void shmem_ctx##type_name##_iput(shmem_ctx_t ctx, type *target, const type *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_IPUT(ctx, type, target, source, tst, sst,     \
                           nelems, pe);                             \
        return ;                                                    \
    }

#define SHMEM_TYPE_IPUT(type_name, type)                            \
    void shmem##type_name##_iput(type *target, const type *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_IPUT(oshmem_ctx_default, type, target, source, \
                           tst, sst, nelems, pe);                   \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_iput       = pshmem_ctx_char_iput
#pragma weak shmem_ctx_short_iput      = pshmem_ctx_short_iput
#pragma weak shmem_ctx_int_iput        = pshmem_ctx_int_iput
#pragma weak shmem_ctx_long_iput       = pshmem_ctx_long_iput
#pragma weak shmem_ctx_float_iput      = pshmem_ctx_float_iput
#pragma weak shmem_ctx_double_iput     = pshmem_ctx_double_iput
#pragma weak shmem_ctx_longlong_iput   = pshmem_ctx_longlong_iput
#pragma weak shmem_ctx_schar_iput      = pshmem_ctx_schar_iput
#pragma weak shmem_ctx_uchar_iput      = pshmem_ctx_uchar_iput
#pragma weak shmem_ctx_ushort_iput     = pshmem_ctx_ushort_iput
#pragma weak shmem_ctx_uint_iput       = pshmem_ctx_uint_iput
#pragma weak shmem_ctx_ulong_iput      = pshmem_ctx_ulong_iput
#pragma weak shmem_ctx_ulonglong_iput  = pshmem_ctx_ulonglong_iput
#pragma weak shmem_ctx_longdouble_iput = pshmem_ctx_longdouble_iput
#pragma weak shmem_ctx_int8_iput       = pshmem_ctx_int8_iput
#pragma weak shmem_ctx_int16_iput      = pshmem_ctx_int16_iput
#pragma weak shmem_ctx_int32_iput      = pshmem_ctx_int32_iput
#pragma weak shmem_ctx_int64_iput      = pshmem_ctx_int64_iput
#pragma weak shmem_ctx_uint8_iput      = pshmem_ctx_uint8_iput
#pragma weak shmem_ctx_uint16_iput     = pshmem_ctx_uint16_iput
#pragma weak shmem_ctx_uint32_iput     = pshmem_ctx_uint32_iput
#pragma weak shmem_ctx_uint64_iput     = pshmem_ctx_uint64_iput
#pragma weak shmem_ctx_size_iput       = pshmem_ctx_size_iput
#pragma weak shmem_ctx_ptrdiff_iput    = pshmem_ctx_ptrdiff_iput

#pragma weak shmem_char_iput           = pshmem_char_iput
#pragma weak shmem_short_iput          = pshmem_short_iput
#pragma weak shmem_int_iput            = pshmem_int_iput
#pragma weak shmem_long_iput           = pshmem_long_iput
#pragma weak shmem_float_iput          = pshmem_float_iput
#pragma weak shmem_double_iput         = pshmem_double_iput
#pragma weak shmem_longlong_iput       = pshmem_longlong_iput
#pragma weak shmem_schar_iput          = pshmem_schar_iput
#pragma weak shmem_uchar_iput          = pshmem_uchar_iput
#pragma weak shmem_ushort_iput         = pshmem_ushort_iput
#pragma weak shmem_uint_iput           = pshmem_uint_iput
#pragma weak shmem_ulong_iput          = pshmem_ulong_iput
#pragma weak shmem_ulonglong_iput      = pshmem_ulonglong_iput
#pragma weak shmem_longdouble_iput     = pshmem_longdouble_iput
#pragma weak shmem_int8_iput           = pshmem_int8_iput
#pragma weak shmem_int16_iput          = pshmem_int16_iput
#pragma weak shmem_int32_iput          = pshmem_int32_iput
#pragma weak shmem_int64_iput          = pshmem_int64_iput
#pragma weak shmem_uint8_iput          = pshmem_uint8_iput
#pragma weak shmem_uint16_iput         = pshmem_uint16_iput
#pragma weak shmem_uint32_iput         = pshmem_uint32_iput
#pragma weak shmem_uint64_iput         = pshmem_uint64_iput
#pragma weak shmem_size_iput           = pshmem_size_iput
#pragma weak shmem_ptrdiff_iput        = pshmem_ptrdiff_iput

#pragma weak shmem_ctx_iput8 = pshmem_ctx_iput8
#pragma weak shmem_ctx_iput16 = pshmem_ctx_iput16
#pragma weak shmem_ctx_iput32 = pshmem_ctx_iput32
#pragma weak shmem_ctx_iput64 = pshmem_ctx_iput64
#pragma weak shmem_ctx_iput128 = pshmem_ctx_iput128

#pragma weak shmem_iput8 = pshmem_iput8
#pragma weak shmem_iput16 = pshmem_iput16
#pragma weak shmem_iput32 = pshmem_iput32
#pragma weak shmem_iput64 = pshmem_iput64
#pragma weak shmem_iput128 = pshmem_iput128
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_IPUT(_char, char)
SHMEM_CTX_TYPE_IPUT(_short, short)
SHMEM_CTX_TYPE_IPUT(_int, int)
SHMEM_CTX_TYPE_IPUT(_long, long)
SHMEM_CTX_TYPE_IPUT(_longlong, long long)
SHMEM_CTX_TYPE_IPUT(_schar, signed char)
SHMEM_CTX_TYPE_IPUT(_uchar, unsigned char)
SHMEM_CTX_TYPE_IPUT(_ushort, unsigned short)
SHMEM_CTX_TYPE_IPUT(_uint, unsigned int)
SHMEM_CTX_TYPE_IPUT(_ulong, unsigned long)
SHMEM_CTX_TYPE_IPUT(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_IPUT(_float, float)
SHMEM_CTX_TYPE_IPUT(_double, double)
SHMEM_CTX_TYPE_IPUT(_longdouble, long double)
SHMEM_CTX_TYPE_IPUT(_int8, int8_t)
SHMEM_CTX_TYPE_IPUT(_int16, int16_t)
SHMEM_CTX_TYPE_IPUT(_int32, int32_t)
SHMEM_CTX_TYPE_IPUT(_int64, int64_t)
SHMEM_CTX_TYPE_IPUT(_uint8, uint8_t)
SHMEM_CTX_TYPE_IPUT(_uint16, uint16_t)
SHMEM_CTX_TYPE_IPUT(_uint32, uint32_t)
SHMEM_CTX_TYPE_IPUT(_uint64, uint64_t)
SHMEM_CTX_TYPE_IPUT(_size, size_t)
SHMEM_CTX_TYPE_IPUT(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_IPUT(_char, char)
SHMEM_TYPE_IPUT(_short, short)
SHMEM_TYPE_IPUT(_int, int)
SHMEM_TYPE_IPUT(_long, long)
SHMEM_TYPE_IPUT(_longlong, long long)
SHMEM_TYPE_IPUT(_schar, signed char)
SHMEM_TYPE_IPUT(_uchar, unsigned char)
SHMEM_TYPE_IPUT(_ushort, unsigned short)
SHMEM_TYPE_IPUT(_uint, unsigned int)
SHMEM_TYPE_IPUT(_ulong, unsigned long)
SHMEM_TYPE_IPUT(_ulonglong, unsigned long long)
SHMEM_TYPE_IPUT(_float, float)
SHMEM_TYPE_IPUT(_double, double)
SHMEM_TYPE_IPUT(_longdouble, long double)
SHMEM_TYPE_IPUT(_int8, int8_t)
SHMEM_TYPE_IPUT(_int16, int16_t)
SHMEM_TYPE_IPUT(_int32, int32_t)
SHMEM_TYPE_IPUT(_int64, int64_t)
SHMEM_TYPE_IPUT(_uint8, uint8_t)
SHMEM_TYPE_IPUT(_uint16, uint16_t)
SHMEM_TYPE_IPUT(_uint32, uint32_t)
SHMEM_TYPE_IPUT(_uint64, uint64_t)
SHMEM_TYPE_IPUT(_size, size_t)
SHMEM_TYPE_IPUT(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_IPUTMEM(ctx, target, source, tst, sst, element_size, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t i = 0;                                               \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        for (i = 0; i < nelems; i++)                                \
        {                                                           \
            rc = MCA_SPML_CALL(put(                                 \
                ctx,                                                \
                (void*)((char*)target + i * tst * element_size),    \
                element_size,                                       \
                (void*)((char*)source + i * sst * element_size),    \
                pe));                                               \
        }                                                           \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_IPUTMEM(name, element_size, prefix)          \
    void prefix##_ctx##name(shmem_ctx_t ctx, void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_IPUTMEM(ctx, target, source, tst, sst,             \
                         element_size, nelems, pe);                 \
        return ;                                                    \
    }

#define SHMEM_TYPE_IPUTMEM(name, element_size, prefix)              \
    void prefix##name(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_IPUTMEM(oshmem_ctx_default, target, source, tst,    \
                         sst, element_size, nelems, pe);            \
        return ;                                                    \
    }

SHMEM_CTX_TYPE_IPUTMEM(_iput8, 1, shmem)
SHMEM_CTX_TYPE_IPUTMEM(_iput16, 2, shmem)
SHMEM_CTX_TYPE_IPUTMEM(_iput32, 4, shmem)
SHMEM_CTX_TYPE_IPUTMEM(_iput64, 8, shmem)
SHMEM_CTX_TYPE_IPUTMEM(_iput128, 16, shmem)
SHMEM_TYPE_IPUTMEM(_iput8, 1, shmem)
SHMEM_TYPE_IPUTMEM(_iput16, 2, shmem)
SHMEM_TYPE_IPUTMEM(_iput32, 4, shmem)
SHMEM_TYPE_IPUTMEM(_iput64, 8, shmem)
SHMEM_TYPE_IPUTMEM(_iput128, 16, shmem)

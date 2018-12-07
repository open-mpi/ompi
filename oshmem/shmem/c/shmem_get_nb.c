/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
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
#define DO_SHMEM_TYPE_GET_NB(ctx, type, target, source, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * sizeof(type);                               \
        rc = MCA_SPML_CALL(get_nb(                                  \
            ctx,                                                    \
            (void *)source,                                         \
            size,                                                   \
            (void *)target,                                         \
            pe, NULL));                                             \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_GET_NB(type_name, type)                      \
    void shmem_ctx##type_name##_get_nbi(shmem_ctx_t ctx, type *target, const type *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_GET_NB(ctx, type, target, source, nelems, pe); \
        return ;                                                    \
    }

#define SHMEM_TYPE_GET_NB(type_name, type)                          \
    void shmem##type_name##_get_nbi(type *target, const type *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_GET_NB(oshmem_ctx_default, type, target,       \
                             source, nelems, pe);                   \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_get_nbi       = pshmem_ctx_char_get_nbi
#pragma weak shmem_ctx_short_get_nbi      = pshmem_ctx_short_get_nbi
#pragma weak shmem_ctx_int_get_nbi        = pshmem_ctx_int_get_nbi
#pragma weak shmem_ctx_long_get_nbi       = pshmem_ctx_long_get_nbi
#pragma weak shmem_ctx_float_get_nbi      = pshmem_ctx_float_get_nbi
#pragma weak shmem_ctx_double_get_nbi     = pshmem_ctx_double_get_nbi
#pragma weak shmem_ctx_longlong_get_nbi   = pshmem_ctx_longlong_get_nbi
#pragma weak shmem_ctx_schar_get_nbi      = pshmem_ctx_schar_get_nbi
#pragma weak shmem_ctx_uchar_get_nbi      = pshmem_ctx_uchar_get_nbi
#pragma weak shmem_ctx_ushort_get_nbi     = pshmem_ctx_ushort_get_nbi
#pragma weak shmem_ctx_uint_get_nbi       = pshmem_ctx_uint_get_nbi
#pragma weak shmem_ctx_ulong_get_nbi      = pshmem_ctx_ulong_get_nbi
#pragma weak shmem_ctx_ulonglong_get_nbi  = pshmem_ctx_ulonglong_get_nbi
#pragma weak shmem_ctx_longdouble_get_nbi = pshmem_ctx_longdouble_get_nbi
#pragma weak shmem_ctx_int8_get_nbi       = pshmem_ctx_int8_get_nbi
#pragma weak shmem_ctx_int16_get_nbi      = pshmem_ctx_int16_get_nbi
#pragma weak shmem_ctx_int32_get_nbi      = pshmem_ctx_int32_get_nbi
#pragma weak shmem_ctx_int64_get_nbi      = pshmem_ctx_int64_get_nbi
#pragma weak shmem_ctx_uint8_get_nbi      = pshmem_ctx_uint8_get_nbi
#pragma weak shmem_ctx_uint16_get_nbi     = pshmem_ctx_uint16_get_nbi
#pragma weak shmem_ctx_uint32_get_nbi     = pshmem_ctx_uint32_get_nbi
#pragma weak shmem_ctx_uint64_get_nbi     = pshmem_ctx_uint64_get_nbi
#pragma weak shmem_ctx_size_get_nbi       = pshmem_ctx_size_get_nbi
#pragma weak shmem_ctx_ptrdiff_get_nbi    = pshmem_ctx_ptrdiff_get_nbi

#pragma weak shmem_char_get_nbi           = pshmem_char_get_nbi
#pragma weak shmem_short_get_nbi          = pshmem_short_get_nbi
#pragma weak shmem_int_get_nbi            = pshmem_int_get_nbi
#pragma weak shmem_long_get_nbi           = pshmem_long_get_nbi
#pragma weak shmem_float_get_nbi          = pshmem_float_get_nbi
#pragma weak shmem_double_get_nbi         = pshmem_double_get_nbi
#pragma weak shmem_longlong_get_nbi       = pshmem_longlong_get_nbi
#pragma weak shmem_schar_get_nbi          = pshmem_schar_get_nbi
#pragma weak shmem_uchar_get_nbi          = pshmem_uchar_get_nbi
#pragma weak shmem_ushort_get_nbi         = pshmem_ushort_get_nbi
#pragma weak shmem_uint_get_nbi           = pshmem_uint_get_nbi
#pragma weak shmem_ulong_get_nbi          = pshmem_ulong_get_nbi
#pragma weak shmem_ulonglong_get_nbi      = pshmem_ulonglong_get_nbi
#pragma weak shmem_longdouble_get_nbi     = pshmem_longdouble_get_nbi
#pragma weak shmem_int8_get_nbi           = pshmem_int8_get_nbi
#pragma weak shmem_int16_get_nbi          = pshmem_int16_get_nbi
#pragma weak shmem_int32_get_nbi          = pshmem_int32_get_nbi
#pragma weak shmem_int64_get_nbi          = pshmem_int64_get_nbi
#pragma weak shmem_uint8_get_nbi          = pshmem_uint8_get_nbi
#pragma weak shmem_uint16_get_nbi         = pshmem_uint16_get_nbi
#pragma weak shmem_uint32_get_nbi         = pshmem_uint32_get_nbi
#pragma weak shmem_uint64_get_nbi         = pshmem_uint64_get_nbi
#pragma weak shmem_size_get_nbi           = pshmem_size_get_nbi
#pragma weak shmem_ptrdiff_get_nbi        = pshmem_ptrdiff_get_nbi

#pragma weak shmem_ctx_get8_nbi = pshmem_ctx_get8_nbi
#pragma weak shmem_ctx_get16_nbi = pshmem_ctx_get16_nbi
#pragma weak shmem_ctx_get32_nbi = pshmem_ctx_get32_nbi
#pragma weak shmem_ctx_get64_nbi = pshmem_ctx_get64_nbi
#pragma weak shmem_ctx_get128_nbi = pshmem_ctx_get128_nbi
#pragma weak shmem_ctx_getmem_nbi = pshmem_ctx_getmem_nbi

#pragma weak shmem_get8_nbi = pshmem_get8_nbi
#pragma weak shmem_get16_nbi = pshmem_get16_nbi
#pragma weak shmem_get32_nbi = pshmem_get32_nbi
#pragma weak shmem_get64_nbi = pshmem_get64_nbi
#pragma weak shmem_get128_nbi = pshmem_get128_nbi
#pragma weak shmem_getmem_nbi = pshmem_getmem_nbi
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_GET_NB(_char, char)
SHMEM_CTX_TYPE_GET_NB(_short, short)
SHMEM_CTX_TYPE_GET_NB(_int, int)
SHMEM_CTX_TYPE_GET_NB(_long, long)
SHMEM_CTX_TYPE_GET_NB(_longlong, long long)
SHMEM_CTX_TYPE_GET_NB(_schar, signed char)
SHMEM_CTX_TYPE_GET_NB(_uchar, unsigned char)
SHMEM_CTX_TYPE_GET_NB(_ushort, unsigned short)
SHMEM_CTX_TYPE_GET_NB(_uint, unsigned int)
SHMEM_CTX_TYPE_GET_NB(_ulong, unsigned long)
SHMEM_CTX_TYPE_GET_NB(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_GET_NB(_float, float)
SHMEM_CTX_TYPE_GET_NB(_double, double)
SHMEM_CTX_TYPE_GET_NB(_longdouble, long double)
SHMEM_CTX_TYPE_GET_NB(_int8, int8_t)
SHMEM_CTX_TYPE_GET_NB(_int16, int16_t)
SHMEM_CTX_TYPE_GET_NB(_int32, int32_t)
SHMEM_CTX_TYPE_GET_NB(_int64, int64_t)
SHMEM_CTX_TYPE_GET_NB(_uint8, uint8_t)
SHMEM_CTX_TYPE_GET_NB(_uint16, uint16_t)
SHMEM_CTX_TYPE_GET_NB(_uint32, uint32_t)
SHMEM_CTX_TYPE_GET_NB(_uint64, uint64_t)
SHMEM_CTX_TYPE_GET_NB(_size, size_t)
SHMEM_CTX_TYPE_GET_NB(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_GET_NB(_char, char)
SHMEM_TYPE_GET_NB(_short, short)
SHMEM_TYPE_GET_NB(_int, int)
SHMEM_TYPE_GET_NB(_long, long)
SHMEM_TYPE_GET_NB(_longlong, long long)
SHMEM_TYPE_GET_NB(_schar, signed char)
SHMEM_TYPE_GET_NB(_uchar, unsigned char)
SHMEM_TYPE_GET_NB(_ushort, unsigned short)
SHMEM_TYPE_GET_NB(_uint, unsigned int)
SHMEM_TYPE_GET_NB(_ulong, unsigned long)
SHMEM_TYPE_GET_NB(_ulonglong, unsigned long long)
SHMEM_TYPE_GET_NB(_float, float)
SHMEM_TYPE_GET_NB(_double, double)
SHMEM_TYPE_GET_NB(_longdouble, long double)
SHMEM_TYPE_GET_NB(_int8, int8_t)
SHMEM_TYPE_GET_NB(_int16, int16_t)
SHMEM_TYPE_GET_NB(_int32, int32_t)
SHMEM_TYPE_GET_NB(_int64, int64_t)
SHMEM_TYPE_GET_NB(_uint8, uint8_t)
SHMEM_TYPE_GET_NB(_uint16, uint16_t)
SHMEM_TYPE_GET_NB(_uint32, uint32_t)
SHMEM_TYPE_GET_NB(_uint64, uint64_t)
SHMEM_TYPE_GET_NB(_size, size_t)
SHMEM_TYPE_GET_NB(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_GETMEM_NB(ctx, target, source, element_size, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(get_nb(                                  \
            ctx,                                                    \
            (void *)source,                                         \
            size,                                                   \
            (void *)target,                                         \
            pe, NULL));                                             \
       RUNTIME_CHECK_RC(rc);                                        \
    } while (0)

#define SHMEM_CTX_TYPE_GETMEM_NB(name, element_size, prefix)        \
    void prefix##_ctx##name##_nbi(shmem_ctx_t ctx, void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_GETMEM_NB(ctx, target, source, element_size, nelems, pe); \
        return ;                                                    \
    }

#define SHMEM_TYPE_GETMEM_NB(name, element_size, prefix)            \
    void prefix##name##_nbi(void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_GETMEM_NB(oshmem_ctx_default, target, source,       \
                           element_size, nelems, pe);               \
        return ;                                                    \
    }

SHMEM_CTX_TYPE_GETMEM_NB(_get8, 1, shmem)
SHMEM_CTX_TYPE_GETMEM_NB(_get16, 2, shmem)
SHMEM_CTX_TYPE_GETMEM_NB(_get32, 4, shmem)
SHMEM_CTX_TYPE_GETMEM_NB(_get64, 8, shmem)
SHMEM_CTX_TYPE_GETMEM_NB(_get128, 16, shmem)
SHMEM_CTX_TYPE_GETMEM_NB(_getmem, 1, shmem)
SHMEM_TYPE_GETMEM_NB(_get8, 1, shmem)
SHMEM_TYPE_GETMEM_NB(_get16, 2, shmem)
SHMEM_TYPE_GETMEM_NB(_get32, 4, shmem)
SHMEM_TYPE_GETMEM_NB(_get64, 8, shmem)
SHMEM_TYPE_GETMEM_NB(_get128, 16, shmem)
SHMEM_TYPE_GETMEM_NB(_getmem, 1, shmem)

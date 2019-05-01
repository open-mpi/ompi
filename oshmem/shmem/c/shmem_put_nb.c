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
 * The nonblocking put routines provide a method for copying data from a contiguous local data
 * object to a data object on a specified PE.
 * These routines transfer nelems elements of the data object at address source on the calling
 * PE, to the data object at address target on the remote PE pe. These routines start the
 * remote transfer and may return before the data is delivered to the remote PE. The delivery
 * of data into the data object on the destination PE from different put calls may occur in any
 * order. Because of this, two successive put operations may deliver data out of order unless a
 * call to shmem_fence() is introduced between the two calls.
 * The routines return after posting the operation. The operation is considered complete after a
 * subsequent call to shmem_quiet. At the completion of shmem_quiet, the data has been copied
 * into the dest array on the destination PE.
 */
#define DO_SHMEM_TYPE_PUT_NB(ctx, type, target, source, len, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = len * sizeof(type);                                  \
        rc = MCA_SPML_CALL(put_nb(                                  \
            ctx,                                                    \
            (void *)target,                                         \
            size,                                                   \
            (void *)source,                                         \
            pe, NULL));                                             \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUT_NB(type_name, type)                      \
    void shmem_ctx##type_name##_put_nbi(shmem_ctx_t ctx, type *target, const type *source, size_t len, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_PUT_NB(ctx, type, target, source, len, pe);   \
        return ;                                                    \
    }

#define SHMEM_TYPE_PUT_NB(type_name, type)                          \
    void shmem##type_name##_put_nbi(type *target, const type *source, size_t len, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_PUT_NB(oshmem_ctx_default, type, target,       \
                             source, len, pe);                      \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_put_nbi       = pshmem_ctx_char_put_nbi
#pragma weak shmem_ctx_short_put_nbi      = pshmem_ctx_short_put_nbi
#pragma weak shmem_ctx_int_put_nbi        = pshmem_ctx_int_put_nbi
#pragma weak shmem_ctx_long_put_nbi       = pshmem_ctx_long_put_nbi
#pragma weak shmem_ctx_float_put_nbi      = pshmem_ctx_float_put_nbi
#pragma weak shmem_ctx_double_put_nbi     = pshmem_ctx_double_put_nbi
#pragma weak shmem_ctx_longlong_put_nbi   = pshmem_ctx_longlong_put_nbi
#pragma weak shmem_ctx_schar_put_nbi      = pshmem_ctx_schar_put_nbi
#pragma weak shmem_ctx_uchar_put_nbi      = pshmem_ctx_uchar_put_nbi
#pragma weak shmem_ctx_ushort_put_nbi     = pshmem_ctx_ushort_put_nbi
#pragma weak shmem_ctx_uint_put_nbi       = pshmem_ctx_uint_put_nbi
#pragma weak shmem_ctx_ulong_put_nbi      = pshmem_ctx_ulong_put_nbi
#pragma weak shmem_ctx_ulonglong_put_nbi  = pshmem_ctx_ulonglong_put_nbi
#pragma weak shmem_ctx_longdouble_put_nbi = pshmem_ctx_longdouble_put_nbi
#pragma weak shmem_ctx_int8_put_nbi       = pshmem_ctx_int8_put_nbi
#pragma weak shmem_ctx_int16_put_nbi      = pshmem_ctx_int16_put_nbi
#pragma weak shmem_ctx_int32_put_nbi      = pshmem_ctx_int32_put_nbi
#pragma weak shmem_ctx_int64_put_nbi      = pshmem_ctx_int64_put_nbi
#pragma weak shmem_ctx_uint8_put_nbi      = pshmem_ctx_uint8_put_nbi
#pragma weak shmem_ctx_uint16_put_nbi     = pshmem_ctx_uint16_put_nbi
#pragma weak shmem_ctx_uint32_put_nbi     = pshmem_ctx_uint32_put_nbi
#pragma weak shmem_ctx_uint64_put_nbi     = pshmem_ctx_uint64_put_nbi
#pragma weak shmem_ctx_size_put_nbi       = pshmem_ctx_size_put_nbi
#pragma weak shmem_ctx_ptrdiff_put_nbi    = pshmem_ctx_ptrdiff_put_nbi

#pragma weak shmem_char_put_nbi           = pshmem_char_put_nbi
#pragma weak shmem_short_put_nbi          = pshmem_short_put_nbi
#pragma weak shmem_int_put_nbi            = pshmem_int_put_nbi
#pragma weak shmem_long_put_nbi           = pshmem_long_put_nbi
#pragma weak shmem_float_put_nbi          = pshmem_float_put_nbi
#pragma weak shmem_double_put_nbi         = pshmem_double_put_nbi
#pragma weak shmem_longlong_put_nbi       = pshmem_longlong_put_nbi
#pragma weak shmem_schar_put_nbi          = pshmem_schar_put_nbi
#pragma weak shmem_uchar_put_nbi          = pshmem_uchar_put_nbi
#pragma weak shmem_ushort_put_nbi         = pshmem_ushort_put_nbi
#pragma weak shmem_uint_put_nbi           = pshmem_uint_put_nbi
#pragma weak shmem_ulong_put_nbi          = pshmem_ulong_put_nbi
#pragma weak shmem_ulonglong_put_nbi      = pshmem_ulonglong_put_nbi
#pragma weak shmem_longdouble_put_nbi     = pshmem_longdouble_put_nbi
#pragma weak shmem_int8_put_nbi           = pshmem_int8_put_nbi
#pragma weak shmem_int16_put_nbi          = pshmem_int16_put_nbi
#pragma weak shmem_int32_put_nbi          = pshmem_int32_put_nbi
#pragma weak shmem_int64_put_nbi          = pshmem_int64_put_nbi
#pragma weak shmem_uint8_put_nbi          = pshmem_uint8_put_nbi
#pragma weak shmem_uint16_put_nbi         = pshmem_uint16_put_nbi
#pragma weak shmem_uint32_put_nbi         = pshmem_uint32_put_nbi
#pragma weak shmem_uint64_put_nbi         = pshmem_uint64_put_nbi
#pragma weak shmem_size_put_nbi           = pshmem_size_put_nbi
#pragma weak shmem_ptrdiff_put_nbi        = pshmem_ptrdiff_put_nbi

#pragma weak shmem_put8_nbi = pshmem_put8_nbi
#pragma weak shmem_put16_nbi = pshmem_put16_nbi
#pragma weak shmem_put32_nbi = pshmem_put32_nbi
#pragma weak shmem_put64_nbi = pshmem_put64_nbi
#pragma weak shmem_put128_nbi = pshmem_put128_nbi
#pragma weak shmem_putmem_nbi = pshmem_putmem_nbi

#pragma weak shmem_ctx_put8_nbi = pshmem_ctx_put8_nbi
#pragma weak shmem_ctx_put16_nbi = pshmem_ctx_put16_nbi
#pragma weak shmem_ctx_put32_nbi = pshmem_ctx_put32_nbi
#pragma weak shmem_ctx_put64_nbi = pshmem_ctx_put64_nbi
#pragma weak shmem_ctx_put128_nbi = pshmem_ctx_put128_nbi
#pragma weak shmem_ctx_putmem_nbi = pshmem_ctx_putmem_nbi
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_PUT_NB(_char, char)
SHMEM_CTX_TYPE_PUT_NB(_short, short)
SHMEM_CTX_TYPE_PUT_NB(_int, int)
SHMEM_CTX_TYPE_PUT_NB(_long, long)
SHMEM_CTX_TYPE_PUT_NB(_longlong, long long)
SHMEM_CTX_TYPE_PUT_NB(_schar, signed char)
SHMEM_CTX_TYPE_PUT_NB(_uchar, unsigned char)
SHMEM_CTX_TYPE_PUT_NB(_ushort, unsigned short)
SHMEM_CTX_TYPE_PUT_NB(_uint, unsigned int)
SHMEM_CTX_TYPE_PUT_NB(_ulong, unsigned long)
SHMEM_CTX_TYPE_PUT_NB(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_PUT_NB(_float, float)
SHMEM_CTX_TYPE_PUT_NB(_double, double)
SHMEM_CTX_TYPE_PUT_NB(_longdouble, long double)
SHMEM_CTX_TYPE_PUT_NB(_int8, int8_t)
SHMEM_CTX_TYPE_PUT_NB(_int16, int16_t)
SHMEM_CTX_TYPE_PUT_NB(_int32, int32_t)
SHMEM_CTX_TYPE_PUT_NB(_int64, int64_t)
SHMEM_CTX_TYPE_PUT_NB(_uint8, uint8_t)
SHMEM_CTX_TYPE_PUT_NB(_uint16, uint16_t)
SHMEM_CTX_TYPE_PUT_NB(_uint32, uint32_t)
SHMEM_CTX_TYPE_PUT_NB(_uint64, uint64_t)
SHMEM_CTX_TYPE_PUT_NB(_size, size_t)
SHMEM_CTX_TYPE_PUT_NB(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_PUT_NB(_char, char)
SHMEM_TYPE_PUT_NB(_short, short)
SHMEM_TYPE_PUT_NB(_int, int)
SHMEM_TYPE_PUT_NB(_long, long)
SHMEM_TYPE_PUT_NB(_longlong, long long)
SHMEM_TYPE_PUT_NB(_schar, signed char)
SHMEM_TYPE_PUT_NB(_uchar, unsigned char)
SHMEM_TYPE_PUT_NB(_ushort, unsigned short)
SHMEM_TYPE_PUT_NB(_uint, unsigned int)
SHMEM_TYPE_PUT_NB(_ulong, unsigned long)
SHMEM_TYPE_PUT_NB(_ulonglong, unsigned long long)
SHMEM_TYPE_PUT_NB(_float, float)
SHMEM_TYPE_PUT_NB(_double, double)
SHMEM_TYPE_PUT_NB(_longdouble, long double)
SHMEM_TYPE_PUT_NB(_int8, int8_t)
SHMEM_TYPE_PUT_NB(_int16, int16_t)
SHMEM_TYPE_PUT_NB(_int32, int32_t)
SHMEM_TYPE_PUT_NB(_int64, int64_t)
SHMEM_TYPE_PUT_NB(_uint8, uint8_t)
SHMEM_TYPE_PUT_NB(_uint16, uint16_t)
SHMEM_TYPE_PUT_NB(_uint32, uint32_t)
SHMEM_TYPE_PUT_NB(_uint64, uint64_t)
SHMEM_TYPE_PUT_NB(_size, size_t)
SHMEM_TYPE_PUT_NB(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_PUTMEM_NB(ctx, target, source, element_size, nelems, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(put_nb(                                  \
            ctx,                                                    \
            (void *)target,                                         \
            size,                                                   \
            (void *)source,                                         \
            pe, NULL));                                             \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUTMEM_NB(name, element_size, prefix)        \
    void prefix##_ctx##name##_nbi(shmem_ctx_t ctx, void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_PUTMEM_NB(ctx, target, source, element_size,       \
                           nelems, pe);                             \
        return ;                                                    \
    }

#define SHMEM_TYPE_PUTMEM_NB(name, element_size, prefix)            \
    void prefix##name##_nbi(void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        DO_SHMEM_PUTMEM_NB(oshmem_ctx_default, target, source,       \
                           element_size, nelems, pe);               \
        return ;                                                    \
    }

SHMEM_CTX_TYPE_PUTMEM_NB(_put8, 1, shmem)
SHMEM_CTX_TYPE_PUTMEM_NB(_put16, 2, shmem)
SHMEM_CTX_TYPE_PUTMEM_NB(_put32, 4, shmem)
SHMEM_CTX_TYPE_PUTMEM_NB(_put64, 8, shmem)
SHMEM_CTX_TYPE_PUTMEM_NB(_put128, 16, shmem)
SHMEM_CTX_TYPE_PUTMEM_NB(_putmem, 1, shmem)
SHMEM_TYPE_PUTMEM_NB(_put8, 1, shmem)
SHMEM_TYPE_PUTMEM_NB(_put16, 2, shmem)
SHMEM_TYPE_PUTMEM_NB(_put32, 4, shmem)
SHMEM_TYPE_PUTMEM_NB(_put64, 8, shmem)
SHMEM_TYPE_PUTMEM_NB(_put128, 16, shmem)
SHMEM_TYPE_PUTMEM_NB(_putmem, 1, shmem)

void shmemx_alltoall_global_nb(void *dest,
                               const void *source,
                               size_t size,
                               long *counter)
{
    int rc = MCA_SPML_CALL(put_all_nb(dest, source, size, counter));
    RUNTIME_CHECK_RC(rc);
}

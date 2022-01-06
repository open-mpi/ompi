/*
 * Copyright (c) 2021      NVIDIA Corporation.
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

#define DO_SHMEM_TYPE_PUT_SIGNAL(ctx, type, dest, source, nelems, sig_addr, signal, sig_op, pe) do {  \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(dest);                                   \
                                                                    \
        size = nelems * sizeof(type);                               \
        rc = MCA_SPML_CALL(put_signal(                              \
            ctx,                                                    \
            (void*)dest,                                            \
            size,                                                   \
            (void*)source,                                          \
            sig_addr, signal, sig_op, pe));                         \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUT_SIGNAL(type_name, type)                                                    \
    void shmem_ctx##type_name##_put_signal(shmem_ctx_t ctx, type *dest, const type *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe)\
    {                                                                                                 \
        DO_SHMEM_TYPE_PUT_SIGNAL(ctx, type, dest, source, nelems, sig_addr, signal, sig_op, pe);      \
        return ;                                                                                      \
    }

#define SHMEM_TYPE_PUT_SIGNAL(type_name, type)                            \
    void shmem##type_name##_put_signal(type *dest, const type *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe)\
    {                                                                     \
        DO_SHMEM_TYPE_PUT_SIGNAL(oshmem_ctx_default, type, dest,          \
                          source, nelems, sig_addr, signal, sig_op, pe);  \
        return ;                                                          \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_char_put_signal             = pshmem_ctx_char_put_signal
#pragma weak shmem_ctx_short_put_signal            = pshmem_ctx_short_put_signal
#pragma weak shmem_ctx_int_put_signal              = pshmem_ctx_int_put_signal
#pragma weak shmem_ctx_long_put_signal             = pshmem_ctx_long_put_signal
#pragma weak shmem_ctx_float_put_signal            = pshmem_ctx_float_put_signal
#pragma weak shmem_ctx_double_put_signal           = pshmem_ctx_double_put_signal
#pragma weak shmem_ctx_longlong_put_signal         = pshmem_ctx_longlong_put_signal
#pragma weak shmem_ctx_schar_put_signal            = pshmem_ctx_schar_put_signal
#pragma weak shmem_ctx_uchar_put_signal            = pshmem_ctx_uchar_put_signal
#pragma weak shmem_ctx_ushort_put_signal           = pshmem_ctx_ushort_put_signal
#pragma weak shmem_ctx_uint_put_signal             = pshmem_ctx_uint_put_signal
#pragma weak shmem_ctx_ulong_put_signal            = pshmem_ctx_ulong_put_signal
#pragma weak shmem_ctx_ulonglong_put_signal        = pshmem_ctx_ulonglong_put_signal
#pragma weak shmem_ctx_longdouble_put_signal       = pshmem_ctx_longdouble_put_signal
#pragma weak shmem_ctx_int8_put_signal             = pshmem_ctx_int8_put_signal
#pragma weak shmem_ctx_int16_put_signal            = pshmem_ctx_int16_put_signal
#pragma weak shmem_ctx_int32_put_signal            = pshmem_ctx_int32_put_signal
#pragma weak shmem_ctx_int64_put_signal            = pshmem_ctx_int64_put_signal
#pragma weak shmem_ctx_uint8_put_signal            = pshmem_ctx_uint8_put_signal
#pragma weak shmem_ctx_uint16_put_signal           = pshmem_ctx_uint16_put_signal
#pragma weak shmem_ctx_uint32_put_signal           = pshmem_ctx_uint32_put_signal
#pragma weak shmem_ctx_uint64_put_signal           = pshmem_ctx_uint64_put_signal
#pragma weak shmem_ctx_size_put_signal             = pshmem_ctx_size_put_signal
#pragma weak shmem_ctx_ptrdiff_put_signal          = pshmem_ctx_ptrdiff_put_signal

#pragma weak shmem_char_put_signal                 = pshmem_char_put_signal
#pragma weak shmem_short_put_signal                = pshmem_short_put_signal
#pragma weak shmem_int_put_signal                  = pshmem_int_put_signal
#pragma weak shmem_long_put_signal                 = pshmem_long_put_signal
#pragma weak shmem_float_put_signal                = pshmem_float_put_signal
#pragma weak shmem_double_put_signal               = pshmem_double_put_signal
#pragma weak shmem_longlong_put_signal             = pshmem_longlong_put_signal
#pragma weak shmem_schar_put_signal                = pshmem_schar_put_signal
#pragma weak shmem_uchar_put_signal                = pshmem_uchar_put_signal
#pragma weak shmem_ushort_put_signal               = pshmem_ushort_put_signal
#pragma weak shmem_uint_put_signal                 = pshmem_uint_put_signal
#pragma weak shmem_ulong_put_signal                = pshmem_ulong_put_signal
#pragma weak shmem_ulonglong_put_signal            = pshmem_ulonglong_put_signal
#pragma weak shmem_longdouble_put_signal           = pshmem_longdouble_put_signal
#pragma weak shmem_int8_put_signal                 = pshmem_int8_put_signal
#pragma weak shmem_int16_put_signal                = pshmem_int16_put_signal
#pragma weak shmem_int32_put_signal                = pshmem_int32_put_signal
#pragma weak shmem_int64_put_signal                = pshmem_int64_put_signal
#pragma weak shmem_uint8_put_signal                = pshmem_uint8_put_signal
#pragma weak shmem_uint16_put_signal               = pshmem_uint16_put_signal
#pragma weak shmem_uint32_put_signal               = pshmem_uint32_put_signal
#pragma weak shmem_uint64_put_signal               = pshmem_uint64_put_signal
#pragma weak shmem_size_put_signal                 = pshmem_size_put_signal
#pragma weak shmem_ptrdiff_put_signal              = pshmem_ptrdiff_put_signal

#pragma weak shmem_put8_signal                     = pshmem_put8_signal
#pragma weak shmem_put16_signal                    = pshmem_put16_signal
#pragma weak shmem_put32_signal                    = pshmem_put32_signal
#pragma weak shmem_put64_signal                    = pshmem_put64_signal
#pragma weak shmem_put128_signal                   = pshmem_put128_signal

#pragma weak shmem_ctx_put8_signal                 = pshmem_ctx_put8_signal
#pragma weak shmem_ctx_put16_signal                = pshmem_ctx_put16_signal
#pragma weak shmem_ctx_put32_signal                = pshmem_ctx_put32_signal
#pragma weak shmem_ctx_put64_signal                = pshmem_ctx_put64_signal
#pragma weak shmem_ctx_put128_signal               = pshmem_ctx_put128_signal

#pragma weak shmem_putmem_signal                  = pshmem_putmem_signal
#pragma weak shmem_ctx_putmem_signal              = pshmem_ctx_putmem_signal


#pragma weak shmem_signal_fetch                   = pshmem_signal_fetch




#include "oshmem/shmem/c/profile-defines.h"
#endif

SHMEM_CTX_TYPE_PUT_SIGNAL(_char, char)
SHMEM_CTX_TYPE_PUT_SIGNAL(_short, short)
SHMEM_CTX_TYPE_PUT_SIGNAL(_int, int)
SHMEM_CTX_TYPE_PUT_SIGNAL(_long, long)
SHMEM_CTX_TYPE_PUT_SIGNAL(_longlong, long long)
SHMEM_CTX_TYPE_PUT_SIGNAL(_schar, signed char)
SHMEM_CTX_TYPE_PUT_SIGNAL(_uchar, unsigned char)
SHMEM_CTX_TYPE_PUT_SIGNAL(_ushort, unsigned short)
SHMEM_CTX_TYPE_PUT_SIGNAL(_uint, unsigned int)
SHMEM_CTX_TYPE_PUT_SIGNAL(_ulong, unsigned long)
SHMEM_CTX_TYPE_PUT_SIGNAL(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_PUT_SIGNAL(_float, float)
SHMEM_CTX_TYPE_PUT_SIGNAL(_double, double)
SHMEM_CTX_TYPE_PUT_SIGNAL(_longdouble, long double)
SHMEM_CTX_TYPE_PUT_SIGNAL(_int8, int8_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_int16, int16_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_int32, int32_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_int64, int64_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_uint8, uint8_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_uint16, uint16_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_uint32, uint32_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_uint64, uint64_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_size, size_t)
SHMEM_CTX_TYPE_PUT_SIGNAL(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_PUT_SIGNAL(_char, char)
SHMEM_TYPE_PUT_SIGNAL(_short, short)
SHMEM_TYPE_PUT_SIGNAL(_int, int)
SHMEM_TYPE_PUT_SIGNAL(_long, long)
SHMEM_TYPE_PUT_SIGNAL(_longlong, long long)
SHMEM_TYPE_PUT_SIGNAL(_schar, signed char)
SHMEM_TYPE_PUT_SIGNAL(_uchar, unsigned char)
SHMEM_TYPE_PUT_SIGNAL(_ushort, unsigned short)
SHMEM_TYPE_PUT_SIGNAL(_uint, unsigned int)
SHMEM_TYPE_PUT_SIGNAL(_ulong, unsigned long)
SHMEM_TYPE_PUT_SIGNAL(_ulonglong, unsigned long long)
SHMEM_TYPE_PUT_SIGNAL(_float, float)
SHMEM_TYPE_PUT_SIGNAL(_double, double)
SHMEM_TYPE_PUT_SIGNAL(_longdouble, long double)
SHMEM_TYPE_PUT_SIGNAL(_int8, int8_t)
SHMEM_TYPE_PUT_SIGNAL(_int16, int16_t)
SHMEM_TYPE_PUT_SIGNAL(_int32, int32_t)
SHMEM_TYPE_PUT_SIGNAL(_int64, int64_t)
SHMEM_TYPE_PUT_SIGNAL(_uint8, uint8_t)
SHMEM_TYPE_PUT_SIGNAL(_uint16, uint16_t)
SHMEM_TYPE_PUT_SIGNAL(_uint32, uint32_t)
SHMEM_TYPE_PUT_SIGNAL(_uint64, uint64_t)
SHMEM_TYPE_PUT_SIGNAL(_size, size_t)
SHMEM_TYPE_PUT_SIGNAL(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_PUTMEM_SIGNAL(ctx, dest, source, element_size, nelems, sig_addr, signal, sig_op, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(dest);                                   \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(put_signal(                              \
            ctx,                                                    \
            (void*)dest,                                            \
            size,                                                   \
            (void*)source,                                          \
            sig_addr, signal, sig_op, pe));                         \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUTMEM_SIGNAL(name, element_size, prefix)               \
    void prefix##_ctx##name(shmem_ctx_t ctx, void *dest, const void *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe) \
    {                                                                          \
        DO_SHMEM_PUTMEM_SIGNAL(ctx, dest, source,                              \
                        element_size, nelems, sig_addr, signal, sig_op, pe);   \
        return ;                                                               \
    }

#define SHMEM_TYPE_PUTMEM_SIGNAL(name, element_size, prefix)                           \
    void prefix##name(void *dest, const void *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe) \
    {                                                                                  \
        DO_SHMEM_PUTMEM_SIGNAL(oshmem_ctx_default, dest,                               \
                        source, element_size, nelems, sig_addr, signal, sig_op, pe);   \
        return ;                                                                       \
    }

SHMEM_CTX_TYPE_PUTMEM_SIGNAL(_putmem_signal, 1, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL(_put8_signal,  1, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL(_put16_signal, 2, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL(_put32_signal, 4, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL(_put64_signal, 8, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL(_put128_signal, 16, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL(_putmem_signal, 1, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL(_put8_signal,  1, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL(_put16_signal, 2, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL(_put32_signal, 4, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL(_put64_signal, 8, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL(_put128_signal, 16, shmem)


uint64_t shmem_signal_fetch(const uint64_t *sig_addr)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}


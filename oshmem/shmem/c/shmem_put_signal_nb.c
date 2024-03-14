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

#define DO_SHMEM_TYPE_PUT_SIGNAL_NBI(ctx, type, dest, source, nelems, sig_addr, signal, sig_op, pe) do {  \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(dest);                                   \
                                                                    \
        size = nelems * sizeof(type);                               \
        rc = MCA_SPML_CALL(put_signal_nb(                           \
            ctx,                                                    \
            (void*)dest,                                            \
            size,                                                   \
            (void*)source,                                          \
            sig_addr, signal, sig_op, pe));                         \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(type_name, type)                                                 \
    void shmem_ctx##type_name##_put_signal_nbi(shmem_ctx_t ctx, type *dest, const type *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe)\
    {                                                                                                  \
        DO_SHMEM_TYPE_PUT_SIGNAL_NBI(ctx, type, dest, source, nelems, sig_addr, signal, sig_op, pe);   \
        return ;                                                                                       \
    }

#define SHMEM_TYPE_PUT_SIGNAL_NBI(type_name, type)                                                     \
    void shmem##type_name##_put_signal_nbi(type *dest, const type *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe)\
    {                                                                                                  \
        DO_SHMEM_TYPE_PUT_SIGNAL_NBI(oshmem_ctx_default, type, dest,                                   \
                          source, nelems, sig_addr, signal, sig_op, pe);                               \
        return ;                                                                                       \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"

#pragma weak shmem_ctx_char_put_signal_nbi             = pshmem_ctx_char_put_signal_nbi
#pragma weak shmem_ctx_short_put_signal_nbi            = pshmem_ctx_short_put_signal_nbi
#pragma weak shmem_ctx_int_put_signal_nbi              = pshmem_ctx_int_put_signal_nbi
#pragma weak shmem_ctx_long_put_signal_nbi             = pshmem_ctx_long_put_signal_nbi
#pragma weak shmem_ctx_float_put_signal_nbi            = pshmem_ctx_float_put_signal_nbi
#pragma weak shmem_ctx_double_put_signal_nbi           = pshmem_ctx_double_put_signal_nbi
#pragma weak shmem_ctx_longlong_put_signal_nbi         = pshmem_ctx_longlong_put_signal_nbi
#pragma weak shmem_ctx_schar_put_signal_nbi            = pshmem_ctx_schar_put_signal_nbi
#pragma weak shmem_ctx_uchar_put_signal_nbi            = pshmem_ctx_uchar_put_signal_nbi
#pragma weak shmem_ctx_ushort_put_signal_nbi           = pshmem_ctx_ushort_put_signal_nbi
#pragma weak shmem_ctx_uint_put_signal_nbi             = pshmem_ctx_uint_put_signal_nbi
#pragma weak shmem_ctx_ulong_put_signal_nbi            = pshmem_ctx_ulong_put_signal_nbi
#pragma weak shmem_ctx_ulonglong_put_signal_nbi        = pshmem_ctx_ulonglong_put_signal_nbi
#pragma weak shmem_ctx_longdouble_put_signal_nbi       = pshmem_ctx_longdouble_put_signal_nbi
#pragma weak shmem_ctx_int8_put_signal_nbi             = pshmem_ctx_int8_put_signal_nbi
#pragma weak shmem_ctx_int16_put_signal_nbi            = pshmem_ctx_int16_put_signal_nbi
#pragma weak shmem_ctx_int32_put_signal_nbi            = pshmem_ctx_int32_put_signal_nbi
#pragma weak shmem_ctx_int64_put_signal_nbi            = pshmem_ctx_int64_put_signal_nbi
#pragma weak shmem_ctx_uint8_put_signal_nbi            = pshmem_ctx_uint8_put_signal_nbi
#pragma weak shmem_ctx_uint16_put_signal_nbi           = pshmem_ctx_uint16_put_signal_nbi
#pragma weak shmem_ctx_uint32_put_signal_nbi           = pshmem_ctx_uint32_put_signal_nbi
#pragma weak shmem_ctx_uint64_put_signal_nbi           = pshmem_ctx_uint64_put_signal_nbi
#pragma weak shmem_ctx_size_put_signal_nbi             = pshmem_ctx_size_put_signal_nbi
#pragma weak shmem_ctx_ptrdiff_put_signal_nbi          = pshmem_ctx_ptrdiff_put_signal_nbi

#pragma weak shmem_char_put_signal_nbi                 = pshmem_char_put_signal_nbi
#pragma weak shmem_short_put_signal_nbi                = pshmem_short_put_signal_nbi
#pragma weak shmem_int_put_signal_nbi                  = pshmem_int_put_signal_nbi
#pragma weak shmem_long_put_signal_nbi                 = pshmem_long_put_signal_nbi
#pragma weak shmem_float_put_signal_nbi                = pshmem_float_put_signal_nbi
#pragma weak shmem_double_put_signal_nbi               = pshmem_double_put_signal_nbi
#pragma weak shmem_longlong_put_signal_nbi             = pshmem_longlong_put_signal_nbi
#pragma weak shmem_schar_put_signal_nbi                = pshmem_schar_put_signal_nbi
#pragma weak shmem_uchar_put_signal_nbi                = pshmem_uchar_put_signal_nbi
#pragma weak shmem_ushort_put_signal_nbi               = pshmem_ushort_put_signal_nbi
#pragma weak shmem_uint_put_signal_nbi                 = pshmem_uint_put_signal_nbi
#pragma weak shmem_ulong_put_signal_nbi                = pshmem_ulong_put_signal_nbi
#pragma weak shmem_ulonglong_put_signal_nbi            = pshmem_ulonglong_put_signal_nbi
#pragma weak shmem_longdouble_put_signal_nbi           = pshmem_longdouble_put_signal_nbi
#pragma weak shmem_int8_put_signal_nbi                 = pshmem_int8_put_signal_nbi
#pragma weak shmem_int16_put_signal_nbi                = pshmem_int16_put_signal_nbi
#pragma weak shmem_int32_put_signal_nbi                = pshmem_int32_put_signal_nbi
#pragma weak shmem_int64_put_signal_nbi                = pshmem_int64_put_signal_nbi
#pragma weak shmem_uint8_put_signal_nbi                = pshmem_uint8_put_signal_nbi
#pragma weak shmem_uint16_put_signal_nbi               = pshmem_uint16_put_signal_nbi
#pragma weak shmem_uint32_put_signal_nbi               = pshmem_uint32_put_signal_nbi
#pragma weak shmem_uint64_put_signal_nbi               = pshmem_uint64_put_signal_nbi
#pragma weak shmem_size_put_signal_nbi                 = pshmem_size_put_signal_nbi
#pragma weak shmem_ptrdiff_put_signal_nbi              = pshmem_ptrdiff_put_signal_nbi

#pragma weak shmem_put8_signal_nbi                     = pshmem_put8_signal_nbi
#pragma weak shmem_put16_signal_nbi                    = pshmem_put16_signal_nbi
#pragma weak shmem_put32_signal_nbi                    = pshmem_put32_signal_nbi
#pragma weak shmem_put64_signal_nbi                    = pshmem_put64_signal_nbi
#pragma weak shmem_put128_signal_nbi                   = pshmem_put128_signal_nbi

#pragma weak shmem_ctx_put8_signal_nbi                 = pshmem_ctx_put8_signal_nbi
#pragma weak shmem_ctx_put16_signal_nbi                = pshmem_ctx_put16_signal_nbi
#pragma weak shmem_ctx_put32_signal_nbi                = pshmem_ctx_put32_signal_nbi
#pragma weak shmem_ctx_put64_signal_nbi                = pshmem_ctx_put64_signal_nbi
#pragma weak shmem_ctx_put128_signal_nbi               = pshmem_ctx_put128_signal_nbi

#pragma weak shmem_putmem_signal_nbi                  = pshmem_putmem_signal_nbi
#pragma weak shmem_ctx_putmem_signal_nbi              = pshmem_ctx_putmem_signal_nbi


#include "oshmem/shmem/c/profile-defines.h"
#endif

SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_char, char)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_short, short)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_int, int)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_long, long)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_longlong, long long)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_schar, signed char)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_uchar, unsigned char)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_ushort, unsigned short)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_uint, unsigned int)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_ulong, unsigned long)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_ulonglong, unsigned long long)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_float, float)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_double, double)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_longdouble, long double)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_int8, int8_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_int16, int16_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_int32, int32_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_int64, int64_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_uint8, uint8_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_uint16, uint16_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_uint32, uint32_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_uint64, uint64_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_size, size_t)
SHMEM_CTX_TYPE_PUT_SIGNAL_NBI(_ptrdiff, ptrdiff_t)

SHMEM_TYPE_PUT_SIGNAL_NBI(_char, char)
SHMEM_TYPE_PUT_SIGNAL_NBI(_short, short)
SHMEM_TYPE_PUT_SIGNAL_NBI(_int, int)
SHMEM_TYPE_PUT_SIGNAL_NBI(_long, long)
SHMEM_TYPE_PUT_SIGNAL_NBI(_longlong, long long)
SHMEM_TYPE_PUT_SIGNAL_NBI(_schar, signed char)
SHMEM_TYPE_PUT_SIGNAL_NBI(_uchar, unsigned char)
SHMEM_TYPE_PUT_SIGNAL_NBI(_ushort, unsigned short)
SHMEM_TYPE_PUT_SIGNAL_NBI(_uint, unsigned int)
SHMEM_TYPE_PUT_SIGNAL_NBI(_ulong, unsigned long)
SHMEM_TYPE_PUT_SIGNAL_NBI(_ulonglong, unsigned long long)
SHMEM_TYPE_PUT_SIGNAL_NBI(_float, float)
SHMEM_TYPE_PUT_SIGNAL_NBI(_double, double)
SHMEM_TYPE_PUT_SIGNAL_NBI(_longdouble, long double)
SHMEM_TYPE_PUT_SIGNAL_NBI(_int8, int8_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_int16, int16_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_int32, int32_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_int64, int64_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_uint8, uint8_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_uint16, uint16_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_uint32, uint32_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_uint64, uint64_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_size, size_t)
SHMEM_TYPE_PUT_SIGNAL_NBI(_ptrdiff, ptrdiff_t)

#define DO_SHMEM_PUTMEM_SIGNAL_NBI(ctx, dest, source, element_size, nelems, sig_addr, signal, sig_op, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(dest);                                   \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(put_signal_nb(                           \
            ctx,                                                    \
            (void*)dest,                                            \
            size,                                                   \
            (void*)source,                                          \
            sig_addr, signal, sig_op, pe));                         \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_PUTMEM_SIGNAL_NBI(name, element_size, prefix)           \
    void prefix##_ctx##name(shmem_ctx_t ctx, void *dest, const void *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe) \
    {                                                                          \
        DO_SHMEM_PUTMEM_SIGNAL_NBI(ctx, dest, source,                          \
                        element_size, nelems, sig_addr, signal, sig_op, pe);   \
        return ;                                                               \
    }

#define SHMEM_TYPE_PUTMEM_SIGNAL_NBI(name, element_size, prefix)                       \
    void prefix##name(void *dest, const void *source, size_t nelems, uint64_t *sig_addr, uint64_t signal, int sig_op, int pe) \
    {                                                                                  \
        DO_SHMEM_PUTMEM_SIGNAL_NBI(oshmem_ctx_default, dest,                           \
                        source, element_size, nelems, sig_addr, signal, sig_op, pe);   \
        return ;                                                                       \
    }

SHMEM_CTX_TYPE_PUTMEM_SIGNAL_NBI(_putmem_signal_nbi, 1, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL_NBI(_put8_signal_nbi,  1, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL_NBI(_put16_signal_nbi, 2, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL_NBI(_put32_signal_nbi, 4, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL_NBI(_put64_signal_nbi, 8, shmem)
SHMEM_CTX_TYPE_PUTMEM_SIGNAL_NBI(_put128_signal_nbi, 16, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL_NBI(_putmem_signal_nbi, 1, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL_NBI(_put8_signal_nbi,  1, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL_NBI(_put16_signal_nbi, 2, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL_NBI(_put32_signal_nbi, 4, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL_NBI(_put64_signal_nbi, 8, shmem)
SHMEM_TYPE_PUTMEM_SIGNAL_NBI(_put128_signal_nbi, 16, shmem)


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
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/op/op.h"
#include "oshmem/mca/atomic/atomic.h"

/*
 * These routines perform an atomic fetch operation.
 * The fetch routines retrieve the value at address target on PE pe.
 * The operation must be completed without the possibility of another process 
 * updating target during the fetch.
 */
#define DO_SHMEM_TYPE_ATOMIC_FETCH(ctx, type_name, type, target, pe, out_value) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
        type value = 0;                                             \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(out_value);                                   \
        rc = MCA_ATOMIC_CALL(fadd(                                  \
            ctx,                                                    \
            (void*)target,                                          \
            (void*)&out_value,                                      \
            value,                                                  \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_ATOMIC_FETCH(type_name, type, prefix)        \
    type prefix##_ctx##type_name##_atomic_fetch(shmem_ctx_t ctx, const type *target, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH(ctx, type_name, type, target,    \
                                   pe, out_value);                  \
        return out_value;                                           \
    }

#define SHMEM_TYPE_ATOMIC_FETCH(type_name, type, prefix)            \
    type prefix##type_name##_atomic_fetch(const type *target, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH(oshmem_ctx_default, type_name,   \
                                   type, target, pe, out_value);    \
                                                                    \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_int_atomic_fetch = pshmem_ctx_int_atomic_fetch
#pragma weak shmem_ctx_long_atomic_fetch = pshmem_ctx_long_atomic_fetch
#pragma weak shmem_ctx_longlong_atomic_fetch = pshmem_ctx_longlong_atomic_fetch
#pragma weak shmem_ctx_uint_atomic_fetch = pshmem_ctx_uint_atomic_fetch
#pragma weak shmem_ctx_ulong_atomic_fetch = pshmem_ctx_ulong_atomic_fetch
#pragma weak shmem_ctx_ulonglong_atomic_fetch = pshmem_ctx_ulonglong_atomic_fetch
#pragma weak shmem_ctx_double_atomic_fetch = pshmem_ctx_double_atomic_fetch
#pragma weak shmem_ctx_float_atomic_fetch = pshmem_ctx_float_atomic_fetch

#pragma weak shmem_int_atomic_fetch = pshmem_int_atomic_fetch
#pragma weak shmem_long_atomic_fetch = pshmem_long_atomic_fetch
#pragma weak shmem_longlong_atomic_fetch = pshmem_longlong_atomic_fetch
#pragma weak shmem_uint_atomic_fetch = pshmem_uint_atomic_fetch
#pragma weak shmem_ulong_atomic_fetch = pshmem_ulong_atomic_fetch
#pragma weak shmem_ulonglong_atomic_fetch = pshmem_ulonglong_atomic_fetch
#pragma weak shmem_double_atomic_fetch = pshmem_double_atomic_fetch
#pragma weak shmem_float_atomic_fetch = pshmem_float_atomic_fetch

#pragma weak shmem_int_fetch = pshmem_int_fetch
#pragma weak shmem_long_fetch = pshmem_long_fetch
#pragma weak shmem_longlong_fetch = pshmem_longlong_fetch
#pragma weak shmem_double_fetch = pshmem_double_fetch
#pragma weak shmem_float_fetch = pshmem_float_fetch

#pragma weak shmemx_int32_fetch = pshmemx_int32_fetch
#pragma weak shmemx_int64_fetch = pshmemx_int64_fetch
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_FETCH(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH(_longlong, long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH(_ulonglong, unsigned long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH(_double, double, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH(_float, float, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_int, int, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_long, long, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_double, double, shmem)
SHMEM_TYPE_ATOMIC_FETCH(_float, float, shmem)

/* deprecated APIs */
#define SHMEM_TYPE_FETCH(type_name, type, prefix)                   \
    type prefix##type_name##_fetch(const type *target, int pe)      \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH(oshmem_ctx_default, type_name,    \
                                   type, target, pe, out_value);    \
        return out_value;                                           \
    }

SHMEM_TYPE_FETCH(_int, int, shmem)
SHMEM_TYPE_FETCH(_long, long, shmem)
SHMEM_TYPE_FETCH(_longlong, long long, shmem)
SHMEM_TYPE_FETCH(_double, double, shmem)
SHMEM_TYPE_FETCH(_float, float, shmem)
SHMEM_TYPE_FETCH(_int32, int32_t, shmemx)
SHMEM_TYPE_FETCH(_int64, int64_t, shmemx)


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
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/atomic/atomic.h"

/*
 * shmem_swap performs an atomic swap operation.
 * The atomic swap routines write value to address target on PE pe, and return the previous
 * contents of target. The operation must be completed without the possibility of another
 * process updating target between the time of the fetch and the update.
 */
#define DO_SHMEM_TYPE_ATOMIC_SWAP(ctx, type, target, value, pe, out_value) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(out_value);                                   \
        rc = MCA_ATOMIC_CALL(swap(                                  \
            ctx,                                                    \
            (void*)target,                                          \
            (void*)&out_value,                                      \
            OSHMEM_ATOMIC_PTR_2_INT(&value, sizeof(value)),         \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_ATOMIC_SWAP(type_name, type, prefix)         \
    type prefix##_ctx##type_name##_atomic_swap(shmem_ctx_t ctx, type *target, type value, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_SWAP(ctx, type, target, value, pe,     \
                                  out_value);                       \
        return out_value;                                           \
    }

#define SHMEM_TYPE_ATOMIC_SWAP(type_name, type, prefix)             \
    type prefix##type_name##_atomic_swap(type *target, type value, int pe)\
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_SWAP(oshmem_ctx_default, type, target,  \
                                  value, pe, out_value);            \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_int_atomic_swap = pshmem_ctx_int_atomic_swap
#pragma weak shmem_ctx_long_atomic_swap = pshmem_ctx_long_atomic_swap
#pragma weak shmem_ctx_longlong_atomic_swap = pshmem_ctx_longlong_atomic_swap
#pragma weak shmem_ctx_uint_atomic_swap = pshmem_ctx_uint_atomic_swap
#pragma weak shmem_ctx_ulong_atomic_swap = pshmem_ctx_ulong_atomic_swap
#pragma weak shmem_ctx_ulonglong_atomic_swap = pshmem_ctx_ulonglong_atomic_swap
#pragma weak shmem_ctx_float_atomic_swap = pshmem_ctx_float_atomic_swap
#pragma weak shmem_ctx_double_atomic_swap = pshmem_ctx_double_atomic_swap

#pragma weak shmem_int_atomic_swap = pshmem_int_atomic_swap
#pragma weak shmem_long_atomic_swap = pshmem_long_atomic_swap
#pragma weak shmem_longlong_atomic_swap = pshmem_longlong_atomic_swap
#pragma weak shmem_uint_atomic_swap = pshmem_uint_atomic_swap
#pragma weak shmem_ulong_atomic_swap = pshmem_ulong_atomic_swap
#pragma weak shmem_ulonglong_atomic_swap = pshmem_ulonglong_atomic_swap
#pragma weak shmem_float_atomic_swap = pshmem_float_atomic_swap
#pragma weak shmem_double_atomic_swap = pshmem_double_atomic_swap

#pragma weak shmem_int_swap = pshmem_int_swap
#pragma weak shmem_long_swap = pshmem_long_swap
#pragma weak shmem_longlong_swap = pshmem_longlong_swap
#pragma weak shmem_float_swap = pshmem_float_swap
#pragma weak shmem_double_swap = pshmem_double_swap

#pragma weak shmemx_int32_swap = pshmemx_int32_swap
#pragma weak shmemx_int64_swap = pshmemx_int64_swap
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_SWAP(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_SWAP(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SWAP(_longlong, long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SWAP(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_SWAP(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SWAP(_ulonglong, unsigned long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SWAP(_float, float, shmem)
SHMEM_CTX_TYPE_ATOMIC_SWAP(_double, double, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_int, int, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_long, long, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_float, float, shmem)
SHMEM_TYPE_ATOMIC_SWAP(_double, double, shmem)

/* deprecated APIs */
#define SHMEM_TYPE_SWAP(type_name, type, prefix)                    \
    type prefix##type_name##_swap(type *target, type value, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_SWAP(oshmem_ctx_default, type, target,  \
                                  value, pe, out_value);            \
        return out_value;                                           \
    }
SHMEM_TYPE_SWAP(_int, int, shmem)
SHMEM_TYPE_SWAP(_long, long, shmem)
SHMEM_TYPE_SWAP(_longlong, long long, shmem)
SHMEM_TYPE_SWAP(_float, float, shmem)
SHMEM_TYPE_SWAP(_double, double, shmem)
SHMEM_TYPE_SWAP(_int32, int32_t, shmemx)
SHMEM_TYPE_SWAP(_int64, int64_t, shmemx)

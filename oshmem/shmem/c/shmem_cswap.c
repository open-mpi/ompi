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
 * shmem_cswap performs an atomic conditional swap operation.
 * The conditional swap routines write value to address target on PE pe, and return the previous
 * contents of target. The replacement must occur only if cond is equal to target;
 * otherwise target is left unchanged. In either case, the routine must return the initial value
 * of target. The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */
#define DO_SHMEM_TYPE_ATOMIC_COMPARE_SWAP(ctx, type, target, cond, value, pe, out_value) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(value);                                       \
        rc = MCA_ATOMIC_CALL(cswap(                                 \
            ctx,                                                    \
            (void*)target,                                          \
            (uint64_t*)&out_value,                                  \
            OSHMEM_ATOMIC_PTR_2_INT(&cond, sizeof(cond)),           \
            OSHMEM_ATOMIC_PTR_2_INT(&value, sizeof(value)),         \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_ATOMIC_COMPARE_SWAP(type_name, type, prefix) \
    type prefix##_ctx##type_name##_atomic_compare_swap(shmem_ctx_t ctx, type *target, type cond, type value, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_COMPARE_SWAP(ctx, type, target, cond, value,  \
                                   pe, out_value);                  \
        return out_value;                                           \
    }

#define SHMEM_TYPE_ATOMIC_COMPARE_SWAP(type_name, type, prefix)     \
    type prefix##type_name##_atomic_compare_swap(type *target, type cond, type value, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_COMPARE_SWAP(oshmem_ctx_default, type, target, \
                                   cond, value, pe, out_value);     \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_uint_atomic_compare_swap = pshmem_ctx_uint_atomic_compare_swap
#pragma weak shmem_ctx_ulong_atomic_compare_swap = pshmem_ctx_ulong_atomic_compare_swap
#pragma weak shmem_ctx_ulonglong_atomic_compare_swap = pshmem_ctx_ulonglong_atomic_compare_swap
#pragma weak shmem_ctx_int_atomic_compare_swap = pshmem_ctx_int_atomic_compare_swap
#pragma weak shmem_ctx_long_atomic_compare_swap = pshmem_ctx_long_atomic_compare_swap
#pragma weak shmem_ctx_longlong_atomic_compare_swap = pshmem_ctx_longlong_atomic_compare_swap

#pragma weak shmem_int_atomic_compare_swap = pshmem_int_atomic_compare_swap
#pragma weak shmem_long_atomic_compare_swap = pshmem_long_atomic_compare_swap
#pragma weak shmem_longlong_atomic_compare_swap = pshmem_longlong_atomic_compare_swap
#pragma weak shmem_uint_atomic_compare_swap = pshmem_uint_atomic_compare_swap
#pragma weak shmem_ulong_atomic_compare_swap = pshmem_ulong_atomic_compare_swap
#pragma weak shmem_ulonglong_atomic_compare_swap = pshmem_ulonglong_atomic_compare_swap

#pragma weak shmem_int_cswap = pshmem_int_cswap
#pragma weak shmem_long_cswap = pshmem_long_cswap
#pragma weak shmem_longlong_cswap = pshmem_longlong_cswap

#pragma weak shmemx_int32_cswap = pshmemx_int32_cswap
#pragma weak shmemx_int64_cswap = pshmemx_int64_cswap
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_COMPARE_SWAP(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_COMPARE_SWAP(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_COMPARE_SWAP(_longlong, long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_COMPARE_SWAP(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_COMPARE_SWAP(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_COMPARE_SWAP(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_ATOMIC_COMPARE_SWAP(_int, int, shmem)
SHMEM_TYPE_ATOMIC_COMPARE_SWAP(_long, long, shmem)
SHMEM_TYPE_ATOMIC_COMPARE_SWAP(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_COMPARE_SWAP(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_COMPARE_SWAP(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_COMPARE_SWAP(_ulonglong, unsigned long long, shmem)

/* deprecated APIs */
#define SHMEM_TYPE_CSWAP(type_name, type, prefix)                   \
    type prefix##type_name##_cswap(type *target, type cond, type value, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_COMPARE_SWAP(oshmem_ctx_default, type, target, \
                                   cond, value, pe, out_value);     \
        return out_value;                                           \
    }

SHMEM_TYPE_CSWAP(_int, int, shmem)
SHMEM_TYPE_CSWAP(_long, long, shmem)
SHMEM_TYPE_CSWAP(_longlong, long long, shmem)
SHMEM_TYPE_CSWAP(_int32, int32_t, shmemx)
SHMEM_TYPE_CSWAP(_int64, int64_t, shmemx)


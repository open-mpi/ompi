/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
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

#include "oshmem/mca/atomic/atomic.h"

/*
 * shmem_cswap performs an atomic conditional swap operation.
 * The conditional swap routines write value to address target on PE pe, and return the previous
 * contents of target. The replacement must occur only if cond is equal to target;
 * otherwise target is left unchanged. In either case, the routine must return the initial value
 * of target. The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */
#define SHMEM_TYPE_CSWAP(type_name, type, prefix, suffix)                          \
    type prefix##type_name##_##suffix(type *target, type cond, type value, int pe) \
    {                                                                              \
        int rc = OSHMEM_SUCCESS;                                                   \
        size_t size = 0;                                                           \
        type out_value;                                                            \
                                                                                   \
        RUNTIME_CHECK_INIT();                                                      \
        RUNTIME_CHECK_PE(pe);                                                      \
        RUNTIME_CHECK_ADDR(target);                                                \
                                                                                   \
        size = sizeof(out_value);                                                  \
        rc = MCA_ATOMIC_CALL(cswap(                                                \
            (void*)target,                                                         \
            (void*)&out_value,                                                     \
            (const void*)&cond,                                                    \
            (const void*)&value,                                                   \
            size,                                                                  \
            pe));                                                                  \
        RUNTIME_CHECK_RC(rc);                                                      \
                                                                                   \
        return out_value;                                                          \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_compare_swap = pshmem_int_atomic_compare_swap
#pragma weak shmem_long_atomic_compare_swap = pshmem_long_atomic_compare_swap
#pragma weak shmem_longlong_atomic_compare_swap = pshmem_longlong_atomic_compare_swap
#pragma weak shmemx_int32_cswap = pshmemx_int32_cswap
#pragma weak shmemx_int64_cswap = pshmemx_int64_cswap
/* Backward compatibility */
#pragma weak shmem_int_cswap = pshmem_int_atomic_compare_swap
#pragma weak shmem_long_cswap = pshmem_long_atomic_compare_swap
#pragma weak shmem_longlong_cswap = pshmem_longlong_atomic_compare_swap
#include "oshmem/shmem/c/profile/defines.h"
#else
#pragma weak shmem_int_cswap = shmem_int_atomic_compare_swap
#pragma weak shmem_long_cswap = shmem_long_atomic_compare_swap
#pragma weak shmem_longlong_cswap = shmem_longlong_atomic_compare_swap
#endif

SHMEM_TYPE_CSWAP(_int, int, shmem, atomic_compare_swap)
SHMEM_TYPE_CSWAP(_long, long, shmem, atomic_compare_swap)
SHMEM_TYPE_CSWAP(_longlong, long long, shmem, atomic_compare_swap)
SHMEM_TYPE_CSWAP(_int32, int32_t, shmemx, cswap)
SHMEM_TYPE_CSWAP(_int64, int64_t, shmemx, cswap)


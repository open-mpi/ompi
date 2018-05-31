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
 * shmem_swap performs an atomic swap operation.
 * The atomic swap routines write value to address target on PE pe, and return the previous
 * contents of target. The operation must be completed without the possibility of another
 * process updating target between the time of the fetch and the update.
 */
#define SHMEM_TYPE_SWAP(type_name, type, prefix, suffix)                \
    type prefix##type_name##_##suffix(type *target, type value, int pe) \
    {                                                                   \
        int rc = OSHMEM_SUCCESS;                                        \
        size_t size = 0;                                                \
        type out_value;                                                 \
                                                                        \
        RUNTIME_CHECK_INIT();                                           \
        RUNTIME_CHECK_PE(pe);                                           \
        RUNTIME_CHECK_ADDR(target);                                     \
                                                                        \
        size = sizeof(out_value);                                       \
        rc = MCA_ATOMIC_CALL(cswap(                                     \
            (void*)target,                                              \
            (void*)&out_value,                                          \
            NULL,                                                       \
            (const void*)&value,                                        \
            size,                                                       \
            pe));                                                       \
        RUNTIME_CHECK_RC(rc);                                           \
                                                                        \
        return out_value;                                               \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_swap      = pshmem_int_atomic_swap
#pragma weak shmem_long_atomic_swap     = pshmem_long_atomic_swap
#pragma weak shmem_longlong_atomic_swap = pshmem_longlong_atomic_swap
#pragma weak shmem_float_atomic_swap    = pshmem_float_atomic_swap
#pragma weak shmem_double_atomic_swap   = pshmem_double_atomic_swap
#pragma weak shmemx_int32_swap          = pshmemx_int32_swap
#pragma weak shmemx_int64_swap          = pshmemx_int64_swap
/* Backward compatibility */
#pragma weak shmem_int_swap      = pshmem_int_atomic_swap
#pragma weak shmem_long_swap     = pshmem_long_atomic_swap
#pragma weak shmem_longlong_swap = pshmem_longlong_atomic_swap
#pragma weak shmem_float_swap    = pshmem_float_atomic_swap
#pragma weak shmem_double_swap   = pshmem_double_atomic_swap
#include "oshmem/shmem/c/profile/defines.h"
#else
#pragma weak shmem_int_swap      = shmem_int_atomic_swap
#pragma weak shmem_long_swap     = shmem_long_atomic_swap
#pragma weak shmem_longlong_swap = shmem_longlong_atomic_swap
#pragma weak shmem_float_swap    = shmem_float_atomic_swap
#pragma weak shmem_double_swap   = shmem_double_atomic_swap
#endif

SHMEM_TYPE_SWAP(_int, int, shmem, atomic_swap)
SHMEM_TYPE_SWAP(_long, long, shmem, atomic_swap)
SHMEM_TYPE_SWAP(_longlong, long long, shmem, atomic_swap)
SHMEM_TYPE_SWAP(_float, float, shmem, atomic_swap)
SHMEM_TYPE_SWAP(_double, double, shmem, atomic_swap)
SHMEM_TYPE_SWAP(_int32, int32_t, shmemx, swap)
SHMEM_TYPE_SWAP(_int64, int64_t, shmemx, swap)

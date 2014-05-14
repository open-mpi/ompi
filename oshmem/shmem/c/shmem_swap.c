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

#include "oshmem/mca/atomic/atomic.h"

/*
 * shmem_swap performs an atomic swap operation. 
 * The atomic swap routines write value to address target on PE pe, and return the previous
 * contents of target. The operation must be completed without the possibility of another
 * process updating target between the time of the fetch and the update.
 */
#define SHMEM_TYPE_SWAP(type_name, type)    \
    type shmem##type_name##_swap(type *target, type value, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
        type out_value;                                             \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(out_value);                                   \
        rc = MCA_ATOMIC_CALL(cswap(                                 \
            (void*)target,                                          \
            (void*)&out_value,                                      \
            NULL,                                                   \
            (const void*)&value,                                    \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_swap = pshmem_swap
#pragma weak shmem_int_swap = pshmem_int_swap
#pragma weak shmem_long_swap = pshmem_long_swap
#pragma weak shmem_longlong_swap = pshmem_longlong_swap
#pragma weak shmem_float_swap = pshmem_float_swap
#pragma weak shmem_double_swap = pshmem_double_swap
#pragma weak shmem_int32_swap = pshmem_int32_swap
#pragma weak shmem_int64_swap = pshmem_int64_swap
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_SWAP(, long)
SHMEM_TYPE_SWAP(_int, int)
SHMEM_TYPE_SWAP(_long, long)
SHMEM_TYPE_SWAP(_longlong, long long)
SHMEM_TYPE_SWAP(_float, float)
SHMEM_TYPE_SWAP(_double, double)
SHMEM_TYPE_SWAP(_int32, int32_t)
SHMEM_TYPE_SWAP(_int64, int64_t)

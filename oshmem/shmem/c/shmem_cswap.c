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
 * shmem_cswap performs an atomic conditional swap operation. 
 * The conditional swap routines write value to address target on PE pe, and return the previous
 * contents of target. The replacement must occur only if cond is equal to target;
 * otherwise target is left unchanged. In either case, the routine must return the initial value
 * of target. The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */
#define SHMEM_TYPE_CSWAP(type_name, type)    \
    type shmem##type_name##_cswap(type *target, type cond, type value, int pe) \
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
            (const void*)&cond,                                     \
            (const void*)&value,                                    \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return out_value;                                           \
    }

SHMEM_TYPE_CSWAP(_int, int)
SHMEM_TYPE_CSWAP(_long, long)
SHMEM_TYPE_CSWAP(_longlong, long long)


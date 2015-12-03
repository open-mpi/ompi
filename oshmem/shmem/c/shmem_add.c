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
#include "oshmem/op/op.h"
#include "oshmem/mca/atomic/atomic.h"

/*
 * These routines perform an atomic add operation.
 * The atomic add routines add value to the data at address target on PE pe. The operation
 * must be completed without the possibility of another process updating target between the
 * time of the fetch and the update.
 */
#define SHMEM_TYPE_ADD(type_name, type, prefix)    \
    void prefix##type_name##_add(type *target, type value, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
        type out_value;                                             \
        oshmem_op_t* op = oshmem_op_sum##type_name;                 \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(out_value);                                   \
        rc = MCA_ATOMIC_CALL(fadd(                                  \
            (void*)target,                                          \
            NULL,                                                   \
            (const void*)&value,                                    \
            size,                                                   \
            pe,                                                     \
            op));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_add = pshmem_int_add
#pragma weak shmem_long_add = pshmem_long_add
#pragma weak shmem_longlong_add = pshmem_longlong_add
#pragma weak shmemx_int32_add = pshmemx_int32_add
#pragma weak shmemx_int64_add = pshmemx_int64_add
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_ADD(_int, int, shmem)
SHMEM_TYPE_ADD(_long, long, shmem)
SHMEM_TYPE_ADD(_longlong, long long, shmem)
SHMEM_TYPE_ADD(_int32, int32_t, shmemx)
SHMEM_TYPE_ADD(_int64, int64_t, shmemx)

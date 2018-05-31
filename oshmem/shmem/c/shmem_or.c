/*
 * Copyright (c) 2018      Mellanox Technologies, Inc.
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
 * These routines perform an atomic or operation.
 * The atomic "or" routines operates "or" value to the data at address target on PE pe.
 * The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */
#define SHMEM_TYPE_OR(type_name, type, prefix, suffix)                    \
    void prefix##type_name##_##suffix(type *target, type value, int pe)   \
    {                                                                     \
        int rc = OSHMEM_SUCCESS;                                          \
        size_t size = 0;                                                  \
        type out_value;                                                   \
        oshmem_op_t* op = oshmem_op_sum##type_name;                       \
                                                                          \
        RUNTIME_CHECK_INIT();                                             \
        RUNTIME_CHECK_PE(pe);                                             \
        RUNTIME_CHECK_ADDR(target);                                       \
                                                                          \
        size = sizeof(out_value);                                         \
        rc = MCA_ATOMIC_CALL(for(                                         \
            (void*)target,                                                \
            NULL,                                                         \
            (const void*)&value,                                          \
            size,                                                         \
            pe,                                                           \
            op));                                                         \
        RUNTIME_CHECK_RC(rc);                                             \
                                                                          \
        return ;                                                          \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_or = pshmem_int_atomic_or
#pragma weak shmem_long_atomic_or = pshmem_long_atomic_or
#pragma weak shmem_longlong_atomic_or = pshmem_longlong_atomic_or
#pragma weak shmemx_int32_or = pshmemx_int32_or
#pragma weak shmemx_int64_or = pshmemx_int64_or
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_OR(_int, int, shmem, atomic_or)
SHMEM_TYPE_OR(_long, long, shmem, atomic_or)
SHMEM_TYPE_OR(_longlong, long long, shmem, atomic_or)
SHMEM_TYPE_OR(_int32, int32_t, shmemx, or)
SHMEM_TYPE_OR(_int64, int64_t, shmemx, or)

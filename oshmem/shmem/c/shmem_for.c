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
 * These routines perform an atomic fetch-and-add operation.
 * The fetch and or routines retrieve the value at address target on PE pe, and update
 * target with the result of "or" value to the retrieved value. The operation must be completed
 * without the possibility of another process updating target between the time of the
 * fetch and the update.
 */
#define SHMEM_TYPE_FAND(type_name, type, prefix, suffix)                \
    type prefix##type_name##_##suffix(type *target, type value, int pe) \
    {                                                                   \
        int rc = OSHMEM_SUCCESS;                                        \
        size_t size = 0;                                                \
        type out_value;                                                 \
        oshmem_op_t* op = oshmem_op_sum##type_name;                     \
                                                                        \
        RUNTIME_CHECK_INIT();                                           \
        RUNTIME_CHECK_PE(pe);                                           \
        RUNTIME_CHECK_ADDR(target);                                     \
                                                                        \
        size = sizeof(out_value);                                       \
        rc = MCA_ATOMIC_CALL(for(                                       \
            (void*)target,                                              \
            (void*)&out_value,                                          \
            (const void*)&value,                                        \
            size,                                                       \
            pe,                                                         \
            op));                                                       \
        RUNTIME_CHECK_RC(rc);                                           \
                                                                        \
        return out_value;                                               \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_for = pshmem_int_atomic_for
#pragma weak shmem_long_atomic_for = pshmem_long_atomic_for
#pragma weak shmem_longlong_atomic_for = pshmem_longlong_atomic_for
#pragma weak shmemx_int32_for = pshmemx_int32_for
#pragma weak shmemx_int64_for = pshmemx_int64_for
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_FAND(_int, int, shmem, atomic_for)
SHMEM_TYPE_FAND(_long, long, shmem, atomic_for)
SHMEM_TYPE_FAND(_longlong, long long, shmem, atomic_for)
SHMEM_TYPE_FAND(_int32, int32_t, shmemx, for)
SHMEM_TYPE_FAND(_int64, int64_t, shmemx, for)

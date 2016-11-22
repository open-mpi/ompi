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
 * These routines perform an atomic fetch-and-add operation.
 * The fetch and add routines retrieve the value at address target on PE pe, and update
 * target with the result of adding value to the retrieved value. The operation must be completed
 * without the possibility of another process updating target between the time of the
 * fetch and the update.
 */
#define SHMEM_TYPE_FADD(type_name, type, prefix)    \
    type prefix##type_name##_fadd(type *target, type value, int pe) \
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
            (void*)&out_value,                                      \
            (const void*)&value,                                    \
            size,                                                   \
            pe,                                                     \
            op));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_fadd = pshmem_int_fadd
#pragma weak shmem_long_fadd = pshmem_long_fadd
#pragma weak shmem_longlong_fadd = pshmem_longlong_fadd
#pragma weak shmemx_int32_fadd = pshmemx_int32_fadd
#pragma weak shmemx_int64_fadd = pshmemx_int64_fadd
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_FADD(_int, int, shmem)
SHMEM_TYPE_FADD(_long, long, shmem)
SHMEM_TYPE_FADD(_longlong, long long, shmem)
SHMEM_TYPE_FADD(_int32, int32_t, shmemx)
SHMEM_TYPE_FADD(_int64, int64_t, shmemx)

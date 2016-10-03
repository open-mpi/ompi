/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
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
 * These routines perform an atomic fetch operation.
 * The fetch routines retrieve the value at address target on PE pe.
 * The operation must be completed without the possibility of another process 
 * updating target during the fetch.
 */
#define SHMEM_TYPE_FETCH(type_name, type, prefix)    \
    type prefix##type_name##_fetch(const type *target, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
        type out_value;                                             \
        type value = 0;                                             \
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
#pragma weak shmem_int_fetch = pshmem_int_fetch
#pragma weak shmem_long_fetch = pshmem_long_fetch
#pragma weak shmem_longlong_fetch = pshmem_longlong_fetch
#pragma weak shmem_double_fetch = pshmem_double_fetch
#pragma weak shmem_float_fetch = pshmem_float_fetch
#pragma weak shmemx_int32_fetch = pshmemx_int32_fetch
#pragma weak shmemx_int64_fetch = pshmemx_int64_fetch
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_FETCH(_int, int, shmem)
SHMEM_TYPE_FETCH(_long, long, shmem)
SHMEM_TYPE_FETCH(_longlong, long long, shmem)
SHMEM_TYPE_FETCH(_double, double, shmem)
SHMEM_TYPE_FETCH(_float, float, shmem)
SHMEM_TYPE_FETCH(_int32, int32_t, shmemx)
SHMEM_TYPE_FETCH(_int64, int64_t, shmemx)


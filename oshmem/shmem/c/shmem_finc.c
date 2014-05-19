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
 * These routines perform a fetch-and-increment operation. 
 * The fetch and increment routines retrieve the value at address target on PE pe, and update
 * target with the result of incrementing the retrieved value by one. The operation must be
 * completed without the possibility of another process updating target between the time of
 * the fetch and the update.
 */
#define SHMEM_TYPE_FINC(type_name, type)    \
    type shmem##type_name##_finc(type *target, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size;                                                \
        type value = 1;                                             \
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
#pragma weak shmem_int_finc = pshmem_int_finc
#pragma weak shmem_long_finc = pshmem_long_finc
#pragma weak shmem_longlong_finc = pshmem_longlong_finc
#pragma weak shmem_int32_finc = pshmem_int32_finc
#pragma weak shmem_int64_finc = pshmem_int64_finc
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_FINC(_int, int)
SHMEM_TYPE_FINC(_long, long)
SHMEM_TYPE_FINC(_longlong, long long)
SHMEM_TYPE_FINC(_int32, int32_t)
SHMEM_TYPE_FINC(_int64, int64_t)

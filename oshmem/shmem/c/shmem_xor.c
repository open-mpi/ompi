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
 * These routines perform an atomic xor operation.
 * The atomic "xor" routines operates "xor" value to the data at address target on PE pe.
 * The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */
#define SHMEM_TYPE_XOR(type_name, type, prefix, suffix)                   \
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
        rc = MCA_ATOMIC_CALL(fxor(                                        \
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
#pragma weak shmem_int_atomic_xor = pshmem_int_atomic_xor
#pragma weak shmem_long_atomic_xor = pshmem_long_atomic_xor
#pragma weak shmem_longlong_atomic_xor = pshmem_longlong_atomic_xor
#pragma weak shmemx_int32_xor = pshmemx_int32_xor
#pragma weak shmemx_int64_xor = pshmemx_int64_xor
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_XOR(_int, int, shmem, atomic_xor)
SHMEM_TYPE_XOR(_long, long, shmem, atomic_xor)
SHMEM_TYPE_XOR(_longlong, long long, shmem, atomic_xor)
SHMEM_TYPE_XOR(_int32, int32_t, shmemx, xor)
SHMEM_TYPE_XOR(_int64, int64_t, shmemx, xor)

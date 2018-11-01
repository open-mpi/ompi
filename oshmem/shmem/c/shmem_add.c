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
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"
#include "oshmem/op/op.h"
#include "oshmem/mca/atomic/atomic.h"

/*
 * These routines perform an atomic add operation.
 * The atomic add routines add value to the data at address target on PE pe. The operation
 * must be completed without the possibility of another process updating target between the
 * time of the fetch and the update.
 */
#define DO_SHMEM_TYPE_ATOMIC_ADD(ctx, type_name, type, target, value, pe) { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(value);                                       \
        rc = MCA_ATOMIC_CALL(add(                                   \
            ctx,                                                    \
            (void*)target,                                          \
            value,                                                  \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_ATOMIC_ADD(type_name, type, prefix)          \
    void prefix##_ctx##type_name##_atomic_add(shmem_ctx_t ctx, type *target, type value, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_ADD(ctx, type_name, type, target,      \
                                 value, pe);                        \
        return ;                                                    \
    }

#define SHMEM_TYPE_ATOMIC_ADD(type_name, type, prefix)              \
    void prefix##type_name##_atomic_add(type *target, type value, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_ADD(oshmem_ctx_default, type_name,     \
                                 type, target, value, pe);          \
                                                                    \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_int_atomic_add = pshmem_ctx_int_atomic_add
#pragma weak shmem_ctx_long_atomic_add = pshmem_ctx_long_atomic_add
#pragma weak shmem_ctx_longlong_atomic_add = pshmem_ctx_longlong_atomic_add
#pragma weak shmem_ctx_uint_atomic_add = pshmem_ctx_uint_atomic_add
#pragma weak shmem_ctx_ulong_atomic_add = pshmem_ctx_ulong_atomic_add
#pragma weak shmem_ctx_ulonglong_atomic_add = pshmem_ctx_ulonglong_atomic_add

#pragma weak shmem_int_atomic_add = pshmem_int_atomic_add
#pragma weak shmem_long_atomic_add = pshmem_long_atomic_add
#pragma weak shmem_longlong_atomic_add = pshmem_longlong_atomic_add
#pragma weak shmem_uint_atomic_add = pshmem_uint_atomic_add
#pragma weak shmem_ulong_atomic_add = pshmem_ulong_atomic_add
#pragma weak shmem_ulonglong_atomic_add = pshmem_ulonglong_atomic_add

#pragma weak shmem_int_add = pshmem_int_add
#pragma weak shmem_long_add = pshmem_long_add
#pragma weak shmem_longlong_add = pshmem_longlong_add

#pragma weak shmemx_int32_add = pshmemx_int32_add
#pragma weak shmemx_int64_add = pshmemx_int64_add
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_ADD(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_ADD(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_ADD(_ulonglong, unsigned long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_ADD(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_ADD(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_ADD(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_ADD(_int, int, shmem)
SHMEM_TYPE_ATOMIC_ADD(_long, long, shmem)
SHMEM_TYPE_ATOMIC_ADD(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_ADD(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_ADD(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_ADD(_ulonglong, unsigned long long, shmem)

/* deprecated APIs */
#define SHMEM_TYPE_ADD(type_name, type, prefix)                     \
    void prefix##type_name##_add(type *target, type value, int pe)  \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_ADD(oshmem_ctx_default, type_name,      \
                                 type, target, value, pe);          \
        return ;                                                    \
    }

SHMEM_TYPE_ADD(_int, int, shmem)
SHMEM_TYPE_ADD(_long, long, shmem)
SHMEM_TYPE_ADD(_longlong, long long, shmem)
SHMEM_TYPE_ADD(_int32, int32_t, shmemx)
SHMEM_TYPE_ADD(_int64, int64_t, shmemx)

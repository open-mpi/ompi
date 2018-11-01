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
 * These routines perform an atomic increment operation on a remote data object.
 * The atomic increment routines replace the value of target with its value incremented by
 * one. The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */
#define DO_SHMEM_TYPE_ATOMIC_INC(ctx, type_name, type, target, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
        type value = 1;                                             \
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

#define SHMEM_CTX_TYPE_ATOMIC_INC(type_name, type, prefix)          \
    void prefix##_ctx##type_name##_atomic_inc(shmem_ctx_t ctx, type *target, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_INC(ctx, type_name, type, target, pe); \
        return ;                                                    \
    }

#define SHMEM_TYPE_ATOMIC_INC(type_name, type, prefix)              \
    void prefix##type_name##_atomic_inc(type *target, int pe)       \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_INC(oshmem_ctx_default, type_name,     \
                                 type, target, pe);                 \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_int_atomic_inc = pshmem_ctx_int_atomic_inc
#pragma weak shmem_ctx_long_atomic_inc = pshmem_ctx_long_atomic_inc
#pragma weak shmem_ctx_longlong_atomic_inc = pshmem_ctx_longlong_atomic_inc
#pragma weak shmem_ctx_uint_atomic_inc = pshmem_ctx_uint_atomic_inc
#pragma weak shmem_ctx_ulong_atomic_inc = pshmem_ctx_ulong_atomic_inc
#pragma weak shmem_ctx_ulonglong_atomic_inc = pshmem_ctx_ulonglong_atomic_inc

#pragma weak shmem_int_atomic_inc = pshmem_int_atomic_inc
#pragma weak shmem_long_atomic_inc = pshmem_long_atomic_inc
#pragma weak shmem_longlong_atomic_inc = pshmem_longlong_atomic_inc
#pragma weak shmem_uint_atomic_inc = pshmem_uint_atomic_inc
#pragma weak shmem_ulong_atomic_inc = pshmem_ulong_atomic_inc
#pragma weak shmem_ulonglong_atomic_inc = pshmem_ulonglong_atomic_inc

#pragma weak shmem_int_inc = pshmem_int_inc
#pragma weak shmem_long_inc = pshmem_long_inc
#pragma weak shmem_longlong_inc = pshmem_longlong_inc

#pragma weak shmemx_int32_inc = pshmemx_int32_inc
#pragma weak shmemx_int64_inc = pshmemx_int64_inc
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_INC(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_INC(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_INC(_longlong, long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_INC(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_INC(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_INC(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_ATOMIC_INC(_int, int, shmem)
SHMEM_TYPE_ATOMIC_INC(_long, long, shmem)
SHMEM_TYPE_ATOMIC_INC(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_INC(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_INC(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_INC(_ulonglong, unsigned long long, shmem)

#define SHMEM_TYPE_INC(type_name, type, prefix)                     \
    void prefix##type_name##_inc(type *target, int pe)              \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_INC(oshmem_ctx_default, type_name,     \
                                 type, target, pe);                 \
        return ;                                                    \
    }

SHMEM_TYPE_INC(_int, int, shmem)
SHMEM_TYPE_INC(_long, long, shmem)
SHMEM_TYPE_INC(_longlong, long long, shmem)
SHMEM_TYPE_INC(_int32, int32_t, shmemx)
SHMEM_TYPE_INC(_int64, int64_t, shmemx)

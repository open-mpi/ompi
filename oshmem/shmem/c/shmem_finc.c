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
 * These routines perform a fetch-and-increment operation.
 * The fetch and increment routines retrieve the value at address target on PE pe, and update
 * target with the result of incrementing the retrieved value by one. The operation must be
 * completed without the possibility of another process updating target between the time of
 * the fetch and the update.
 */
#define DO_SHMEM_TYPE_ATOMIC_FETCH_INC(ctx, type_name, type, target, pe, out_value) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size;                                                \
        type value = 1;                                             \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(out_value);                                   \
        rc = MCA_ATOMIC_CALL(fadd(                                  \
            ctx,                                                    \
            (void*)target,                                          \
            (void*)&out_value,                                      \
            value,                                                  \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_ATOMIC_FETCH_INC(type_name, type, prefix)    \
    type prefix##_ctx##type_name##_atomic_fetch_inc(shmem_ctx_t ctx, type *target, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH_INC(ctx, type_name, type, target,\
                                  pe, out_value);                   \
        return out_value;                                           \
    }

#define SHMEM_TYPE_ATOMIC_FETCH_INC(type_name, type, prefix)        \
    type prefix##type_name##_atomic_fetch_inc(type *target, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH_INC(oshmem_ctx_default, type_name,\
                                  type, target, pe, out_value);     \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_int_atomic_fetch_inc = pshmem_ctx_int_atomic_fetch_inc
#pragma weak shmem_ctx_long_atomic_fetch_inc = pshmem_ctx_long_atomic_fetch_inc
#pragma weak shmem_ctx_longlong_atomic_fetch_inc = pshmem_ctx_longlong_atomic_fetch_inc
#pragma weak shmem_ctx_uint_atomic_fetch_inc = pshmem_ctx_uint_atomic_fetch_inc
#pragma weak shmem_ctx_ulong_atomic_fetch_inc = pshmem_ctx_ulong_atomic_fetch_inc
#pragma weak shmem_ctx_ulonglong_atomic_fetch_inc = pshmem_ctx_ulonglong_atomic_fetch_inc

#pragma weak shmem_int_atomic_fetch_inc = pshmem_int_atomic_fetch_inc
#pragma weak shmem_long_atomic_fetch_inc = pshmem_long_atomic_fetch_inc
#pragma weak shmem_longlong_atomic_fetch_inc = pshmem_longlong_atomic_fetch_inc
#pragma weak shmem_uint_atomic_fetch_inc = pshmem_uint_atomic_fetch_inc
#pragma weak shmem_ulong_atomic_fetch_inc = pshmem_ulong_atomic_fetch_inc
#pragma weak shmem_ulonglong_atomic_fetch_inc = pshmem_ulonglong_atomic_fetch_inc

#pragma weak shmem_int_finc = pshmem_int_finc
#pragma weak shmem_long_finc = pshmem_long_finc
#pragma weak shmem_longlong_finc = pshmem_longlong_finc

#pragma weak shmemx_int32_finc = pshmemx_int32_finc
#pragma weak shmemx_int64_finc = pshmemx_int64_finc
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_FETCH_INC(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_INC(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_INC(_longlong, long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_INC(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_INC(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_INC(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_INC(_int, int, shmem)
SHMEM_TYPE_ATOMIC_FETCH_INC(_long, long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_INC(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_INC(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_FETCH_INC(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_INC(_ulonglong, unsigned long long, shmem)

/* deprecated APIs */
#define SHMEM_TYPE_FINC(type_name, type, prefix)                    \
    type prefix##type_name##_finc(type *target, int pe)             \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH_INC(oshmem_ctx_default, type_name,   \
                                  type, target, pe, out_value);     \
        return out_value;                                           \
    }

SHMEM_TYPE_FINC(_int, int, shmem)
SHMEM_TYPE_FINC(_long, long, shmem)
SHMEM_TYPE_FINC(_longlong, long long, shmem)
SHMEM_TYPE_FINC(_int32, int32_t, shmemx)
SHMEM_TYPE_FINC(_int64, int64_t, shmemx)

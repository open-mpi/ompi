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
 * These routines perform an atomic fetch-and-add operation.
 * The fetch and add routines retrieve the value at address target on PE pe, and update
 * target with the result of adding value to the retrieved value. The operation must be completed
 * without the possibility of another process updating target between the time of the
 * fetch and the update.
 */
#define DO_SHMEM_TYPE_ATOMIC_FETCH_ADD(ctx, type_name, type, target, value, pe, out_value) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
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

#define SHMEM_CTX_TYPE_ATOMIC_FETCH_ADD(type_name, type, prefix)         \
    type prefix##_ctx##type_name##_atomic_fetch_add(shmem_ctx_t ctx, type *target, type value, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH_ADD(ctx, type_name, type, target,     \
                                  value, pe, out_value);            \
        return out_value;                                           \
    }

#define SHMEM_TYPE_ATOMIC_FETCH_ADD(type_name, type, prefix)             \
    type prefix##type_name##_atomic_fetch_add(type *target, type value, int pe)\
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH_ADD(oshmem_ctx_default, type_name,    \
                                  type, target, value, pe, out_value); \
        return out_value;                                           \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_int_atomic_fetch_add = pshmem_ctx_int_atomic_fetch_add
#pragma weak shmem_ctx_long_atomic_fetch_add = pshmem_ctx_long_atomic_fetch_add
#pragma weak shmem_ctx_longlong_atomic_fetch_add = pshmem_ctx_longlong_atomic_fetch_add
#pragma weak shmem_ctx_uint_atomic_fetch_add = pshmem_ctx_uint_atomic_fetch_add
#pragma weak shmem_ctx_ulong_atomic_fetch_add = pshmem_ctx_ulong_atomic_fetch_add
#pragma weak shmem_ctx_ulonglong_atomic_fetch_add = pshmem_ctx_ulonglong_atomic_fetch_add

#pragma weak shmem_int_atomic_fetch_add = pshmem_int_atomic_fetch_add
#pragma weak shmem_long_atomic_fetch_add = pshmem_long_atomic_fetch_add
#pragma weak shmem_longlong_atomic_fetch_add = pshmem_longlong_atomic_fetch_add
#pragma weak shmem_uint_atomic_fetch_add = pshmem_uint_atomic_fetch_add
#pragma weak shmem_ulong_atomic_fetch_add = pshmem_ulong_atomic_fetch_add
#pragma weak shmem_ulonglong_atomic_fetch_add = pshmem_ulonglong_atomic_fetch_add

#pragma weak shmem_int_fadd = pshmem_int_fadd
#pragma weak shmem_long_fadd = pshmem_long_fadd
#pragma weak shmem_longlong_fadd = pshmem_longlong_fadd

#pragma weak shmemx_int32_fadd = pshmemx_int32_fadd
#pragma weak shmemx_int64_fadd = pshmemx_int64_fadd
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_FETCH_ADD(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_ADD(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_ADD(_longlong, long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_ADD(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_ADD(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_FETCH_ADD(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_ADD(_int, int, shmem)
SHMEM_TYPE_ATOMIC_FETCH_ADD(_long, long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_ADD(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_ADD(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_FETCH_ADD(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_FETCH_ADD(_ulonglong, unsigned long long, shmem)

/* deprecated APIs */
#define SHMEM_TYPE_FADD(type_name, type, prefix)                    \
    type prefix##type_name##_fadd(type *target, type value, int pe) \
    {                                                               \
        type out_value;                                             \
        DO_SHMEM_TYPE_ATOMIC_FETCH_ADD(oshmem_ctx_default, type_name, \
                                       type, target, value, pe, out_value); \
        return out_value;                                           \
    }

SHMEM_TYPE_FADD(_int, int, shmem)
SHMEM_TYPE_FADD(_long, long, shmem)
SHMEM_TYPE_FADD(_longlong, long long, shmem)
SHMEM_TYPE_FADD(_int32, int32_t, shmemx)
SHMEM_TYPE_FADD(_int64, int64_t, shmemx)

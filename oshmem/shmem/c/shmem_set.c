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
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/atomic/atomic.h"

/*
 * shmem_set performs an atomic set operation.
 * The atomic set routines write value to address target on PE pe.
 * The operation must be completed without the possibility of another
 * process updating the target during the set.
 */
#define DO_SHMEM_TYPE_ATOMIC_SET(ctx, type, target, value, pe) do { \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
        type out_value;                                             \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = sizeof(out_value);                                   \
        rc = MCA_ATOMIC_CALL(swap(                                  \
            ctx,                                                    \
            (void*)target,                                          \
            (void*)&out_value,                                      \
            value,                                                  \
            size,                                                   \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
    } while (0)

#define SHMEM_CTX_TYPE_ATOMIC_SET(type_name, type, prefix)          \
    void prefix##_ctx##type_name##_atomic_set(shmem_ctx_t ctx, type *target, type value, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_SET(ctx, type, target, value, pe);     \
        return;                                                     \
    }

#define SHMEM_TYPE_ATOMIC_SET(type_name, type, prefix)              \
    void prefix##type_name##_atomic_set(type *target, type value, int pe) \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_SET(oshmem_ctx_default, type, target, value, pe); \
        return;                                                     \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_int_atomic_set = pshmem_ctx_int_atomic_set
#pragma weak shmem_ctx_long_atomic_set = pshmem_ctx_long_atomic_set
#pragma weak shmem_ctx_longlong_atomic_set = pshmem_ctx_longlong_atomic_set
#pragma weak shmem_ctx_uint_atomic_set = pshmem_ctx_uint_atomic_set
#pragma weak shmem_ctx_ulong_atomic_set = pshmem_ctx_ulong_atomic_set
#pragma weak shmem_ctx_ulonglong_atomic_set = pshmem_ctx_ulonglong_atomic_set
#pragma weak shmem_ctx_float_atomic_set = pshmem_ctx_float_atomic_set
#pragma weak shmem_ctx_double_atomic_set = pshmem_ctx_double_atomic_set

#pragma weak shmem_int_atomic_set = pshmem_int_atomic_set
#pragma weak shmem_long_atomic_set = pshmem_long_atomic_set
#pragma weak shmem_longlong_atomic_set = pshmem_longlong_atomic_set
#pragma weak shmem_uint_atomic_set = pshmem_uint_atomic_set
#pragma weak shmem_ulong_atomic_set = pshmem_ulong_atomic_set
#pragma weak shmem_ulonglong_atomic_set = pshmem_ulonglong_atomic_set
#pragma weak shmem_float_atomic_set = pshmem_float_atomic_set
#pragma weak shmem_double_atomic_set = pshmem_double_atomic_set

#pragma weak shmem_int_set = pshmem_int_set
#pragma weak shmem_long_set = pshmem_long_set
#pragma weak shmem_longlong_set = pshmem_longlong_set
#pragma weak shmem_float_set = pshmem_float_set
#pragma weak shmem_double_set = pshmem_double_set

#pragma weak shmemx_int32_set = pshmemx_int32_set
#pragma weak shmemx_int64_set = pshmemx_int64_set
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_CTX_TYPE_ATOMIC_SET(_int, int, shmem)
SHMEM_CTX_TYPE_ATOMIC_SET(_long, long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SET(_longlong, long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SET(_uint, unsigned int, shmem)
SHMEM_CTX_TYPE_ATOMIC_SET(_ulong, unsigned long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SET(_ulonglong, unsigned long long, shmem)
SHMEM_CTX_TYPE_ATOMIC_SET(_float, float, shmem)
SHMEM_CTX_TYPE_ATOMIC_SET(_double, double, shmem)
SHMEM_TYPE_ATOMIC_SET(_int, int, shmem)
SHMEM_TYPE_ATOMIC_SET(_long, long, shmem)
SHMEM_TYPE_ATOMIC_SET(_longlong, long long, shmem)
SHMEM_TYPE_ATOMIC_SET(_uint, unsigned int, shmem)
SHMEM_TYPE_ATOMIC_SET(_ulong, unsigned long, shmem)
SHMEM_TYPE_ATOMIC_SET(_ulonglong, unsigned long long, shmem)
SHMEM_TYPE_ATOMIC_SET(_float, float, shmem)
SHMEM_TYPE_ATOMIC_SET(_double, double, shmem)

/* deprecated APIs */
#define SHMEM_TYPE_SET(type_name, type, prefix)                     \
    void prefix##type_name##_set(type *target, type value, int pe)  \
    {                                                               \
        DO_SHMEM_TYPE_ATOMIC_SET(oshmem_ctx_default, type, target, value, pe); \
        return;                                                     \
    }
SHMEM_TYPE_SET(_int, int, shmem)
SHMEM_TYPE_SET(_long, long, shmem)
SHMEM_TYPE_SET(_longlong, long long, shmem)
SHMEM_TYPE_SET(_float, float, shmem)
SHMEM_TYPE_SET(_double, double, shmem)
SHMEM_TYPE_SET(_int32, int32_t, shmemx)
SHMEM_TYPE_SET(_int64, int64_t, shmemx)


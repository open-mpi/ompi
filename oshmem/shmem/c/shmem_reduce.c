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

#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/proc/proc.h"
#include "oshmem/proc/proc_group_cache.h"
#include "oshmem/op/op.h"

#if OSHMEM_GROUP_CACHE_ENABLED == 0
static bool __group_cache_enabled = false;
#else
static bool __group_cache_enabled = true;
#endif /* OSHMEM_GROUP_CACHE_ENABLED */

/*
 * The shared memory (SHMEM) reduction routines perform an associative binary operation
 * across symmetric arrays on multiple virtual PEs. 
 * This routine returns the result of performing a operation on the source data
 * object of every PE in the active set. The active set of PEs is defined by the triple PE_start,
 * logPE_stride and PE_size.
 */
#define SHMEM_TYPE_REDUCE_OP(name, type_name, type)    \
    void shmem##type_name##_##name##_to_all( type *target,                                  \
                                        type *source,                                       \
                                        int nreduce,                                        \
                                        int PE_start,                                       \
                                        int logPE_stride,                                   \
                                        int PE_size,                                        \
                                        type *pWrk,                                         \
                                        long *pSync )                                       \
{                                                                                           \
    int rc = OSHMEM_SUCCESS;                                                                \
    oshmem_group_t*  group = NULL;                                                          \
                                                                                            \
    RUNTIME_CHECK_INIT();                                                                   \
    RUNTIME_CHECK_ADDR(target);                                                             \
    RUNTIME_CHECK_ADDR(source);                                                             \
                                                                                            \
    {                                                                                       \
        /* Create group basing PE_start, logPE_stride and PE_size */                        \
        if (!__group_cache_enabled)                                                         \
        {                                                                                   \
            group = oshmem_proc_group_create(PE_start, (1 << logPE_stride), PE_size);       \
            if (!group)                                                                     \
                rc = OSHMEM_ERROR;                                                          \
        }                                                                                   \
        else                                                                                \
        {                                                                                   \
            group = find_group_in_cache(PE_start,logPE_stride,PE_size);                     \
            if (!group)                                                                     \
            {                                                                               \
                group = oshmem_proc_group_create(PE_start, (1 << logPE_stride), PE_size);   \
                if (!group)                                                                 \
                    rc = OSHMEM_ERROR;                                                      \
                cache_group(group,PE_start,logPE_stride,PE_size);                           \
            }                                                                               \
        }                                                                                   \
                                                                                            \
        /* Collective operation call */                                                     \
        if ( rc == OSHMEM_SUCCESS )                                                         \
        {                                                                                   \
        oshmem_op_t* op = oshmem_op_##name##type_name;                                      \
            size_t size = nreduce * op->dt_size;                                            \
                                                                                            \
            /* Call collective reduce operation */                                          \
            rc = group->g_scoll.scoll_reduce(                                               \
                                group,                                                      \
                                op,                                                         \
                                (void*)target,                                              \
                                (const void*)source,                                        \
                                size,                                                       \
                                pSync,                                                      \
                                (void*)pWrk,                                                \
                                SCOLL_DEFAULT_ALG );                                        \
        }                                                                                   \
                                                                                            \
        if ( !__group_cache_enabled && (rc == OSHMEM_SUCCESS ) )                            \
        {                                                                                   \
            oshmem_proc_group_destroy(group);                                               \
        }                                                                                   \
    }                                                                                       \
    RUNTIME_CHECK_RC(rc);                                                                   \
}

SHMEM_TYPE_REDUCE_OP(and, _short, short)
SHMEM_TYPE_REDUCE_OP(and, _int, int)
SHMEM_TYPE_REDUCE_OP(and, _long, long)
SHMEM_TYPE_REDUCE_OP(and, _longlong, long long)

SHMEM_TYPE_REDUCE_OP(or, _short, short)
SHMEM_TYPE_REDUCE_OP(or, _int, int)
SHMEM_TYPE_REDUCE_OP(or, _long, long)
SHMEM_TYPE_REDUCE_OP(or, _longlong, long long)

SHMEM_TYPE_REDUCE_OP(xor, _short, short)
SHMEM_TYPE_REDUCE_OP(xor, _int, int)
SHMEM_TYPE_REDUCE_OP(xor, _long, long)
SHMEM_TYPE_REDUCE_OP(xor, _longlong, long long)

SHMEM_TYPE_REDUCE_OP(max, _short, short)
SHMEM_TYPE_REDUCE_OP(max, _int, int)
SHMEM_TYPE_REDUCE_OP(max, _long, long)
SHMEM_TYPE_REDUCE_OP(max, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(max, _float, float)
SHMEM_TYPE_REDUCE_OP(max, _double, double)
SHMEM_TYPE_REDUCE_OP(max, _longdouble, long double)

SHMEM_TYPE_REDUCE_OP(min, _short, short)
SHMEM_TYPE_REDUCE_OP(min, _int, int)
SHMEM_TYPE_REDUCE_OP(min, _long, long)
SHMEM_TYPE_REDUCE_OP(min, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(min, _float, float)
SHMEM_TYPE_REDUCE_OP(min, _double, double)
SHMEM_TYPE_REDUCE_OP(min, _longdouble, long double)

SHMEM_TYPE_REDUCE_OP(sum, _short, short)
SHMEM_TYPE_REDUCE_OP(sum, _int, int)
SHMEM_TYPE_REDUCE_OP(sum, _long, long)
SHMEM_TYPE_REDUCE_OP(sum, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(sum, _float, float)
SHMEM_TYPE_REDUCE_OP(sum, _double, double)
SHMEM_TYPE_REDUCE_OP(sum, _longdouble, long double)
SHMEM_TYPE_REDUCE_OP(sum, _complexf, float complex)
SHMEM_TYPE_REDUCE_OP(sum, _complexd, double complex)

SHMEM_TYPE_REDUCE_OP(prod, _short, short)
SHMEM_TYPE_REDUCE_OP(prod, _int, int)
SHMEM_TYPE_REDUCE_OP(prod, _long, long)
SHMEM_TYPE_REDUCE_OP(prod, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(prod, _float, float)
SHMEM_TYPE_REDUCE_OP(prod, _double, double)
SHMEM_TYPE_REDUCE_OP(prod, _longdouble, long double)
SHMEM_TYPE_REDUCE_OP(prod, _complexf, float complex)
SHMEM_TYPE_REDUCE_OP(prod, _complexd, double complex)

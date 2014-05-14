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

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_short_and_to_all     =  pshmem_short_and_to_all
#pragma weak shmem_int_and_to_all       =  pshmem_int_and_to_all
#pragma weak shmem_long_and_to_all      =  pshmem_long_and_to_all
#pragma weak shmem_longlong_and_to_all  =  pshmem_longlong_and_to_all
#pragma weak shmem_int16_and_to_all     =  pshmem_int16_and_to_all
#pragma weak shmem_int32_and_to_all     =  pshmem_int32_and_to_all
#pragma weak shmem_int64_and_to_all     =  pshmem_int64_and_to_all

#pragma weak shmem_short_or_to_all      =  pshmem_short_or_to_all
#pragma weak shmem_int_or_to_all        =  pshmem_int_or_to_all
#pragma weak shmem_long_or_to_all       =  pshmem_long_or_to_all
#pragma weak shmem_longlong_or_to_all   =  pshmem_longlong_or_to_all
#pragma weak shmem_int16_or_to_all      =  pshmem_int16_or_to_all
#pragma weak shmem_int32_or_to_all      =  pshmem_int32_or_to_all
#pragma weak shmem_int64_or_to_all      =  pshmem_int64_or_to_all

#pragma weak shmem_short_xor_to_all     =  pshmem_short_xor_to_all
#pragma weak shmem_int_xor_to_all       =  pshmem_int_xor_to_all
#pragma weak shmem_long_xor_to_all      =  pshmem_long_xor_to_all
#pragma weak shmem_longlong_xor_to_all  =  pshmem_longlong_xor_to_all
#pragma weak shmem_int16_xor_to_all     =  pshmem_int16_xor_to_all
#pragma weak shmem_int32_xor_to_all     =  pshmem_int32_xor_to_all
#pragma weak shmem_int64_xor_to_all     =  pshmem_int64_xor_to_all

#pragma weak shmem_short_max_to_all     =  pshmem_short_max_to_all
#pragma weak shmem_int_max_to_all       =  pshmem_int_max_to_all
#pragma weak shmem_long_max_to_all      =  pshmem_long_max_to_all
#pragma weak shmem_longlong_max_to_all  =  pshmem_longlong_max_to_all
#pragma weak shmem_float_max_to_all     =  pshmem_float_max_to_all
#pragma weak shmem_double_max_to_all    =  pshmem_double_max_to_all
#pragma weak shmem_longdouble_max_to_all=  pshmem_longdouble_max_to_all
#pragma weak shmem_int16_max_to_all     =  pshmem_int16_max_to_all
#pragma weak shmem_int32_max_to_all     =  pshmem_int32_max_to_all
#pragma weak shmem_int64_max_to_all     =  pshmem_int64_max_to_all

#pragma weak shmem_short_min_to_all     =  pshmem_short_min_to_all
#pragma weak shmem_int_min_to_all       =  pshmem_int_min_to_all
#pragma weak shmem_long_min_to_all      =  pshmem_long_min_to_all
#pragma weak shmem_longlong_min_to_all  =  pshmem_longlong_min_to_all
#pragma weak shmem_float_min_to_all     =  pshmem_float_min_to_all
#pragma weak shmem_double_min_to_all    =  pshmem_double_min_to_all
#pragma weak shmem_longdouble_min_to_all=  pshmem_longdouble_min_to_all
#pragma weak shmem_int16_min_to_all     =  pshmem_int16_min_to_all
#pragma weak shmem_int32_min_to_all     =  pshmem_int32_min_to_all
#pragma weak shmem_int64_min_to_all     =  pshmem_int64_min_to_all

#pragma weak shmem_short_sum_to_all     =  pshmem_short_sum_to_all
#pragma weak shmem_int_sum_to_all       =  pshmem_int_sum_to_all
#pragma weak shmem_long_sum_to_all      =  pshmem_long_sum_to_all
#pragma weak shmem_longlong_sum_to_all  =  pshmem_longlong_sum_to_all
#pragma weak shmem_float_sum_to_all     =  pshmem_float_sum_to_all
#pragma weak shmem_double_sum_to_all    =  pshmem_double_sum_to_all
#pragma weak shmem_longdouble_sum_to_all=  pshmem_longdouble_sum_to_all
#pragma weak shmem_complexf_sum_to_all  =  pshmem_complexf_sum_to_all
#pragma weak shmem_complexd_sum_to_all  =  pshmem_complexd_sum_to_all
#pragma weak shmem_int16_sum_to_all     =  pshmem_int16_sum_to_all
#pragma weak shmem_int32_sum_to_all     =  pshmem_int32_sum_to_all
#pragma weak shmem_int64_sum_to_all     =  pshmem_int64_sum_to_all

#pragma weak shmem_short_prod_to_all    =  pshmem_short_prod_to_all
#pragma weak shmem_int_prod_to_all      =  pshmem_int_prod_to_all
#pragma weak shmem_long_prod_to_all     =  pshmem_long_prod_to_all
#pragma weak shmem_longlong_prod_to_all =  pshmem_longlong_prod_to_all
#pragma weak shmem_float_prod_to_all    =  pshmem_float_prod_to_all
#pragma weak shmem_double_prod_to_all   =  pshmem_double_prod_to_all
#pragma weak shmem_longdouble_prod_to_all = pshmem_longdouble_prod_to_all
#pragma weak shmem_complexf_prod_to_all =  pshmem_complexf_prod_to_all
#pragma weak shmem_complexd_prod_to_all =  pshmem_complexd_prod_to_all
#pragma weak shmem_int16_prod_to_all   =  pshmem_int16_prod_to_all
#pragma weak shmem_int32_prod_to_all   =  pshmem_int32_prod_to_all
#pragma weak shmem_int64_prod_to_all   =  pshmem_int64_prod_to_all
#include "oshmem/shmem/c/profile/defines.h"
#endif /* OSHMEM_PROFILING */

SHMEM_TYPE_REDUCE_OP(and, _short, short)
SHMEM_TYPE_REDUCE_OP(and, _int, int)
SHMEM_TYPE_REDUCE_OP(and, _long, long)
SHMEM_TYPE_REDUCE_OP(and, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(and, _int16, int16_t)
SHMEM_TYPE_REDUCE_OP(and, _int32, int32_t)
SHMEM_TYPE_REDUCE_OP(and, _int64, int64_t)

SHMEM_TYPE_REDUCE_OP(or, _short, short)
SHMEM_TYPE_REDUCE_OP(or, _int, int)
SHMEM_TYPE_REDUCE_OP(or, _long, long)
SHMEM_TYPE_REDUCE_OP(or, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(or, _int16, int16_t)
SHMEM_TYPE_REDUCE_OP(or, _int32, int32_t)
SHMEM_TYPE_REDUCE_OP(or, _int64, int64_t)

SHMEM_TYPE_REDUCE_OP(xor, _short, short)
SHMEM_TYPE_REDUCE_OP(xor, _int, int)
SHMEM_TYPE_REDUCE_OP(xor, _long, long)
SHMEM_TYPE_REDUCE_OP(xor, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(xor, _int16, int16_t)
SHMEM_TYPE_REDUCE_OP(xor, _int32, int32_t)
SHMEM_TYPE_REDUCE_OP(xor, _int64, int64_t)

SHMEM_TYPE_REDUCE_OP(max, _short, short)
SHMEM_TYPE_REDUCE_OP(max, _int, int)
SHMEM_TYPE_REDUCE_OP(max, _long, long)
SHMEM_TYPE_REDUCE_OP(max, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(max, _float, float)
SHMEM_TYPE_REDUCE_OP(max, _double, double)
SHMEM_TYPE_REDUCE_OP(max, _longdouble, long double)
SHMEM_TYPE_REDUCE_OP(max, _int16, int16_t)
SHMEM_TYPE_REDUCE_OP(max, _int32, int32_t)
SHMEM_TYPE_REDUCE_OP(max, _int64, int64_t)

SHMEM_TYPE_REDUCE_OP(min, _short, short)
SHMEM_TYPE_REDUCE_OP(min, _int, int)
SHMEM_TYPE_REDUCE_OP(min, _long, long)
SHMEM_TYPE_REDUCE_OP(min, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(min, _float, float)
SHMEM_TYPE_REDUCE_OP(min, _double, double)
SHMEM_TYPE_REDUCE_OP(min, _longdouble, long double)
SHMEM_TYPE_REDUCE_OP(min, _int16, int16_t)
SHMEM_TYPE_REDUCE_OP(min, _int32, int32_t)
SHMEM_TYPE_REDUCE_OP(min, _int64, int64_t)

SHMEM_TYPE_REDUCE_OP(sum, _short, short)
SHMEM_TYPE_REDUCE_OP(sum, _int, int)
SHMEM_TYPE_REDUCE_OP(sum, _long, long)
SHMEM_TYPE_REDUCE_OP(sum, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(sum, _float, float)
SHMEM_TYPE_REDUCE_OP(sum, _double, double)
SHMEM_TYPE_REDUCE_OP(sum, _longdouble, long double)
SHMEM_TYPE_REDUCE_OP(sum, _complexf, float complex)
SHMEM_TYPE_REDUCE_OP(sum, _complexd, double complex)
SHMEM_TYPE_REDUCE_OP(sum, _int16, int16_t)
SHMEM_TYPE_REDUCE_OP(sum, _int32, int32_t)
SHMEM_TYPE_REDUCE_OP(sum, _int64, int64_t)

SHMEM_TYPE_REDUCE_OP(prod, _short, short)
SHMEM_TYPE_REDUCE_OP(prod, _int, int)
SHMEM_TYPE_REDUCE_OP(prod, _long, long)
SHMEM_TYPE_REDUCE_OP(prod, _longlong, long long)
SHMEM_TYPE_REDUCE_OP(prod, _float, float)
SHMEM_TYPE_REDUCE_OP(prod, _double, double)
SHMEM_TYPE_REDUCE_OP(prod, _longdouble, long double)
SHMEM_TYPE_REDUCE_OP(prod, _complexf, float complex)
SHMEM_TYPE_REDUCE_OP(prod, _complexd, double complex)
SHMEM_TYPE_REDUCE_OP(prod, _int16, int16_t)
SHMEM_TYPE_REDUCE_OP(prod, _int32, int32_t)
SHMEM_TYPE_REDUCE_OP(prod, _int64, int64_t)

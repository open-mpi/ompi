/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
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

#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/proc/proc.h"
#include "oshmem/op/op.h"

/*
 * The shared memory (SHMEM) reduction routines perform an associative binary operation
 * across symmetric arrays on multiple virtual PEs.
 * This routine returns the result of performing a operation on the source data
 * object of every PE in the active set. The active set of PEs is defined by the triple PE_start,
 * logPE_stride and PE_size.
 */
#define SHMEM_TYPE_REDUCE_OP(name, type_name, type, prefix)                                 \
    void prefix##type_name##_##name##_to_all( type *target,                                 \
                                        const type *source,                                 \
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
    RUNTIME_CHECK_ADDR_SIZE(target, nreduce);                                               \
    RUNTIME_CHECK_ADDR_SIZE(source, nreduce);                                               \
                                                                                            \
    {                                                                                       \
        group = oshmem_proc_group_create_nofail(PE_start, 1<<logPE_stride, PE_size);        \
        oshmem_op_t* op = oshmem_op_##name##type_name;                                      \
        size_t size = nreduce * op->dt_size;                                                \
                                                                                            \
        /* Call collective reduce operation */                                              \
        rc = group->g_scoll.scoll_reduce(                                                   \
                group,                                                                      \
                op,                                                                         \
                (void*)target,                                                              \
                (const void*)source,                                                        \
                size,                                                                       \
                pSync,                                                                      \
                (void*)pWrk,                                                                \
                SCOLL_DEFAULT_ALG );                                                        \
                                                                                            \
        oshmem_proc_group_destroy(group);                                                   \
    }                                                                                       \
    RUNTIME_CHECK_RC(rc);                                                                   \
}

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_short_and_to_all     =  pshmem_short_and_to_all
#pragma weak shmem_int_and_to_all       =  pshmem_int_and_to_all
#pragma weak shmem_long_and_to_all      =  pshmem_long_and_to_all
#pragma weak shmem_longlong_and_to_all  =  pshmem_longlong_and_to_all
#pragma weak shmemx_int16_and_to_all    =  pshmemx_int16_and_to_all
#pragma weak shmemx_int32_and_to_all    =  pshmemx_int32_and_to_all
#pragma weak shmemx_int64_and_to_all    =  pshmemx_int64_and_to_all

#pragma weak shmem_short_or_to_all      =  pshmem_short_or_to_all
#pragma weak shmem_int_or_to_all        =  pshmem_int_or_to_all
#pragma weak shmem_long_or_to_all       =  pshmem_long_or_to_all
#pragma weak shmem_longlong_or_to_all   =  pshmem_longlong_or_to_all
#pragma weak shmemx_int16_or_to_all     =  pshmemx_int16_or_to_all
#pragma weak shmemx_int32_or_to_all     =  pshmemx_int32_or_to_all
#pragma weak shmemx_int64_or_to_all     =  pshmemx_int64_or_to_all

#pragma weak shmem_short_xor_to_all     =  pshmem_short_xor_to_all
#pragma weak shmem_int_xor_to_all       =  pshmem_int_xor_to_all
#pragma weak shmem_long_xor_to_all      =  pshmem_long_xor_to_all
#pragma weak shmem_longlong_xor_to_all  =  pshmem_longlong_xor_to_all
#pragma weak shmemx_int16_xor_to_all    =  pshmemx_int16_xor_to_all
#pragma weak shmemx_int32_xor_to_all    =  pshmemx_int32_xor_to_all
#pragma weak shmemx_int64_xor_to_all    =  pshmemx_int64_xor_to_all

#pragma weak shmem_short_max_to_all     =  pshmem_short_max_to_all
#pragma weak shmem_int_max_to_all       =  pshmem_int_max_to_all
#pragma weak shmem_long_max_to_all      =  pshmem_long_max_to_all
#pragma weak shmem_longlong_max_to_all  =  pshmem_longlong_max_to_all
#pragma weak shmem_float_max_to_all     =  pshmem_float_max_to_all
#pragma weak shmem_double_max_to_all    =  pshmem_double_max_to_all
#pragma weak shmem_longdouble_max_to_all=  pshmem_longdouble_max_to_all
#pragma weak shmemx_int16_max_to_all    =  pshmemx_int16_max_to_all
#pragma weak shmemx_int32_max_to_all    =  pshmemx_int32_max_to_all
#pragma weak shmemx_int64_max_to_all    =  pshmemx_int64_max_to_all

#pragma weak shmem_short_min_to_all     =  pshmem_short_min_to_all
#pragma weak shmem_int_min_to_all       =  pshmem_int_min_to_all
#pragma weak shmem_long_min_to_all      =  pshmem_long_min_to_all
#pragma weak shmem_longlong_min_to_all  =  pshmem_longlong_min_to_all
#pragma weak shmem_float_min_to_all     =  pshmem_float_min_to_all
#pragma weak shmem_double_min_to_all    =  pshmem_double_min_to_all
#pragma weak shmem_longdouble_min_to_all=  pshmem_longdouble_min_to_all
#pragma weak shmemx_int16_min_to_all    =  pshmemx_int16_min_to_all
#pragma weak shmemx_int32_min_to_all    =  pshmemx_int32_min_to_all
#pragma weak shmemx_int64_min_to_all    =  pshmemx_int64_min_to_all

#pragma weak shmem_short_sum_to_all     =  pshmem_short_sum_to_all
#pragma weak shmem_int_sum_to_all       =  pshmem_int_sum_to_all
#pragma weak shmem_long_sum_to_all      =  pshmem_long_sum_to_all
#pragma weak shmem_longlong_sum_to_all  =  pshmem_longlong_sum_to_all
#pragma weak shmem_float_sum_to_all     =  pshmem_float_sum_to_all
#pragma weak shmem_double_sum_to_all    =  pshmem_double_sum_to_all
#pragma weak shmem_longdouble_sum_to_all=  pshmem_longdouble_sum_to_all
#pragma weak shmem_complexf_sum_to_all  =  pshmem_complexf_sum_to_all
#pragma weak shmem_complexd_sum_to_all  =  pshmem_complexd_sum_to_all
#pragma weak shmemx_int16_sum_to_all    =  pshmemx_int16_sum_to_all
#pragma weak shmemx_int32_sum_to_all    =  pshmemx_int32_sum_to_all
#pragma weak shmemx_int64_sum_to_all    =  pshmemx_int64_sum_to_all

#pragma weak shmem_short_prod_to_all    =  pshmem_short_prod_to_all
#pragma weak shmem_int_prod_to_all      =  pshmem_int_prod_to_all
#pragma weak shmem_long_prod_to_all     =  pshmem_long_prod_to_all
#pragma weak shmem_longlong_prod_to_all =  pshmem_longlong_prod_to_all
#pragma weak shmem_float_prod_to_all    =  pshmem_float_prod_to_all
#pragma weak shmem_double_prod_to_all   =  pshmem_double_prod_to_all
#pragma weak shmem_longdouble_prod_to_all = pshmem_longdouble_prod_to_all
#pragma weak shmem_complexf_prod_to_all =  pshmem_complexf_prod_to_all
#pragma weak shmem_complexd_prod_to_all =  pshmem_complexd_prod_to_all
#pragma weak shmemx_int16_prod_to_all   =  pshmemx_int16_prod_to_all
#pragma weak shmemx_int32_prod_to_all   =  pshmemx_int32_prod_to_all
#pragma weak shmemx_int64_prod_to_all   =  pshmemx_int64_prod_to_all

/* Teams reduction: AND */
#pragma weak shmem_uchar_and_reduce          		= pshmem_uchar_and_reduce
#pragma weak shmem_ushort_and_reduce         		= pshmem_ushort_and_reduce
#pragma weak shmem_uint_and_reduce           		= pshmem_uint_and_reduce
#pragma weak shmem_ulong_and_reduce          		= pshmem_ulong_and_reduce
#pragma weak shmem_ulonglong_and_reduce      		= pshmem_ulonglong_and_reduce
#pragma weak shmem_int_and_reduce                  = pshmem_int_and_reduce
#pragma weak shmem_longlong_and_reduce             = pshmem_longlong_and_reduce
#pragma weak shmem_int8_and_reduce           		= pshmem_int8_and_reduce
#pragma weak shmem_int16_and_reduce          		= pshmem_int16_and_reduce
#pragma weak shmem_int32_and_reduce          		= pshmem_int32_and_reduce
#pragma weak shmem_int64_and_reduce          		= pshmem_int64_and_reduce
#pragma weak shmem_uint8_and_reduce          		= pshmem_uint8_and_reduce
#pragma weak shmem_uint16_and_reduce         		= pshmem_uint16_and_reduce
#pragma weak shmem_uint32_and_reduce         		= pshmem_uint32_and_reduce
#pragma weak shmem_uint64_and_reduce         		= pshmem_uint64_and_reduce
#pragma weak shmem_size_and_reduce           		= pshmem_size_and_reduce

/* Teams reduction: OR */
#pragma weak shmem_uchar_or_reduce          		= pshmem_uchar_or_reduce
#pragma weak shmem_ushort_or_reduce         		= pshmem_ushort_or_reduce
#pragma weak shmem_uint_or_reduce           		= pshmem_uint_or_reduce
#pragma weak shmem_ulong_or_reduce          		= pshmem_ulong_or_reduce
#pragma weak shmem_ulonglong_or_reduce      		= pshmem_ulonglong_or_reduce
#pragma weak shmem_int8_or_reduce           		= pshmem_int8_or_reduce
#pragma weak shmem_int16_or_reduce          		= pshmem_int16_or_reduce
#pragma weak shmem_int32_or_reduce          		= pshmem_int32_or_reduce
#pragma weak shmem_int64_or_reduce          		= pshmem_int64_or_reduce
#pragma weak shmem_uint8_or_reduce          		= pshmem_uint8_or_reduce
#pragma weak shmem_uint16_or_reduce         		= pshmem_uint16_or_reduce
#pragma weak shmem_uint32_or_reduce         		= pshmem_uint32_or_reduce
#pragma weak shmem_uint64_or_reduce         		= pshmem_uint64_or_reduce
#pragma weak shmem_size_or_reduce           		= pshmem_size_or_reduce


/* Teams reduction: XOR */
#pragma weak shmem_uchar_xor_reduce          		= pshmem_uchar_xor_reduce
#pragma weak shmem_ushort_xor_reduce         		= pshmem_ushort_xor_reduce
#pragma weak shmem_uint_xor_reduce           		= pshmem_uint_xor_reduce
#pragma weak shmem_ulong_xor_reduce          		= pshmem_ulong_xor_reduce
#pragma weak shmem_ulonglong_xor_reduce      		= pshmem_ulonglong_xor_reduce
#pragma weak shmem_int8_xor_reduce           		= pshmem_int8_xor_reduce
#pragma weak shmem_int16_xor_reduce          		= pshmem_int16_xor_reduce
#pragma weak shmem_int32_xor_reduce          		= pshmem_int32_xor_reduce
#pragma weak shmem_int64_xor_reduce          		= pshmem_int64_xor_reduce
#pragma weak shmem_uint8_xor_reduce          		= pshmem_uint8_xor_reduce
#pragma weak shmem_uint16_xor_reduce         		= pshmem_uint16_xor_reduce
#pragma weak shmem_uint32_xor_reduce         		= pshmem_uint32_xor_reduce
#pragma weak shmem_uint64_xor_reduce         		= pshmem_uint64_xor_reduce
#pragma weak shmem_size_xor_reduce           		= pshmem_size_xor_reduce


/* Teams reduction: MAX */
#pragma weak shmem_char_max_reduce           		= pshmem_char_max_reduce
#pragma weak shmem_short_max_reduce          		= pshmem_short_max_reduce
#pragma weak shmem_int_max_reduce            		= pshmem_int_max_reduce
#pragma weak shmem_long_max_reduce           		= pshmem_long_max_reduce
#pragma weak shmem_float_max_reduce          		= pshmem_float_max_reduce
#pragma weak shmem_double_max_reduce         		= pshmem_double_max_reduce
#pragma weak shmem_longlong_max_reduce       		= pshmem_longlong_max_reduce
#pragma weak shmem_schar_max_reduce          		= pshmem_schar_max_reduce
#pragma weak shmem_longdouble_max_reduce     		= pshmem_longdouble_max_reduce
#pragma weak shmem_ptrdiff_max_reduce        		= pshmem_ptrdiff_max_reduce
#pragma weak shmem_uchar_max_reduce          		= pshmem_uchar_max_reduce
#pragma weak shmem_ushort_max_reduce         		= pshmem_ushort_max_reduce
#pragma weak shmem_uint_max_reduce           		= pshmem_uint_max_reduce
#pragma weak shmem_ulong_max_reduce          		= pshmem_ulong_max_reduce
#pragma weak shmem_ulonglong_max_reduce      		= pshmem_ulonglong_max_reduce
#pragma weak shmem_int8_max_reduce           		= pshmem_int8_max_reduce
#pragma weak shmem_int16_max_reduce          		= pshmem_int16_max_reduce
#pragma weak shmem_int32_max_reduce          		= pshmem_int32_max_reduce
#pragma weak shmem_int64_max_reduce          		= pshmem_int64_max_reduce
#pragma weak shmem_uint8_max_reduce          		= pshmem_uint8_max_reduce
#pragma weak shmem_uint16_max_reduce         		= pshmem_uint16_max_reduce
#pragma weak shmem_uint32_max_reduce         		= pshmem_uint32_max_reduce
#pragma weak shmem_uint64_max_reduce         		= pshmem_uint64_max_reduce
#pragma weak shmem_size_max_reduce           		= pshmem_size_max_reduce


/* Teams reduction: MIN */
#pragma weak shmem_char_min_reduce           		= pshmem_char_min_reduce
#pragma weak shmem_short_min_reduce          		= pshmem_short_min_reduce
#pragma weak shmem_int_min_reduce            		= pshmem_int_min_reduce
#pragma weak shmem_long_min_reduce           		= pshmem_long_min_reduce
#pragma weak shmem_float_min_reduce          		= pshmem_float_min_reduce
#pragma weak shmem_double_min_reduce         		= pshmem_double_min_reduce
#pragma weak shmem_longlong_min_reduce       		= pshmem_longlong_min_reduce
#pragma weak shmem_schar_min_reduce          		= pshmem_schar_min_reduce
#pragma weak shmem_longdouble_min_reduce     		= pshmem_longdouble_min_reduce
#pragma weak shmem_ptrdiff_min_reduce        		= pshmem_ptrdiff_min_reduce
#pragma weak shmem_uchar_min_reduce          		= pshmem_uchar_min_reduce
#pragma weak shmem_ushort_min_reduce         		= pshmem_ushort_min_reduce
#pragma weak shmem_uint_min_reduce           		= pshmem_uint_min_reduce
#pragma weak shmem_ulong_min_reduce          		= pshmem_ulong_min_reduce
#pragma weak shmem_ulonglong_min_reduce      		= pshmem_ulonglong_min_reduce
#pragma weak shmem_int8_min_reduce           		= pshmem_int8_min_reduce
#pragma weak shmem_int16_min_reduce          		= pshmem_int16_min_reduce
#pragma weak shmem_int32_min_reduce          		= pshmem_int32_min_reduce
#pragma weak shmem_int64_min_reduce          		= pshmem_int64_min_reduce
#pragma weak shmem_uint8_min_reduce          		= pshmem_uint8_min_reduce
#pragma weak shmem_uint16_min_reduce         		= pshmem_uint16_min_reduce
#pragma weak shmem_uint32_min_reduce         		= pshmem_uint32_min_reduce
#pragma weak shmem_uint64_min_reduce         		= pshmem_uint64_min_reduce
#pragma weak shmem_size_min_reduce           		= pshmem_size_min_reduce


/* Teams reduction: SUM */
#pragma weak shmem_char_sum_reduce           		= pshmem_char_sum_reduce
#pragma weak shmem_short_sum_reduce          		= pshmem_short_sum_reduce
#pragma weak shmem_int_sum_reduce            		= pshmem_int_sum_reduce
#pragma weak shmem_long_sum_reduce           		= pshmem_long_sum_reduce
#pragma weak shmem_float_sum_reduce          		= pshmem_float_sum_reduce
#pragma weak shmem_double_sum_reduce         		= pshmem_double_sum_reduce
#pragma weak shmem_longlong_sum_reduce       		= pshmem_longlong_sum_reduce
#pragma weak shmem_schar_sum_reduce          		= pshmem_schar_sum_reduce
#pragma weak shmem_longdouble_sum_reduce     		= pshmem_longdouble_sum_reduce
#pragma weak shmem_ptrdiff_sum_reduce        		= pshmem_ptrdiff_sum_reduce
#pragma weak shmem_uchar_sum_reduce          		= pshmem_uchar_sum_reduce
#pragma weak shmem_ushort_sum_reduce         		= pshmem_ushort_sum_reduce
#pragma weak shmem_uint_sum_reduce           		= pshmem_uint_sum_reduce
#pragma weak shmem_ulong_sum_reduce          		= pshmem_ulong_sum_reduce
#pragma weak shmem_ulonglong_sum_reduce      		= pshmem_ulonglong_sum_reduce
#pragma weak shmem_int8_sum_reduce           		= pshmem_int8_sum_reduce
#pragma weak shmem_int16_sum_reduce          		= pshmem_int16_sum_reduce
#pragma weak shmem_int32_sum_reduce          		= pshmem_int32_sum_reduce
#pragma weak shmem_int64_sum_reduce          		= pshmem_int64_sum_reduce
#pragma weak shmem_uint8_sum_reduce          		= pshmem_uint8_sum_reduce
#pragma weak shmem_uint16_sum_reduce         		= pshmem_uint16_sum_reduce
#pragma weak shmem_uint32_sum_reduce         		= pshmem_uint32_sum_reduce
#pragma weak shmem_uint64_sum_reduce         		= pshmem_uint64_sum_reduce
#pragma weak shmem_size_sum_reduce           		= pshmem_size_sum_reduce
#pragma weak shmem_complexd_sum_reduce       		= pshmem_complexd_sum_reduce
#pragma weak shmem_complexf_sum_reduce       		= pshmem_complexf_sum_reduce


/* Teams reduction: PROD */
#pragma weak shmem_char_prod_reduce           		= pshmem_char_prod_reduce
#pragma weak shmem_short_prod_reduce          		= pshmem_short_prod_reduce
#pragma weak shmem_int_prod_reduce            		= pshmem_int_prod_reduce
#pragma weak shmem_long_prod_reduce           		= pshmem_long_prod_reduce
#pragma weak shmem_float_prod_reduce          		= pshmem_float_prod_reduce
#pragma weak shmem_double_prod_reduce         		= pshmem_double_prod_reduce
#pragma weak shmem_longlong_prod_reduce       		= pshmem_longlong_prod_reduce
#pragma weak shmem_schar_prod_reduce          		= pshmem_schar_prod_reduce
#pragma weak shmem_longdouble_prod_reduce     		= pshmem_longdouble_prod_reduce
#pragma weak shmem_ptrdiff_prod_reduce        		= pshmem_ptrdiff_prod_reduce
#pragma weak shmem_uchar_prod_reduce          		= pshmem_uchar_prod_reduce
#pragma weak shmem_ushort_prod_reduce         		= pshmem_ushort_prod_reduce
#pragma weak shmem_uint_prod_reduce           		= pshmem_uint_prod_reduce
#pragma weak shmem_ulong_prod_reduce          		= pshmem_ulong_prod_reduce
#pragma weak shmem_ulonglong_prod_reduce      		= pshmem_ulonglong_prod_reduce
#pragma weak shmem_int8_prod_reduce           		= pshmem_int8_prod_reduce
#pragma weak shmem_int16_prod_reduce          		= pshmem_int16_prod_reduce
#pragma weak shmem_int32_prod_reduce          		= pshmem_int32_prod_reduce
#pragma weak shmem_int64_prod_reduce          		= pshmem_int64_prod_reduce
#pragma weak shmem_uint8_prod_reduce          		= pshmem_uint8_prod_reduce
#pragma weak shmem_uint16_prod_reduce         		= pshmem_uint16_prod_reduce
#pragma weak shmem_uint32_prod_reduce         		= pshmem_uint32_prod_reduce
#pragma weak shmem_uint64_prod_reduce         		= pshmem_uint64_prod_reduce
#pragma weak shmem_size_prod_reduce           		= pshmem_size_prod_reduce
#pragma weak shmem_complexd_prod_reduce       		= pshmem_complexd_prod_reduce
#pragma weak shmem_complexf_prod_reduce       		= pshmem_complexf_prod_reduce



#include "oshmem/shmem/c/profile-defines.h"
#endif /* OSHMEM_PROFILING */

SHMEM_TYPE_REDUCE_OP(and, _short, short, shmem)
SHMEM_TYPE_REDUCE_OP(and, _int, int, shmem)
SHMEM_TYPE_REDUCE_OP(and, _long, long, shmem)
SHMEM_TYPE_REDUCE_OP(and, _longlong, long long, shmem)
SHMEM_TYPE_REDUCE_OP(and, _int16, int16_t, shmemx)
SHMEM_TYPE_REDUCE_OP(and, _int32, int32_t, shmemx)
SHMEM_TYPE_REDUCE_OP(and, _int64, int64_t, shmemx)

SHMEM_TYPE_REDUCE_OP(or, _short, short, shmem)
SHMEM_TYPE_REDUCE_OP(or, _int, int, shmem)
SHMEM_TYPE_REDUCE_OP(or, _long, long, shmem)
SHMEM_TYPE_REDUCE_OP(or, _longlong, long long, shmem)
SHMEM_TYPE_REDUCE_OP(or, _int16, int16_t, shmemx)
SHMEM_TYPE_REDUCE_OP(or, _int32, int32_t, shmemx)
SHMEM_TYPE_REDUCE_OP(or, _int64, int64_t, shmemx)

SHMEM_TYPE_REDUCE_OP(xor, _short, short, shmem)
SHMEM_TYPE_REDUCE_OP(xor, _int, int, shmem)
SHMEM_TYPE_REDUCE_OP(xor, _long, long, shmem)
SHMEM_TYPE_REDUCE_OP(xor, _longlong, long long, shmem)
SHMEM_TYPE_REDUCE_OP(xor, _int16, int16_t, shmemx)
SHMEM_TYPE_REDUCE_OP(xor, _int32, int32_t, shmemx)
SHMEM_TYPE_REDUCE_OP(xor, _int64, int64_t, shmemx)

SHMEM_TYPE_REDUCE_OP(max, _short, short, shmem)
SHMEM_TYPE_REDUCE_OP(max, _int, int, shmem)
SHMEM_TYPE_REDUCE_OP(max, _long, long, shmem)
SHMEM_TYPE_REDUCE_OP(max, _longlong, long long, shmem)
SHMEM_TYPE_REDUCE_OP(max, _float, float, shmem)
SHMEM_TYPE_REDUCE_OP(max, _double, double, shmem)
SHMEM_TYPE_REDUCE_OP(max, _longdouble, long double, shmem)
SHMEM_TYPE_REDUCE_OP(max, _int16, int16_t, shmemx)
SHMEM_TYPE_REDUCE_OP(max, _int32, int32_t, shmemx)
SHMEM_TYPE_REDUCE_OP(max, _int64, int64_t, shmemx)

SHMEM_TYPE_REDUCE_OP(min, _short, short, shmem)
SHMEM_TYPE_REDUCE_OP(min, _int, int, shmem)
SHMEM_TYPE_REDUCE_OP(min, _long, long, shmem)
SHMEM_TYPE_REDUCE_OP(min, _longlong, long long, shmem)
SHMEM_TYPE_REDUCE_OP(min, _float, float, shmem)
SHMEM_TYPE_REDUCE_OP(min, _double, double, shmem)
SHMEM_TYPE_REDUCE_OP(min, _longdouble, long double, shmem)
SHMEM_TYPE_REDUCE_OP(min, _int16, int16_t, shmemx)
SHMEM_TYPE_REDUCE_OP(min, _int32, int32_t, shmemx)
SHMEM_TYPE_REDUCE_OP(min, _int64, int64_t, shmemx)

SHMEM_TYPE_REDUCE_OP(sum, _short, short, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _int, int, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _long, long, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _longlong, long long, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _float, float, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _double, double, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _longdouble, long double, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _complexf, float complex, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _complexd, double complex, shmem)
SHMEM_TYPE_REDUCE_OP(sum, _int16, int16_t, shmemx)
SHMEM_TYPE_REDUCE_OP(sum, _int32, int32_t, shmemx)
SHMEM_TYPE_REDUCE_OP(sum, _int64, int64_t, shmemx)

SHMEM_TYPE_REDUCE_OP(prod, _short, short, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _int, int, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _long, long, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _longlong, long long, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _float, float, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _double, double, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _longdouble, long double, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _complexf, float complex, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _complexd, double complex, shmem)
SHMEM_TYPE_REDUCE_OP(prod, _int16, int16_t, shmemx)
SHMEM_TYPE_REDUCE_OP(prod, _int32, int32_t, shmemx)
SHMEM_TYPE_REDUCE_OP(prod, _int64, int64_t, shmemx)


#define SHMEM_TYPE_TEAM_REDUCE_OP(_op, type_name, type, op_code, code)                      \
    int shmem##type_name##_##_op##_reduce( shmem_team_t team, type *dest, const type *source, size_t nreduce)                                 \
{                                                                                           \
    int rc = OSHMEM_SUCCESS;                                                                \
                                                                                            \
    RUNTIME_CHECK_INIT();                                                                   \
    RUNTIME_CHECK_ADDR_SIZE(dest, nreduce);                                                 \
    RUNTIME_CHECK_ADDR_SIZE(source, nreduce);                                               \
                                                                                            \
    {                                                                                       \
                                                                                            \
        /* Call collective reduce operation */                                              \
        rc = MCA_SPML_CALL(team_reduce(                                                     \
            team, (void*)dest, (void*)source, nreduce, op_code, code));                     \
                                                                                            \
    }                                                                                       \
    RUNTIME_CHECK_RC(rc);                                                                   \
                                                                                            \
    return rc;                                                                              \
}

SHMEM_TYPE_TEAM_REDUCE_OP(and, _uchar, unsigned char, OSHMEM_OP_AND, SHMEM_UCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _ushort, unsigned short, OSHMEM_OP_AND, SHMEM_USHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _uint, unsigned int, OSHMEM_OP_AND, SHMEM_UINT)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _ulong, unsigned long, OSHMEM_OP_AND, SHMEM_ULONG)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _ulonglong, unsigned long long, OSHMEM_OP_AND, SHMEM_ULLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _int, int, OSHMEM_OP_AND, SHMEM_INT)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _longlong, long long, OSHMEM_OP_AND, SHMEM_LLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _int8, int8_t, OSHMEM_OP_AND, SHMEM_INT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _int16, int16_t, OSHMEM_OP_AND, SHMEM_INT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _int32, int32_t, OSHMEM_OP_AND, SHMEM_INT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _int64, int64_t, OSHMEM_OP_AND, SHMEM_INT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _uint8, uint8_t, OSHMEM_OP_AND, SHMEM_UINT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _uint16, uint16_t, OSHMEM_OP_AND, SHMEM_UINT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _uint32, uint32_t, OSHMEM_OP_AND, SHMEM_UINT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _uint64, uint64_t, OSHMEM_OP_AND, SHMEM_UINT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(and, _size, size_t, OSHMEM_OP_AND, SHMEM_SIZE_T)

SHMEM_TYPE_TEAM_REDUCE_OP(or, _uchar, unsigned char, OSHMEM_OP_OR, SHMEM_UCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _ushort, unsigned short, OSHMEM_OP_OR, SHMEM_USHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _uint, unsigned int, OSHMEM_OP_OR, SHMEM_UINT)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _ulong, unsigned long, OSHMEM_OP_OR, SHMEM_ULONG)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _ulonglong, unsigned long long, OSHMEM_OP_OR, SHMEM_ULLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _int, int, OSHMEM_OP_OR, SHMEM_INT)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _longlong, long long, OSHMEM_OP_OR, SHMEM_LLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _int8, int8_t, OSHMEM_OP_OR, SHMEM_INT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _int16, int16_t, OSHMEM_OP_OR, SHMEM_INT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _int32, int32_t, OSHMEM_OP_OR, SHMEM_INT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _int64, int64_t, OSHMEM_OP_OR, SHMEM_INT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _uint8, uint8_t, OSHMEM_OP_OR, SHMEM_UINT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _uint16, uint16_t, OSHMEM_OP_OR, SHMEM_UINT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _uint32, uint32_t, OSHMEM_OP_OR, SHMEM_UINT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _uint64, uint64_t, OSHMEM_OP_OR, SHMEM_UINT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(or, _size, size_t, OSHMEM_OP_OR, SHMEM_SIZE_T)

SHMEM_TYPE_TEAM_REDUCE_OP(xor, _uchar, unsigned char, OSHMEM_OP_XOR, SHMEM_UCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _ushort, unsigned short, OSHMEM_OP_XOR, SHMEM_USHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _uint, unsigned int, OSHMEM_OP_XOR, SHMEM_UINT)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _ulong, unsigned long, OSHMEM_OP_XOR, SHMEM_ULONG)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _ulonglong, unsigned long long, OSHMEM_OP_XOR, SHMEM_ULLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _int, int, OSHMEM_OP_XOR, SHMEM_INT)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _longlong, long long, OSHMEM_OP_XOR, SHMEM_LLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _int8, int8_t, OSHMEM_OP_XOR, SHMEM_INT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _int16, int16_t, OSHMEM_OP_XOR, SHMEM_INT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _int32, int32_t, OSHMEM_OP_XOR, SHMEM_INT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _int64, int64_t, OSHMEM_OP_XOR, SHMEM_INT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _uint8, uint8_t, OSHMEM_OP_XOR, SHMEM_UINT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _uint16, uint16_t, OSHMEM_OP_XOR, SHMEM_UINT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _uint32, uint32_t, OSHMEM_OP_XOR, SHMEM_UINT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _uint64, uint64_t, OSHMEM_OP_XOR, SHMEM_UINT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(xor, _size, size_t, OSHMEM_OP_XOR, SHMEM_SIZE_T)



SHMEM_TYPE_TEAM_REDUCE_OP(max, _uchar, unsigned char, OSHMEM_OP_MAX, SHMEM_UCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _ushort, unsigned short, OSHMEM_OP_MAX, SHMEM_USHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _uint, unsigned int, OSHMEM_OP_MAX, SHMEM_UINT)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _ulong, unsigned long, OSHMEM_OP_MAX, SHMEM_ULONG)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _ulonglong, unsigned long long, OSHMEM_OP_MAX, SHMEM_ULLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _int8, int8_t, OSHMEM_OP_MAX, SHMEM_INT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _int16, int16_t, OSHMEM_OP_MAX, SHMEM_INT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _int32, int32_t, OSHMEM_OP_MAX, SHMEM_INT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _int64, int64_t, OSHMEM_OP_MAX, SHMEM_INT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _uint8, uint8_t, OSHMEM_OP_MAX, SHMEM_UINT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _uint16, uint16_t, OSHMEM_OP_MAX, SHMEM_UINT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _uint32, uint32_t, OSHMEM_OP_MAX, SHMEM_UINT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _uint64, uint64_t, OSHMEM_OP_MAX, SHMEM_UINT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _size, size_t, OSHMEM_OP_MAX, SHMEM_SIZE_T)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _char, char, OSHMEM_OP_MAX, SHMEM_CHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _short, short, OSHMEM_OP_MAX, SHMEM_SHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _int, int, OSHMEM_OP_MAX, SHMEM_INT)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _long, long, OSHMEM_OP_MAX, SHMEM_LONG)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _float, float, OSHMEM_OP_MAX, SHMEM_FLOAT)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _double, double, OSHMEM_OP_MAX, SHMEM_DOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _longlong, long long, OSHMEM_OP_MAX, SHMEM_LLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _schar, signed char, OSHMEM_OP_MAX, SHMEM_SCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _longdouble, long double, OSHMEM_OP_MAX, SHMEM_LDOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(max, _ptrdiff, ptrdiff_t, OSHMEM_OP_MAX, SHMEM_PTRDIFF_T)


SHMEM_TYPE_TEAM_REDUCE_OP(min, _uchar, unsigned char, OSHMEM_OP_MIN, SHMEM_UCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _ushort, unsigned short, OSHMEM_OP_MIN, SHMEM_USHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _uint, unsigned int, OSHMEM_OP_MIN, SHMEM_UINT)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _ulong, unsigned long, OSHMEM_OP_MIN, SHMEM_ULONG)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _ulonglong, unsigned long long, OSHMEM_OP_MIN, SHMEM_ULLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _int8, int8_t, OSHMEM_OP_MIN, SHMEM_INT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _int16, int16_t, OSHMEM_OP_MIN, SHMEM_INT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _int32, int32_t, OSHMEM_OP_MIN, SHMEM_INT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _int64, int64_t, OSHMEM_OP_MIN, SHMEM_INT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _uint8, uint8_t, OSHMEM_OP_MIN, SHMEM_UINT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _uint16, uint16_t, OSHMEM_OP_MIN, SHMEM_UINT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _uint32, uint32_t, OSHMEM_OP_MIN, SHMEM_UINT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _uint64, uint64_t, OSHMEM_OP_MIN, SHMEM_UINT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _size, size_t, OSHMEM_OP_MIN, SHMEM_SIZE_T)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _char, char, OSHMEM_OP_MIN, SHMEM_CHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _short, short, OSHMEM_OP_MIN, SHMEM_SHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _int, int, OSHMEM_OP_MIN, SHMEM_INT)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _long, long, OSHMEM_OP_MIN, SHMEM_LONG)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _float, float, OSHMEM_OP_MIN, SHMEM_FLOAT)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _double, double, OSHMEM_OP_MIN, SHMEM_DOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _longlong, long long, OSHMEM_OP_MIN, SHMEM_LLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _schar, signed char, OSHMEM_OP_MIN, SHMEM_SCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _longdouble, long double, OSHMEM_OP_MIN, SHMEM_LDOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(min, _ptrdiff, ptrdiff_t, OSHMEM_OP_MIN, SHMEM_PTRDIFF_T)


SHMEM_TYPE_TEAM_REDUCE_OP(sum, _uchar, unsigned char, OSHMEM_OP_SUM, SHMEM_UCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _ushort, unsigned short, OSHMEM_OP_SUM, SHMEM_USHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _uint, unsigned int, OSHMEM_OP_SUM, SHMEM_UINT)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _ulong, unsigned long, OSHMEM_OP_SUM, SHMEM_ULONG)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _ulonglong, unsigned long long, OSHMEM_OP_SUM, SHMEM_ULLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _int8, int8_t, OSHMEM_OP_SUM, SHMEM_INT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _int16, int16_t, OSHMEM_OP_SUM, SHMEM_INT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _int32, int32_t, OSHMEM_OP_SUM, SHMEM_INT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _int64, int64_t, OSHMEM_OP_SUM, SHMEM_INT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _uint8, uint8_t, OSHMEM_OP_SUM, SHMEM_UINT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _uint16, uint16_t, OSHMEM_OP_SUM, SHMEM_UINT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _uint32, uint32_t, OSHMEM_OP_SUM, SHMEM_UINT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _uint64, uint64_t, OSHMEM_OP_SUM, SHMEM_UINT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _size, size_t, OSHMEM_OP_SUM, SHMEM_SIZE_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _char, char, OSHMEM_OP_SUM, SHMEM_CHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _short, short, OSHMEM_OP_SUM, SHMEM_SHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _int, int, OSHMEM_OP_SUM, SHMEM_INT)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _long, long, OSHMEM_OP_SUM, SHMEM_LONG)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _float, float, OSHMEM_OP_SUM, SHMEM_FLOAT)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _double, double, OSHMEM_OP_SUM, SHMEM_DOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _longlong, long long, OSHMEM_OP_SUM, SHMEM_LLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _schar, signed char, OSHMEM_OP_SUM, SHMEM_SCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _longdouble, long double, OSHMEM_OP_SUM, SHMEM_LDOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _ptrdiff, ptrdiff_t, OSHMEM_OP_SUM, SHMEM_PTRDIFF_T)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _complexd, double complex, OSHMEM_OP_SUM, SHMEM_COMPLEXD)
SHMEM_TYPE_TEAM_REDUCE_OP(sum, _complexf, float complex, OSHMEM_OP_SUM, SHMEM_COMPLEXF)

SHMEM_TYPE_TEAM_REDUCE_OP(prod, _uchar, unsigned char, OSHMEM_OP_PROD, SHMEM_UCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _ushort, unsigned short, OSHMEM_OP_PROD, SHMEM_USHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _uint, unsigned int, OSHMEM_OP_PROD, SHMEM_UINT)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _ulong, unsigned long, OSHMEM_OP_PROD, SHMEM_ULONG)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _ulonglong, unsigned long long, OSHMEM_OP_PROD, SHMEM_ULLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _int8, int8_t, OSHMEM_OP_PROD, SHMEM_INT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _int16, int16_t, OSHMEM_OP_PROD, SHMEM_INT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _int32, int32_t, OSHMEM_OP_PROD, SHMEM_INT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _int64, int64_t, OSHMEM_OP_PROD, SHMEM_INT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _uint8, uint8_t, OSHMEM_OP_PROD, SHMEM_UINT8_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _uint16, uint16_t, OSHMEM_OP_PROD, SHMEM_UINT16_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _uint32, uint32_t, OSHMEM_OP_PROD, SHMEM_UINT32_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _uint64, uint64_t, OSHMEM_OP_PROD, SHMEM_UINT64_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _size, size_t, OSHMEM_OP_PROD, SHMEM_SIZE_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _char, char, OSHMEM_OP_PROD, SHMEM_CHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _short, short, OSHMEM_OP_PROD, SHMEM_SHORT)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _int, int, OSHMEM_OP_PROD, SHMEM_INT)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _long, long, OSHMEM_OP_PROD, SHMEM_LONG)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _float, float, OSHMEM_OP_PROD, SHMEM_FLOAT)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _double, double, OSHMEM_OP_PROD, SHMEM_DOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _longlong, long long, OSHMEM_OP_PROD, SHMEM_LLONG)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _schar, signed char, OSHMEM_OP_PROD, SHMEM_SCHAR)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _longdouble, long double, OSHMEM_OP_PROD, SHMEM_LDOUBLE)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _ptrdiff, ptrdiff_t, OSHMEM_OP_PROD, SHMEM_PTRDIFF_T)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _complexd, double complex, OSHMEM_OP_PROD, SHMEM_COMPLEXD)
SHMEM_TYPE_TEAM_REDUCE_OP(prod, _complexf, float complex, OSHMEM_OP_PROD, SHMEM_COMPLEXF)

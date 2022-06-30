/*
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef SCOLL_UCC_DTYPES_H
#define SCOLL_UCC_DTYPES_H

#include "oshmem/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"

#include <ucc/api/ucc.h>

#define SCOLL_UCC_DT_UNSUPPORTED -1
#define SCOLL_UCC_OP_UNSUPPORTED -1

static ucc_datatype_t shmem_datatype_to_ucc_dt[OSHMEM_OP_TYPE_NUMBER + 1] = {
#if SIZEOF_SHORT == 2
    [OSHMEM_OP_TYPE_SHORT] = UCC_DT_INT16,
#else
    [OSHMEM_OP_TYPE_SHORT] = SCOLL_UCC_DT_UNSUPPORTED,
#endif
#if SIZEOF_INT == 4
    [OSHMEM_OP_TYPE_INT] = UCC_DT_INT32,
#else
    [OSHMEM_OP_TYPE_INT] = SCOLL_UCC_DT_UNSUPPORTED,
#endif
#if SIZEOF_LONG == 8
    [OSHMEM_OP_TYPE_LONG] = UCC_DT_INT64,
#else
    [OSHMEM_OP_TYPE_LONG] = SCOLL_UCC_DT_UNSUPPORTED,
#endif
#if SIZEOF_LONG_LONG == 8
    [OSHMEM_OP_TYPE_LLONG] = UCC_DT_INT64,
#else
    [OSHMEM_OP_TYPE_LLONG] = SCOLL_UCC_DT_UNSUPPORTED,
#endif
    [OSHMEM_OP_TYPE_INT16_T] = UCC_DT_INT16,
    [OSHMEM_OP_TYPE_INT32_T] = UCC_DT_INT32,
    [OSHMEM_OP_TYPE_INT64_T] = UCC_DT_INT64,
#if SIZEOF_FLOAT == 4
    [OSHMEM_OP_TYPE_FLOAT] = UCC_DT_FLOAT32,
#else
    [OSHMEM_OP_TYPE_FLOAT] = SCOLL_UCC_DT_UNSUPPORTED,
#endif
#if SIZEOF_DOUBLE == 8
    [OSHMEM_OP_TYPE_DOUBLE] = UCC_DT_FLOAT64,
#else
    [OSHMEM_OP_TYPE_DOUBLE] = SCOLL_UCC_DT_UNSUPPORTED,
#endif
#if UCC_HAVE_COMPLEX_AND_FLOAT128_DT
    [OSHMEM_OP_TYPE_FREAL16] = UCC_DT_FLOAT128,
    #if SIZEOF_LONG_DOUBLE == 16
        [OSHMEM_OP_TYPE_LDOUBLE] = UCC_DT_FLOAT128,
    #else
        [OSHMEM_OP_TYPE_LDOUBLE] = SCOLL_UCC_DT_UNSUPPORTED,
    #endif
    #if SIZEOF_FLOAT__COMPLEX == 8
        [OSHMEM_OP_TYPE_FCOMPLEX] = UCC_DT_FLOAT32_COMPLEX,
    #else
        [OSHMEM_OP_TYPE_FCOMPLEX] = SCOLL_UCC_DT_UNSUPPORTED,
    #endif
    #if SIZEOF_DOUBLE__COMPLEX == 16
        [OSHMEM_OP_TYPE_DCOMPLEX] = UCC_DT_FLOAT64_COMPLEX,
    #else
        [OSHMEM_OP_TYPE_DCOMPLEX] = SCOLL_UCC_DT_UNSUPPORTED,
    #endif
#else
    [OSHMEM_OP_TYPE_FREAL16] = SCOLL_UCC_DT_UNSUPPORTED,
    [OSHMEM_OP_TYPE_LDOUBLE] = SCOLL_UCC_DT_UNSUPPORTED,
    [OSHMEM_OP_TYPE_FCOMPLEX] = SCOLL_UCC_DT_UNSUPPORTED,
    [OSHMEM_OP_TYPE_DCOMPLEX] = SCOLL_UCC_DT_UNSUPPORTED,
#endif
    [OSHMEM_OP_TYPE_FINT2] = UCC_DT_INT16,
    [OSHMEM_OP_TYPE_FINT4] = UCC_DT_INT32,
    [OSHMEM_OP_TYPE_FINT8] = UCC_DT_INT64,
    [OSHMEM_OP_TYPE_FREAL4] = UCC_DT_FLOAT32,
    [OSHMEM_OP_TYPE_FREAL8] = UCC_DT_FLOAT64,
    [OSHMEM_OP_TYPE_NUMBER] = SCOLL_UCC_DT_UNSUPPORTED
};

static inline ucc_datatype_t shmem_op_to_ucc_dtype(oshmem_op_t *op)
{
    ucc_datatype_t dtype;

    dtype = shmem_datatype_to_ucc_dt[op->dt];
    if (op->dt == OSHMEM_OP_TYPE_NUMBER) {
        switch (op->dt_size) {
            case 8:
                return UCC_DT_INT64;
            case 4:
                return UCC_DT_INT32;
            case 2:
                return UCC_DT_INT16;
            case 1:
                return UCC_DT_INT8;
            default:
                return SCOLL_UCC_DT_UNSUPPORTED;
        }
    }

    return dtype;
}

static ucc_reduction_op_t oshmem_op_to_ucc_op_map[OSHMEM_OP_NUMBER + 1] = {
   UCC_OP_BAND,                  /* OSHMEM_OP_AND */
   UCC_OP_BOR,                   /* OSHMEM_OP_OR */
   UCC_OP_BXOR,                  /* OSHMEM_OP_XOR */
   UCC_OP_MAX,                   /* OSHMEM_OP_MAX */
   UCC_OP_MIN,                   /* OSHMEM_OP_MIN */
   UCC_OP_SUM,                   /* OSHMEM_OP_SUM */
   UCC_OP_PROD,                  /* OSHMEM_OP_PROD */
   SCOLL_UCC_OP_UNSUPPORTED,     /* OSHMEM_OP_NUMBER */
};

static inline ucc_reduction_op_t shmem_op_to_ucc_op(int op)
{
    return oshmem_op_to_ucc_op_map[op];
}

#endif /* SCOLL_UCC_DTYPES_H */

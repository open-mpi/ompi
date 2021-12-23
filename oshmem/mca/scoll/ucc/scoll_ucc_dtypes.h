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
    UCC_DT_INT16,                 /* OSHMEM_OP_TYPE_SHORT             0 */
    UCC_DT_INT32,                 /* OSHMEM_OP_TYPE_INT               1 */
    UCC_DT_INT64,                 /* OSHMEM_OP_TYPE_LONG              2 */
    UCC_DT_INT64,                 /* OSHMEM_OP_TYPE_LLONG             3 */
    UCC_DT_INT16,                 /* OSHMEM_OP_TYPE_INT16_T           4 */
    UCC_DT_INT32,                 /* OSHMEM_OP_TYPE_INT32_T           5 */
    UCC_DT_INT64,                 /* OSHMEM_OP_TYPE_INT64_T           6 */
    UCC_DT_FLOAT32,               /* OSHMEM_OP_TYPE_FLOAT             7 */
    UCC_DT_FLOAT64,               /* OSHMEM_OP_TYPE_DOUBLE            8 */
    SCOLL_UCC_DT_UNSUPPORTED,     /* OSHMEM_OP_TYPE_LDOUBLE           9 */
    SCOLL_UCC_DT_UNSUPPORTED,     /* OSHMEM_OP_TYPE_FCOMPLEX         10 */
    SCOLL_UCC_DT_UNSUPPORTED,     /* OSHMEM_OP_TYPE_DCOMPLEX         11 */
    UCC_DT_INT16,                 /* OSHMEM_OP_TYPE_FINT2            12 */
    UCC_DT_INT32,                 /* OSHMEM_OP_TYPE_FINT4            13 */
    UCC_DT_INT64,                 /* OSHMEM_OP_TYPE_FINT8            14 */
    UCC_DT_FLOAT32,               /* OSHMEM_OP_TYPE_FREAL4           15 */
    UCC_DT_FLOAT64,               /* OSHMEM_OP_TYPE_FREAL8           16 */
    SCOLL_UCC_DT_UNSUPPORTED,     /* OSHMEM_OP_TYPE_FREAL16          17 */
    SCOLL_UCC_DT_UNSUPPORTED      /* OSHMEM_OP_TYPE_NUMBER           18 */
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

/*
 * Copyright (c) 2020 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef COLL_MCCL_DTYPES_H
#define COLL_MCCL_DTYPES_H
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/mca/op/op.h"
#include "api/mccl.h"


static tccl_dt_t ompi_datatype_2_tccl_dt[OMPI_DATATYPE_MAX_PREDEFINED] = {
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_LOOP           0 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_END_LOOP       1 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_LB             2 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_UB             3 */
    TCCL_DT_INT8,                  /*OPAL_DATATYPE_INT1           4 */
    TCCL_DT_INT16,                 /*OPAL_DATATYPE_INT2           5 */
    TCCL_DT_INT32,                 /*OPAL_DATATYPE_INT4           6 */
    TCCL_DT_INT64,                 /*OPAL_DATATYPE_INT8           7 */
    TCCL_DT_INT128,                /*OPAL_DATATYPE_INT16          8 */
    TCCL_DT_UINT8,                 /*OPAL_DATATYPE_UINT1          9 */
    TCCL_DT_UINT16,                /*OPAL_DATATYPE_UINT2          10 */
    TCCL_DT_UINT32,                /*OPAL_DATATYPE_UINT4          11 */
    TCCL_DT_UINT64,                /*OPAL_DATATYPE_UINT8          12 */
    TCCL_DT_UINT128,               /*OPAL_DATATYPE_UINT16         13 */
    TCCL_DT_FLOAT16,               /*OPAL_DATATYPE_FLOAT2         14 */
    TCCL_DT_FLOAT32,               /*OPAL_DATATYPE_FLOAT4         15 */
    TCCL_DT_FLOAT64,               /*OPAL_DATATYPE_FLOAT8         16 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_FLOAT12        17 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_FLOAT16        18 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_SHORT_FLOAT_COMPLEX 19 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_FLOAT_COMPLEX  20 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_DOUBLE_COMPLEX 21 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_LONG_DOUBLE_COMPLEX 22 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_BOOL           23 */
    TCCL_DT_UNSUPPORTED,           /*OPAL_DATATYPE_WCHAR          24 */
    TCCL_DT_UNSUPPORTED            /*OPAL_DATATYPE_UNAVAILABLE    25 */
};

static inline tccl_dt_t ompi_dtype_to_tccl_dtype(ompi_datatype_t *dtype)
{
    int ompi_type_id = dtype->id;
    int opal_type_id = dtype->super.id;

    if (ompi_type_id < OMPI_DATATYPE_MPI_MAX_PREDEFINED &&
        dtype->super.flags & OMPI_DATATYPE_FLAG_PREDEFINED) {
        if (opal_type_id > 0 && opal_type_id < OPAL_DATATYPE_MAX_PREDEFINED) {
            return  ompi_datatype_2_tccl_dt[opal_type_id];
        }
    }
    return TCCL_DT_UNSUPPORTED;
}

static tccl_op_t ompi_op_to_tccl_op_map[OMPI_OP_BASE_FORTRAN_OP_MAX + 1] = {
   TCCL_OP_UNSUPPORTED,          /* OMPI_OP_BASE_FORTRAN_NULL = 0 */
   TCCL_OP_MAX,                  /* OMPI_OP_BASE_FORTRAN_MAX */
   TCCL_OP_MIN,                  /* OMPI_OP_BASE_FORTRAN_MIN */
   TCCL_OP_SUM,                  /* OMPI_OP_BASE_FORTRAN_SUM */
   TCCL_OP_PROD,                 /* OMPI_OP_BASE_FORTRAN_PROD */
   TCCL_OP_LAND,                 /* OMPI_OP_BASE_FORTRAN_LAND */
   TCCL_OP_BAND,                 /* OMPI_OP_BASE_FORTRAN_BAND */
   TCCL_OP_LOR,                  /* OMPI_OP_BASE_FORTRAN_LOR */
   TCCL_OP_BOR,                  /* OMPI_OP_BASE_FORTRAN_BOR */
   TCCL_OP_LXOR,                 /* OMPI_OP_BASE_FORTRAN_LXOR */
   TCCL_OP_BXOR,                 /* OMPI_OP_BASE_FORTRAN_BXOR */
   TCCL_OP_UNSUPPORTED,          /* OMPI_OP_BASE_FORTRAN_MAXLOC */
   TCCL_OP_UNSUPPORTED,          /* OMPI_OP_BASE_FORTRAN_MINLOC */
   TCCL_OP_UNSUPPORTED,          /* OMPI_OP_BASE_FORTRAN_REPLACE */
   TCCL_OP_UNSUPPORTED,          /* OMPI_OP_BASE_FORTRAN_NO_OP */
   TCCL_OP_UNSUPPORTED           /* OMPI_OP_BASE_FORTRAN_OP_MAX */
};

static inline tccl_op_t ompi_op_to_tccl_op(ompi_op_t *op) {
    if (op->o_f_to_c_index > OMPI_OP_BASE_FORTRAN_OP_MAX) {
        return TCCL_OP_UNSUPPORTED;
    }
    return ompi_op_to_tccl_op_map[op->o_f_to_c_index];
}

#endif /* COLL_MCCL_DTYPES_H */

/*
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef COLL_UCC_DTYPES_H
#define COLL_UCC_DTYPES_H
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/mca/op/op.h"
#include <ucc/api/ucc.h>

#define COLL_UCC_DT_UNSUPPORTED ((ucc_datatype_t)-1)
#define COLL_UCC_OP_UNSUPPORTED ((ucc_reduction_op_t)-1)

static ucc_datatype_t ompi_datatype_2_ucc_dt[OPAL_DATATYPE_MAX_PREDEFINED] = {
    [OPAL_DATATYPE_LOOP]                = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_END_LOOP]            = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_LB]                  = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_UB]                  = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_INT1]                = UCC_DT_INT8,
    [OPAL_DATATYPE_INT2]                = UCC_DT_INT16,
    [OPAL_DATATYPE_INT4]                = UCC_DT_INT32,
    [OPAL_DATATYPE_INT8]                = UCC_DT_INT64,
    [OPAL_DATATYPE_INT16]               = UCC_DT_INT128,
    [OPAL_DATATYPE_UINT1]               = UCC_DT_UINT8,
    [OPAL_DATATYPE_UINT2]               = UCC_DT_UINT16,
    [OPAL_DATATYPE_UINT4]               = UCC_DT_UINT32,
    [OPAL_DATATYPE_UINT8]               = UCC_DT_UINT64,
    [OPAL_DATATYPE_UINT16]              = UCC_DT_UINT128,
    [OPAL_DATATYPE_FLOAT2]              = UCC_DT_FLOAT16,
    [OPAL_DATATYPE_FLOAT4]              = UCC_DT_FLOAT32,
    [OPAL_DATATYPE_FLOAT8]              = UCC_DT_FLOAT64,
    [OPAL_DATATYPE_FLOAT12]             = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_FLOAT16]             = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_SHORT_FLOAT_COMPLEX] = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_FLOAT_COMPLEX]       = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_DOUBLE_COMPLEX]      = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_LONG_DOUBLE_COMPLEX] = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_BOOL]                = COLL_UCC_DT_UNSUPPORTED,
    [OPAL_DATATYPE_WCHAR]               = COLL_UCC_DT_UNSUPPORTED,
#if SIZEOF_LONG == 4
    [OPAL_DATATYPE_LONG]                = UCC_DT_INT32,
    [OPAL_DATATYPE_UNSIGNED_LONG]       = UCC_DT_UINT32,
#elif SIZEOF_LONG == 8
    [OPAL_DATATYPE_LONG]                = UCC_DT_INT64,
    [OPAL_DATATYPE_UNSIGNED_LONG]       = UCC_DT_UINT64,
#endif
    [OPAL_DATATYPE_UNAVAILABLE]         = COLL_UCC_DT_UNSUPPORTED
};

static inline ucc_datatype_t ompi_dtype_to_ucc_dtype(ompi_datatype_t *dtype)
{
    int ompi_type_id = dtype->id;
    int opal_type_id = dtype->super.id;

    if (ompi_type_id < OMPI_DATATYPE_MPI_MAX_PREDEFINED &&
        dtype->super.flags & OMPI_DATATYPE_FLAG_PREDEFINED) {
        if (opal_type_id > 0 && opal_type_id < OPAL_DATATYPE_MAX_PREDEFINED) {
            return  ompi_datatype_2_ucc_dt[opal_type_id];
        }
    }
    return COLL_UCC_DT_UNSUPPORTED;
}

static ucc_reduction_op_t ompi_op_to_ucc_op_map[OMPI_OP_BASE_FORTRAN_OP_MAX + 1] = {
   COLL_UCC_OP_UNSUPPORTED,     /* OMPI_OP_BASE_FORTRAN_NULL = 0 */
   UCC_OP_MAX,                  /* OMPI_OP_BASE_FORTRAN_MAX */
   UCC_OP_MIN,                  /* OMPI_OP_BASE_FORTRAN_MIN */
   UCC_OP_SUM,                  /* OMPI_OP_BASE_FORTRAN_SUM */
   UCC_OP_PROD,                 /* OMPI_OP_BASE_FORTRAN_PROD */
   UCC_OP_LAND,                 /* OMPI_OP_BASE_FORTRAN_LAND */
   UCC_OP_BAND,                 /* OMPI_OP_BASE_FORTRAN_BAND */
   UCC_OP_LOR,                  /* OMPI_OP_BASE_FORTRAN_LOR */
   UCC_OP_BOR,                  /* OMPI_OP_BASE_FORTRAN_BOR */
   UCC_OP_LXOR,                 /* OMPI_OP_BASE_FORTRAN_LXOR */
   UCC_OP_BXOR,                 /* OMPI_OP_BASE_FORTRAN_BXOR */
   COLL_UCC_OP_UNSUPPORTED,     /* OMPI_OP_BASE_FORTRAN_MAXLOC */
   COLL_UCC_OP_UNSUPPORTED,     /* OMPI_OP_BASE_FORTRAN_MINLOC */
   COLL_UCC_OP_UNSUPPORTED,     /* OMPI_OP_BASE_FORTRAN_REPLACE */
   COLL_UCC_OP_UNSUPPORTED,     /* OMPI_OP_BASE_FORTRAN_NO_OP */
   COLL_UCC_OP_UNSUPPORTED      /* OMPI_OP_BASE_FORTRAN_OP_MAX */
};

static inline ucc_reduction_op_t ompi_op_to_ucc_op(ompi_op_t *op) {
    if (op->o_f_to_c_index > OMPI_OP_BASE_FORTRAN_OP_MAX) {
        return COLL_UCC_OP_UNSUPPORTED;
    }
    return ompi_op_to_ucc_op_map[op->o_f_to_c_index];
}

#endif /* COLL_UCC_DTYPES_H */

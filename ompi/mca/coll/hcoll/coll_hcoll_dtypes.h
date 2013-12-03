#ifndef COLL_HCOLL_DTYPES_H
#define COLL_HCOLL_DTYPES_H

/*Here comes the translation between ompi_datatype_t and dte_data_representation
  This is not complete and takes into account the basic datatypes only
  It is used to extract allreduce bcol functions where the arrhythmetics has to be done*/

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/op/op.h"
#include "hcoll_dte.h"

/*to keep this at hand: Ids of the basic opal_datatypes:
#define OPAL_DATATYPE_INT1           4
#define OPAL_DATATYPE_INT2           5
#define OPAL_DATATYPE_INT4           6
#define OPAL_DATATYPE_INT8           7
#define OPAL_DATATYPE_INT16          8
#define OPAL_DATATYPE_UINT1          9
#define OPAL_DATATYPE_UINT2          10
#define OPAL_DATATYPE_UINT4          11
#define OPAL_DATATYPE_UINT8          12
#define OPAL_DATATYPE_UINT16         13
#define OPAL_DATATYPE_FLOAT2         14
#define OPAL_DATATYPE_FLOAT4         15
#define OPAL_DATATYPE_FLOAT8         16
#define OPAL_DATATYPE_FLOAT12        17
#define OPAL_DATATYPE_FLOAT16        18

total 15 types
*/



static dte_data_representation_t* ompi_datatype_2_dte_data_rep[OPAL_DATATYPE_MAX_PREDEFINED] = {
    &DTE_ZERO,                  /*OPAL_DATATYPE_LOOP           0 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_END_LOOP       1 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_LB             2 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_UB             3 */
    &DTE_BYTE,                  /*OPAL_DATATYPE_INT1           4 */
    &DTE_INT16,                 /*OPAL_DATATYPE_INT2           5 */
    &DTE_INT32,                 /*OPAL_DATATYPE_INT4           6 */
    &DTE_INT64,                 /*OPAL_DATATYPE_INT8           7 */
    &DTE_INT128,                /*OPAL_DATATYPE_INT16          8 */
    &DTE_UBYTE,                 /*OPAL_DATATYPE_UINT1          9 */
    &DTE_UINT16,                /*OPAL_DATATYPE_UINT2          10 */
    &DTE_UINT32,                /*OPAL_DATATYPE_UINT4          11 */
    &DTE_UINT64,                /*OPAL_DATATYPE_UINT8          12 */
    &DTE_UINT128,               /*OPAL_DATATYPE_UINT16         13 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_FLOAT2         14 */
    &DTE_FLOAT32,               /*OPAL_DATATYPE_FLOAT4         15 */
    &DTE_FLOAT64,               /*OPAL_DATATYPE_FLOAT8         16 */
    &DTE_FLOAT96,               /*OPAL_DATATYPE_FLOAT12        17 */
    &DTE_FLOAT128,              /*OPAL_DATATYPE_FLOAT16        18 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_COMPLEX8       19 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_COMPLEX16      20 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_COMPLEX32      21 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_BOOL           22 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_WCHAR          23 */
    &DTE_ZERO                   /*OPAL_DATATYPE_UNAVAILABLE    24 */
};

static dte_data_representation_t ompi_dtype_2_dte_dtype(ompi_datatype_t *dtype){
    int ompi_type_id = dtype->id;
    int opal_type_id = dtype->super.id;
    dte_data_representation_t dte_data_rep;
    if (OPAL_UNLIKELY( ompi_type_id < 0 ||
                       ompi_type_id >= OPAL_DATATYPE_MAX_PREDEFINED)){
        dte_data_rep = DTE_ZERO;
        dte_data_rep.rep.in_line_rep.data_handle.in_line.in_line = 0;
        dte_data_rep.rep.in_line_rep.data_handle.pointer_to_handle = (uint64_t ) &dtype->super;
        return dte_data_rep;
    }
    return *ompi_datatype_2_dte_data_rep[opal_type_id];
}


/*
enum ompi_op_type {
    OMPI_OP_BASE_FORTRAN_NULL = 0,
    OMPI_OP_BASE_FORTRAN_MAX,
    OMPI_OP_BASE_FORTRAN_MIN,
    OMPI_OP_BASE_FORTRAN_SUM,
    OMPI_OP_BASE_FORTRAN_PROD,
    OMPI_OP_BASE_FORTRAN_LAND,
    OMPI_OP_BASE_FORTRAN_BAND,
    OMPI_OP_BASE_FORTRAN_LOR,
    OMPI_OP_BASE_FORTRAN_BOR,
    OMPI_OP_BASE_FORTRAN_LXOR,
    OMPI_OP_BASE_FORTRAN_BXOR,
    OMPI_OP_BASE_FORTRAN_MAXLOC,
    OMPI_OP_BASE_FORTRAN_MINLOC,
    OMPI_OP_BASE_FORTRAN_REPLACE,

    OMPI_OP_BASE_FORTRAN_OP_MAX
};*/
static hcoll_dte_op_t* ompi_op_2_hcoll_op[OMPI_OP_BASE_FORTRAN_OP_MAX] = {
   &hcoll_dte_op_null,
   &hcoll_dte_op_max,
   &hcoll_dte_op_min,
   &hcoll_dte_op_sum,
   &hcoll_dte_op_prod,
   &hcoll_dte_op_land,
   &hcoll_dte_op_band,
   &hcoll_dte_op_lor,
   &hcoll_dte_op_bor,
   &hcoll_dte_op_lxor,
   &hcoll_dte_op_bxor,
   &hcoll_dte_op_null,
   &hcoll_dte_op_null,
   &hcoll_dte_op_null
};
static hcoll_dte_op_t* ompi_op_2_hcolrte_op(ompi_op_t *op){
    return ompi_op_2_hcoll_op[op->o_f_to_c_index];
}

#endif /* COLL_HCOLL_DTYPES_H */

#ifndef COLL_HCOLL_DTYPES_H
#define COLL_HCOLL_DTYPES_H

/*Here comes the translation between ompi_datatype_t and dte_data_representation
  This is not complete and takes into account the basic datatypes only
  It is used to extract allreduce bcol functions where the arrhythmetics has to be done*/

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/mca/op/op.h"
#include "hcoll/api/hcoll_dte.h"
extern int hcoll_type_attr_keyval;

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
#define OPAL_DATATYPE_FLOAT_COMPLEX  19
#define OPAL_DATATYPE_DOUBLE_COMPLEX 20

total 15 types
*/

static dte_data_representation_t* ompi_datatype_2_dte_data_rep[OMPI_DATATYPE_MAX_PREDEFINED] = {
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
#if defined(DTE_FLOAT32_COMPLEX)
    &DTE_FLOAT32_COMPLEX,       /*OPAL_DATATYPE_COMPLEX8       19 */
#else
    &DTE_ZERO,
#endif
#if defined(DTE_FLOAT64_COMPLEX)
    &DTE_FLOAT64_COMPLEX,       /*OPAL_DATATYPE_COMPLEX32      20 */
#else
    &DTE_ZERO,
#endif
#if defined(DTE_FLOAT128_COMPLEX)
    &DTE_FLOAT128_COMPLEX,       /*OPAL_DATATYPE_COMPLEX64     21 */
#else
    &DTE_ZERO,
#endif
    &DTE_ZERO,                  /*OPAL_DATATYPE_BOOL           22 */
    &DTE_ZERO,                  /*OPAL_DATATYPE_WCHAR          23 */
    &DTE_ZERO                   /*OPAL_DATATYPE_UNAVAILABLE    24 */
};

enum {
    TRY_FIND_DERIVED,
    NO_DERIVED
};


#if HCOLL_API >= HCOLL_VERSION(3,6)
static inline
int hcoll_map_derived_type(ompi_datatype_t *dtype, dte_data_representation_t *new_dte)
{
    int rc;
    if (NULL == dtype->args) {
        /* predefined type, shouldn't call this */
        return OMPI_SUCCESS;
    }
    rc = hcoll_create_mpi_type((void*)dtype, new_dte);
    return rc == HCOLL_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR;
}

static dte_data_representation_t find_derived_mapping(ompi_datatype_t *dtype){
    dte_data_representation_t dte = DTE_ZERO;
    mca_coll_hcoll_dtype_t *hcoll_dtype;
    if (mca_coll_hcoll_component.derived_types_support_enabled) {
        int map_found = 0;
        ompi_attr_get_c(dtype->d_keyhash, hcoll_type_attr_keyval,
                        (void**)&hcoll_dtype, &map_found);
        if (!map_found)
            hcoll_map_derived_type(dtype, &dte);
        else
            dte = hcoll_dtype->type;
    }

    return dte;
}



static inline  dte_data_representation_t
ompi_predefined_derived_2_hcoll(int ompi_id) {
    switch(ompi_id) {
    case OMPI_DATATYPE_MPI_FLOAT_INT:
        return DTE_FLOAT_INT;
    case OMPI_DATATYPE_MPI_DOUBLE_INT:
        return DTE_DOUBLE_INT;
    case OMPI_DATATYPE_MPI_LONG_INT:
        return DTE_LONG_INT;
    case OMPI_DATATYPE_MPI_SHORT_INT:
        return DTE_SHORT_INT;
    case OMPI_DATATYPE_MPI_LONG_DOUBLE_INT:
        return DTE_LONG_DOUBLE_INT;
    case OMPI_DATATYPE_MPI_2INT:
        return DTE_2INT;
#if HCOLL_API >= HCOLL_VERSION(3,7)
    case OMPI_DATATYPE_MPI_2INTEGER:
#if OMPI_SIZEOF_FORTRAN_INTEGER == 4
        return DTE_2INT;
#elif OMPI_SIZEOF_FORTRAN_INTEGER == 8
        return DTE_2INT64;
#else
        return DTE_ZERO;
#endif
    case OMPI_DATATYPE_MPI_2REAL:
#if OMPI_SIZEOF_FORTRAN_REAL == 4
        return DTE_2FLOAT32;
#elif OMPI_SIZEOF_FORTRAN_REAL == 8
        return DTE_2FLOAT64;
#else
        return DTE_ZERO;
#endif
    case OMPI_DATATYPE_MPI_2DBLPREC:
#if OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION == 4
        return DTE_2FLOAT32;
#elif OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION == 8
        return DTE_2FLOAT64;
#else
        return DTE_ZERO;
#endif
#endif
    default:
        break;
    }
    return DTE_ZERO;
}
#endif

static dte_data_representation_t
ompi_dtype_2_hcoll_dtype( ompi_datatype_t *dtype,
                          const int mode)
{
    int ompi_type_id = dtype->id;
    int opal_type_id = dtype->super.id;
    dte_data_representation_t dte_data_rep = DTE_ZERO;

    if (ompi_type_id < OMPI_DATATYPE_MPI_MAX_PREDEFINED &&
        dtype->super.flags & OMPI_DATATYPE_FLAG_PREDEFINED) {
        if (opal_type_id > 0 && opal_type_id < OPAL_DATATYPE_MAX_PREDEFINED) {
            dte_data_rep =  *ompi_datatype_2_dte_data_rep[opal_type_id];
        }
#if HCOLL_API >= HCOLL_VERSION(3,6)
        else if (TRY_FIND_DERIVED == mode){
            dte_data_rep =  ompi_predefined_derived_2_hcoll(ompi_type_id);
        }
    } else {
        if (TRY_FIND_DERIVED == mode)
            dte_data_rep =  find_derived_mapping(dtype);
#endif
    }
    if (HCOL_DTE_IS_ZERO(dte_data_rep) && TRY_FIND_DERIVED == mode &&
        !mca_coll_hcoll_component.hcoll_datatype_fallback) {
        dte_data_rep = DTE_ZERO;
        dte_data_rep.rep.in_line_rep.data_handle.in_line.in_line = 0;
        dte_data_rep.rep.in_line_rep.data_handle.pointer_to_handle = (uint64_t ) &dtype->super;
    }
    return dte_data_rep;
}

static hcoll_dte_op_t* ompi_op_2_hcoll_op[OMPI_OP_BASE_FORTRAN_OP_MAX + 1] = {
   &hcoll_dte_op_null,          /* OMPI_OP_BASE_FORTRAN_NULL = 0 */
   &hcoll_dte_op_max,           /* OMPI_OP_BASE_FORTRAN_MAX */
   &hcoll_dte_op_min,           /* OMPI_OP_BASE_FORTRAN_MIN */
   &hcoll_dte_op_sum,           /* OMPI_OP_BASE_FORTRAN_SUM */
   &hcoll_dte_op_prod,          /* OMPI_OP_BASE_FORTRAN_PROD */
   &hcoll_dte_op_land,          /* OMPI_OP_BASE_FORTRAN_LAND */
   &hcoll_dte_op_band,          /* OMPI_OP_BASE_FORTRAN_BAND */
   &hcoll_dte_op_lor,           /* OMPI_OP_BASE_FORTRAN_LOR */
   &hcoll_dte_op_bor,           /* OMPI_OP_BASE_FORTRAN_BOR */
   &hcoll_dte_op_lxor,          /* OMPI_OP_BASE_FORTRAN_LXOR */
   &hcoll_dte_op_bxor,          /* OMPI_OP_BASE_FORTRAN_BXOR */
   &hcoll_dte_op_null,          /* OMPI_OP_BASE_FORTRAN_MAXLOC */
   &hcoll_dte_op_null,          /* OMPI_OP_BASE_FORTRAN_MINLOC */
   &hcoll_dte_op_null,          /* OMPI_OP_BASE_FORTRAN_REPLACE */
   &hcoll_dte_op_null,          /* OMPI_OP_BASE_FORTRAN_NO_OP */
   &hcoll_dte_op_null           /* OMPI_OP_BASE_FORTRAN_OP_MAX */
};
static hcoll_dte_op_t* ompi_op_2_hcolrte_op(ompi_op_t *op) {
    if (op->o_f_to_c_index > OMPI_OP_BASE_FORTRAN_OP_MAX) {
        return ompi_op_2_hcoll_op[0]; /* return null */
    }
    return ompi_op_2_hcoll_op[op->o_f_to_c_index];
}


#if HCOLL_API >= HCOLL_VERSION(3,6)
static int hcoll_type_attr_del_fn(MPI_Datatype type, int keyval, void *attr_val, void *extra) {
    int ret = OMPI_SUCCESS;
    mca_coll_hcoll_dtype_t *dtype =
        (mca_coll_hcoll_dtype_t*) attr_val;

    assert(dtype);
    if (HCOLL_SUCCESS != (ret = hcoll_dt_destroy(dtype->type))) {
        HCOL_ERROR("failed to delete type attr: hcoll_dte_destroy returned %d",ret);
        return OMPI_ERROR;
    }
    opal_free_list_return(&mca_coll_hcoll_component.dtypes,
                          &dtype->super);

    return OMPI_SUCCESS;
}
#else
static int hcoll_type_attr_del_fn(MPI_Datatype type, int keyval, void *attr_val, void *extra) {
    /*Do nothing - it's an old version of hcoll w/o dtypes support */
    return OMPI_SUCCESS;
}
#endif
#endif /* COLL_HCOLL_DTYPES_H */

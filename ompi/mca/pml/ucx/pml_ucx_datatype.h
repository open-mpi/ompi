/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_UCX_DATATYPE_H_
#define PML_UCX_DATATYPE_H_

#include "pml_ucx.h"


#define PML_UCX_DATATYPE_INVALID   0

#ifdef HAVE_UCP_REQUEST_PARAM_T
typedef struct {
    ucp_datatype_t          datatype;
    int                     size_shift;
    struct {
        ucp_request_param_t send;
        ucp_request_param_t isend;
        ucp_request_param_t recv;
        ucp_request_param_t irecv;
    } op_param;
} pml_ucx_datatype_t;
#endif

struct pml_ucx_convertor {
    opal_free_list_item_t   super;
    ompi_datatype_t         *datatype;
    opal_convertor_t        opal_conv;
    size_t                  offset;
};

ucp_datatype_t mca_pml_ucx_init_datatype(ompi_datatype_t *datatype);

int mca_pml_ucx_datatype_attr_del_fn(ompi_datatype_t* datatype, int keyval,
                                     void *attr_val, void *extra);

OBJ_CLASS_DECLARATION(mca_pml_ucx_convertor_t);


__opal_attribute_always_inline__
static inline ucp_datatype_t
mca_pml_ucx_from_ompi_datatype(ompi_datatype_t *datatype)
{
#ifdef HAVE_UCP_REQUEST_PARAM_T
    return ((pml_ucx_datatype_t*)datatype->pml_data)->datatype;
#else
    return (ucp_datatype_t)datatype->pml_data;
#endif
}


__opal_attribute_always_inline__
static inline ucp_datatype_t mca_pml_ucx_get_datatype(ompi_datatype_t *datatype)
{
    if (OPAL_UNLIKELY(datatype->pml_data == PML_UCX_DATATYPE_INVALID)) {
        return mca_pml_ucx_init_datatype(datatype);
    }

    return mca_pml_ucx_from_ompi_datatype(datatype);
}

#ifdef HAVE_UCP_REQUEST_PARAM_T
__opal_attribute_always_inline__
static inline pml_ucx_datatype_t*
mca_pml_ucx_get_op_data(ompi_datatype_t *datatype)
{
    pml_ucx_datatype_t *ucp_type = (pml_ucx_datatype_t*)datatype->pml_data;

    if (OPAL_LIKELY(ucp_type != PML_UCX_DATATYPE_INVALID)) {
        return ucp_type;
    }

    mca_pml_ucx_init_datatype(datatype);
    return (pml_ucx_datatype_t*)datatype->pml_data;
}

__opal_attribute_always_inline__
static inline size_t mca_pml_ucx_get_data_size(pml_ucx_datatype_t *op_data,
                                               size_t count)
{
    return count << op_data->size_shift;
}
#endif

#endif /* PML_UCX_DATATYPE_H_ */

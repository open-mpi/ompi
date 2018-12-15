/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2020 Huawei Technologies Co., Ltd.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef COMMON_UCX_DATATYPE_H_
#define COMMON_UCX_DATATYPE_H_

#include "common_ucx_freelist.h"

#include "ompi/request/request.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"

#include "opal/mca/common/ucx/common_ucx.h"

#define COMMON_UCX_DATATYPE_INVALID   0

#ifdef HAVE_UCP_REQUEST_PARAM_T
typedef struct {
    ucp_datatype_t          datatype;
    int                     size_shift;
    struct {
        ucp_request_param_t send;
        ucp_request_param_t bsend;
        ucp_request_param_t recv;
    } op_param;
} mca_common_ucx_datatype_t;
#endif

typedef struct mca_common_ucx_datatype_ctx {
    opal_object_t             super;
    int                       datatype_attr_keyval;
    ucp_datatype_t            predefined_types[OMPI_DATATYPE_MPI_MAX_PREDEFINED];
    mca_common_ucx_freelist_t convs; /* Converters pool */
} mca_common_ucx_datatype_ctx_t;

typedef struct mca_common_ucx_convertor {
    opal_free_list_item_t          super;
    mca_common_ucx_datatype_ctx_t *ctx;
    ompi_datatype_t               *datatype;
    opal_convertor_t               opal_conv;
    size_t                         offset;
} mca_common_ucx_convertor_t;

typedef struct ompi_common_ucx_module {
    ompi_request_t                completed_request;
    mca_common_ucx_freelist_t     requests;
    mca_common_ucx_datatype_ctx_t datatype_ctx;
    mca_common_ucx_datatype_t     datatype_init;
    int                           is_initialized;
} ompi_common_ucx_module_t;

extern ompi_common_ucx_module_t ompi_common_ucx;

ucp_datatype_t mca_common_ucx_init_datatype(ompi_datatype_t *datatype);

int mca_common_ucx_datatype_attr_del_fn(ompi_datatype_t* datatype, int keyval,
                                        void *attr_val, void *extra);

OBJ_CLASS_DECLARATION(mca_common_ucx_convertor_t);
OBJ_CLASS_DECLARATION(mca_common_ucx_datatype_ctx_t);


__opal_attribute_always_inline__
static inline ucp_datatype_t mca_common_ucx_get_datatype(ompi_datatype_t *datatype)
{
#ifdef HAVE_UCP_REQUEST_PARAM_T
    mca_common_ucx_datatype_t *ucp_type = (mca_common_ucx_datatype_t*)datatype->pml_data;

    if (OPAL_LIKELY(ucp_type != COMMON_UCX_DATATYPE_INVALID)) {
        return ucp_type->datatype;
    }
#else
    ucp_datatype_t ucp_type = datatype->pml_data;

    if (OPAL_LIKELY(ucp_type != COMMON_UCX_DATATYPE_INVALID)) {
        return ucp_type;
    }
#endif

    return mca_common_ucx_init_datatype(datatype);
}

#ifdef HAVE_UCP_REQUEST_PARAM_T
__opal_attribute_always_inline__
static inline mca_common_ucx_datatype_t*
mca_common_ucx_get_op_data(ompi_datatype_t *datatype)
{
    mca_common_ucx_datatype_t *ucp_type = (mca_common_ucx_datatype_t*)datatype->pml_data;

    if (OPAL_LIKELY(ucp_type != COMMON_UCX_DATATYPE_INVALID)) {
        return ucp_type;
    }

    mca_common_ucx_init_datatype(datatype);
    return (mca_common_ucx_datatype_t*)datatype->pml_data;
}

__opal_attribute_always_inline__
static inline size_t mca_common_ucx_get_data_size(mca_common_ucx_datatype_t *op_data,
                                                  size_t count)
{
    return count << op_data->size_shift;
}
#endif

#endif /* COMMON_UCX_DATATYPE_H_ */

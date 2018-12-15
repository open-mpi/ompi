/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Huawei Technologies Co., Ltd.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/message/message.h"
#include "ompi/runtime/ompi_spc.h"
#include <inttypes.h>

__opal_attribute_always_inline__ static inline void
mca_pml_ucx_send_completion_internal(void *request, ucs_status_t status)
{
    ompi_request_t *req = request;

    PML_UCX_VERBOSE(8, "send request %p completed with status %s", (void*)req,
                    ucs_status_string(status));

    mca_common_ucx_set_status(&req->req_status, status);
    PML_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    ompi_request_complete(req, true);
}

__opal_attribute_always_inline__ static inline void
mca_pml_ucx_bsend_completion_internal(void *request, ucs_status_t status)
{
    ompi_request_t *req = request;

    PML_UCX_VERBOSE(8, "bsend request %p buffer %p completed with status %s", (void*)req,
                    req->req_complete_cb_data, ucs_status_string(status));

    PML_UCX_ASSERT((((mca_pml_ucx_persistent_request_t*)req)->flags &
                    MCA_PML_UCX_REQUEST_FLAG_SEND) != 0);
    PML_UCX_ASSERT(MCA_PML_BASE_SEND_BUFFERED ==
                   ((mca_pml_ucx_persistent_request_t*)req)->send.mode);
    OBJ_RELEASE(((mca_pml_ucx_persistent_request_t*)req)->datatype.ompi_datatype);

    mca_pml_base_bsend_request_free(req->req_complete_cb_data);
    req->req_complete_cb_data = NULL;
    mca_common_ucx_set_status(&req->req_status, status);
    PML_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    mca_common_ucx_request_free(&req);
}

__opal_attribute_always_inline__ static inline void
mca_pml_ucx_recv_completion_internal(void *request, ucs_status_t status,
                                     const ucp_tag_recv_info_t *info)
{
    ompi_request_t *req = request;

    PML_UCX_VERBOSE(8, "receive request %p completed with status %s tag %"PRIx64" len %zu",
                    (void*)req, ucs_status_string(status), info->sender_tag,
                    info->length);

    SPC_USER_OR_MPI(PML_UCX_TAG_GET_MPI_TAG(info->sender_tag), info->length,
                    OMPI_SPC_BYTES_RECEIVED_USER, OMPI_SPC_BYTES_RECEIVED_MPI);

    mca_pml_ucx_set_recv_status(&req->req_status, status, info);
    PML_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    ompi_request_complete(req, true);
}

void mca_pml_ucx_send_completion(void *request, ucs_status_t status)
{
    mca_pml_ucx_send_completion_internal(request, status);
}

void mca_pml_ucx_bsend_completion(void *request, ucs_status_t status)
{
    mca_pml_ucx_bsend_completion_internal(request, status);
}

void mca_pml_ucx_recv_completion(void *request, ucs_status_t status,
                                 ucp_tag_recv_info_t *info)
{
    mca_pml_ucx_recv_completion_internal(request, status, info);
}

void mca_pml_ucx_send_nbx_completion(void *request, ucs_status_t status,
                                     void *user_data)
{
    mca_pml_ucx_send_completion_internal(request, status);
}

void mca_pml_ucx_bsend_nbx_completion(void *request, ucs_status_t status,
                                      void *user_data)
{
    mca_pml_ucx_bsend_completion_internal(request, status);
}

void mca_pml_ucx_recv_nbx_completion(void *request, ucs_status_t status,
                                     const ucp_tag_recv_info_t *info,
                                     void *user_data)
{
    mca_pml_ucx_recv_completion_internal(request, status, info);
}

void mca_pml_ucx_psend_completion(void *request, ucs_status_t status)
{
    ompi_request_t *tmp_req = request;

    PML_UCX_VERBOSE(8, "persistent send request %p completed with status %s",
                    (void*)tmp_req, ucs_status_string(status));

    mca_common_ucx_set_status(&tmp_req->req_status, status);
    mca_common_ucx_preq_completion(tmp_req);
}

void mca_pml_ucx_precv_completion(void *request, ucs_status_t status,
                                  ucp_tag_recv_info_t *info)
{
    ompi_request_t *tmp_req = request;

    PML_UCX_VERBOSE(8, "persistent receive request %p completed with status %s tag %"PRIx64" len %zu",
                    (void*)tmp_req, ucs_status_string(status), info->sender_tag,
                    info->length);

    mca_pml_ucx_set_recv_status(&tmp_req->req_status, status, info);
    mca_common_ucx_preq_completion(tmp_req);
}

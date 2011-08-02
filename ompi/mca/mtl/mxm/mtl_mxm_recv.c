/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "orte/util/show_help.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_request.h"


static void ompi_mtl_mxm_recv_completion_cb(void *context)
{
	mca_mtl_mxm_request_t *req = (mca_mtl_mxm_request_t *) context;
    struct ompi_request_t *ompi_req = req->super.ompi_req;
    mxm_recv_req_t *mxm_recv_req = (mxm_recv_req_t *)req->mxm_base_request;

    /* Set completion status and envelope */
    ompi_req->req_status.MPI_TAG    = mxm_recv_req->completion.sender_tag;
    ompi_req->req_status.MPI_SOURCE = mxm_recv_req->completion.sender_imm;
    ompi_req->req_status.MPI_ERROR  = ompi_mtl_mxm_to_mpi_status(req->mxm_base_request->error);
    ompi_req->req_status._ucount    = mxm_recv_req->completion.actual_len;

    /* Copy data */
    ompi_mtl_datatype_unpack(req->convertor, req->buf,
                             mxm_recv_req->completion.actual_len);

    if (req->free_after) {
        free(req->buf);
    }

    req->super.completion_callback(&req->super);
}


int ompi_mtl_mxm_irecv(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t *comm, int src, int tag,
                       struct opal_convertor_t *convertor,
                       struct mca_mtl_request_t *mtl_request)
{
    mca_mtl_mxm_request_t * mtl_mxm_request;
    mxm_error_t err;
    mxm_recv_req_t *mxm_recv_req;
    int ret;

    mtl_mxm_request = (mca_mtl_mxm_request_t*) mtl_request;

    mtl_mxm_request->convertor 	= convertor;
    ret = ompi_mtl_datatype_recv_buf(mtl_mxm_request->convertor,
                                     &mtl_mxm_request->buf,
                                     &mtl_mxm_request->length,
                                     &mtl_mxm_request->free_after);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    mxm_recv_req = (mxm_recv_req_t *)mtl_mxm_request->mxm_base_request;

    /* prepare a receive request embedded in the MTL request */
    mxm_recv_req->base.state    = MXM_REQ_NEW;
    mxm_recv_req->base.mq       = (mxm_mq_h)comm->c_pml_comm;
    mxm_recv_req->tag      		= tag;
    mxm_recv_req->tag_mask  	= (tag == MPI_ANY_TAG) ? 0 : 0xffffffffU;
    mxm_recv_req->base.conn     = (src == MPI_ANY_SOURCE) ? NULL : ompi_mtl_mxm_conn_lookup(comm, src);

    mxm_recv_req->base.data.buffer.ptr 		= mtl_mxm_request->buf;
    mxm_recv_req->base.data.buffer.length	= mtl_mxm_request->length;
    mxm_recv_req->base.completed_cb 		= ompi_mtl_mxm_recv_completion_cb;
    mxm_recv_req->base.context 				= mtl_mxm_request;

    /* post-recv */
    err = mxm_req_recv(mxm_recv_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting receive", true,
                       mxm_error_string(err), mtl_mxm_request->buf, mtl_mxm_request->length);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


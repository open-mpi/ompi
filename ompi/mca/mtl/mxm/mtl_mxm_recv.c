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


static void ompi_mtl_mxm_recv_completion_cb(mxm_req_t *req)
{
    mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t *) req->context;
    struct ompi_request_t *ompi_req = mtl_mxm_request->super.ompi_req;

    /* Set completion status and envelope */
    ompi_req->req_status.MPI_TAG    = req->completion.sender_tag;
    ompi_req->req_status.MPI_SOURCE = req->completion.sender_imm;
    ompi_req->req_status.MPI_ERROR  = ompi_mtl_mxm_to_mpi_status(req->completion.status);
    ompi_req->req_status._ucount    = req->completion.actual_len;

    /* Copy data */
    ompi_mtl_datatype_unpack(mtl_mxm_request->convertor, mtl_mxm_request->buf,
                             req->completion.actual_len);

    if (mtl_mxm_request->free_after) {
        free(mtl_mxm_request->buf);
    }

    mtl_mxm_request->super.completion_callback(&mtl_mxm_request->super);
}


int ompi_mtl_mxm_irecv(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t *comm, int src, int tag,
                       struct opal_convertor_t *convertor,
                       struct mca_mtl_request_t *mtl_request)
{
    mca_mtl_mxm_request_t * mtl_mxm_request;
    mca_mtl_mxm_endpoint_t* mxm_endpoint;
    ompi_proc_t* ompi_proc;
    mxm_error_t err;
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

    /* prepare a receive request embedded in the MTL request */
    mtl_mxm_request->mxm_request.state     = MXM_REQ_NEW;
    mtl_mxm_request->mxm_request.mq        = (mxm_mq_h)comm->c_pml_comm;
    mtl_mxm_request->mxm_request.tag       = tag;
    mtl_mxm_request->mxm_request.tag_mask  = (tag == MPI_ANY_TAG) ? 0 : 0xffffffffU;
    mtl_mxm_request->mxm_request.conn      = (src == MPI_ANY_SOURCE) ? NULL :
                                               ompi_mtl_mxm_conn_lookup(comm, src);

    mtl_mxm_request->mxm_request.data.buf.ptr = mtl_mxm_request->buf;
    mtl_mxm_request->mxm_request.data.buf.len = mtl_mxm_request->length;
    mtl_mxm_request->mxm_request.completed_cb = ompi_mtl_mxm_recv_completion_cb;
    mtl_mxm_request->mxm_request.context      = mtl_mxm_request;
    mtl_mxm_request->mxm_request.flags        = MXM_REQ_FLAG_NONBLOCK;

    /* post-recv */
    err = mxm_req_recv(&mtl_mxm_request->mxm_request);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting receive", true,
                       mxm_error_string(err), mtl_mxm_request->buf, mtl_mxm_request->length);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


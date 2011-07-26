/* * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/pml/pml.h"
#include "opal/datatype/opal_convertor.h"
#include "orte/util/show_help.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_request.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"


static void ompi_mtl_mxm_send_completion_cb(mxm_req_t *req)
{

    mca_mtl_mxm_request_t *mtl_mxm_request;
    mtl_mxm_request = (mca_mtl_mxm_request_t *) req->context;

    if (mtl_mxm_request->free_after) {
        free(mtl_mxm_request->buf);
    }

    switch (req->completion.status) {
    case MXM_OK:
        mtl_mxm_request->super.ompi_req->req_status.MPI_ERROR
                = OMPI_SUCCESS;
        break;
    case MXM_ERR_MESSAGE_TRUNCATED:
        mtl_mxm_request->super.ompi_req->req_status.MPI_ERROR
                = MPI_ERR_TRUNCATE;
        break;
    default:
        mtl_mxm_request->super.ompi_req->req_status.MPI_ERROR
                = MPI_ERR_INTERN;
        break;
    }

    mtl_mxm_request->super.completion_callback(&mtl_mxm_request->super);
}

int ompi_mtl_mxm_send(struct mca_mtl_base_module_t* mtl,
                      struct ompi_communicator_t* comm, int dest, int tag,
                      struct opal_convertor_t *convertor,
                      mca_pml_base_send_mode_t mode)
{
    mxm_req_t mxm_req;
    bool free_after;
    mxm_error_t err;
    int ret;

    /* prepare local send request */
    mxm_req.state          = MXM_REQ_NEW;
    mxm_req.mq             = ompi_mtl_mxm_mq_lookup(comm);
    mxm_req.conn           = ompi_mtl_mxm_conn_lookup(comm, dest);
    mxm_req.tag            = tag;
    mxm_req.imm_data       = ompi_comm_rank(comm);
    mxm_req.completed_cb   = NULL;
    mxm_req.flags          = 0;
    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mxm_req.flags |= MXM_REQ_FLAG_SEND_SYNC;
    }
    ret = ompi_mtl_datatype_pack(convertor, &mxm_req.data.buf.ptr, &mxm_req.data.buf.len,
                                 &free_after);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* post-send */
    err = mxm_req_send(&mxm_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting send", true, 0, mxm_error_string(err));
        return OMPI_ERROR;
    }

    /* wait for request completion */
    err = mxm_req_wait(&mxm_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error while waiting in send", true, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_isend(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t* comm, int dest, int tag,
                       struct opal_convertor_t *convertor,
                       mca_pml_base_send_mode_t mode, bool blocking,
                       mca_mtl_request_t * mtl_request)
{
    mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t *)mtl_request;
    mxm_error_t err;
    int ret;

    assert(mtl == &ompi_mtl_mxm.super);

    mtl_mxm_request->convertor = convertor;
    ret = ompi_mtl_datatype_pack(mtl_mxm_request->convertor,
                                 &mtl_mxm_request->buf,
                                 &mtl_mxm_request->length,
                                 &mtl_mxm_request->free_after);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* prepare a send request embedded in the MTL request */
    mtl_mxm_request->mxm_request.state        = MXM_REQ_NEW;
    mtl_mxm_request->mxm_request.mq           = ompi_mtl_mxm_mq_lookup(comm);
    mtl_mxm_request->mxm_request.conn         = ompi_mtl_mxm_conn_lookup(comm, dest);
    mtl_mxm_request->mxm_request.tag          = tag;
    mtl_mxm_request->mxm_request.imm_data     = ompi_comm_rank(comm);
    mtl_mxm_request->mxm_request.data.buf.ptr = mtl_mxm_request->buf;
    mtl_mxm_request->mxm_request.data.buf.len = mtl_mxm_request->length;
    mtl_mxm_request->mxm_request.completed_cb = ompi_mtl_mxm_send_completion_cb;
    mtl_mxm_request->mxm_request.context      = mtl_mxm_request;
    mtl_mxm_request->mxm_request.flags        = MXM_REQ_FLAG_NONBLOCK;
    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mtl_mxm_request->mxm_request.flags |= MXM_REQ_FLAG_SEND_SYNC;
    }

    /* post-send */
    err = mxm_req_send(&mtl_mxm_request->mxm_request);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting send", true, 1, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

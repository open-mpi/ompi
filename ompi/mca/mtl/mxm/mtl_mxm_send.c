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


static void ompi_mtl_mxm_send_completion_cb(void *context)
{
	mca_mtl_mxm_request_t *mtl_mxm_request = context;

    if (mtl_mxm_request->free_after) {
        free(mtl_mxm_request->buf);
    }

    mtl_mxm_request->super.ompi_req->req_status.MPI_ERROR  = ompi_mtl_mxm_to_mpi_status(mtl_mxm_request->mxm.base.error);

    mtl_mxm_request->super.completion_callback(&mtl_mxm_request->super);
}

int ompi_mtl_mxm_send(struct mca_mtl_base_module_t* mtl,
                      struct ompi_communicator_t* comm, int dest, int tag,
                      struct opal_convertor_t *convertor,
                      mca_pml_base_send_mode_t mode)
{
    mxm_send_req_t mxm_send_req;
    bool free_after;
    mxm_error_t err;
    int ret;

    /* prepare local send request */
    mxm_send_req.base.state         = MXM_REQ_NEW;
    mxm_send_req.base.mq            = ompi_mtl_mxm_mq_lookup(comm);
    mxm_send_req.base.conn          = ompi_mtl_mxm_conn_lookup(comm, dest);
    mxm_send_req.base.flags         = MXM_REQ_FLAG_WAIT;
    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mxm_send_req.base.flags |= MXM_REQ_FLAG_SEND_SYNC;
    }

    mxm_send_req.base.data_type     = MXM_REQ_DATA_BUFFER;
    ret = ompi_mtl_datatype_pack(convertor, &mxm_send_req.base.data.buffer.ptr,
                                 &mxm_send_req.base.data.buffer.length,
                                 &free_after);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    mxm_send_req.base.data.buffer.mkey   = MXM_MKEY_NONE;
    mxm_send_req.base.context            = NULL;
    mxm_send_req.base.completed_cb       = NULL;
    mxm_send_req.opcode                  = MXM_REQ_OP_SEND;
    mxm_send_req.op.send.tag             = tag;
    mxm_send_req.op.send.imm_data        = ompi_comm_rank(comm);


    /* post-send */
    err = mxm_req_send(&mxm_send_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting send", true, 0, mxm_error_string(err));
        return OMPI_ERROR;
    }

    /* wait for request completion */
    mxm_req_wait(&mxm_send_req.base);

    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_isend(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t* comm, int dest, int tag,
                       struct opal_convertor_t *convertor,
                       mca_pml_base_send_mode_t mode, bool blocking,
                       mca_mtl_request_t * mtl_request)
{
    mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t *)mtl_request;
    mxm_send_req_t *mxm_send_req;
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

    mxm_send_req = &mtl_mxm_request->mxm.send;

    /* prepare a send request embedded in the MTL request */
    mxm_send_req->base.state               = MXM_REQ_NEW;
    mxm_send_req->base.mq                  = ompi_mtl_mxm_mq_lookup(comm);
    mxm_send_req->base.conn                = ompi_mtl_mxm_conn_lookup(comm, dest);
    mxm_send_req->base.flags               = 0;
    mxm_send_req->base.data_type           = MXM_REQ_DATA_BUFFER;
    mxm_send_req->base.data.buffer.ptr     = mtl_mxm_request->buf;
    mxm_send_req->base.data.buffer.length  = mtl_mxm_request->length;
    mxm_send_req->base.data.buffer.mkey    = MXM_MKEY_NONE;
    mxm_send_req->base.context             = mtl_mxm_request;
    mxm_send_req->base.completed_cb        = ompi_mtl_mxm_send_completion_cb;
    mxm_send_req->opcode                   = MXM_REQ_OP_SEND;
    mxm_send_req->op.send.tag              = tag;
    mxm_send_req->op.send.imm_data         = ompi_comm_rank(comm);

    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mxm_send_req->base.flags |= MXM_REQ_FLAG_SEND_SYNC;
    }

    /* post-send */
    err = mxm_req_send(mxm_send_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting send", true, 1, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/message/message.h"
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
    mxm_recv_req_t *mxm_recv_req = &req->mxm.recv;

    /* Set completion status and envelope */
    ompi_mtl_mxm_to_mpi_status(mxm_recv_req->base.error, &ompi_req->req_status);
    ompi_req->req_status.MPI_TAG    = mxm_recv_req->completion.sender_tag;
    ompi_req->req_status.MPI_SOURCE = mxm_recv_req->completion.sender_imm;
    ompi_req->req_status._ucount    = mxm_recv_req->completion.actual_len;

    /* Copy data and free the buffer */
    ompi_mtl_datatype_unpack(req->convertor, req->buf,
                             mxm_recv_req->completion.actual_len);
    req->super.completion_callback(&req->super);
}

static inline __opal_attribute_always_inline__ int
        ompi_mtl_mxm_recv_init(mca_mtl_mxm_request_t *mtl_mxm_request,
                               opal_convertor_t *convertor,
                               mxm_recv_req_t *mxm_recv_req)
{
    int ret;

    mtl_mxm_request->convertor = convertor;
    ret = ompi_mtl_datatype_recv_buf(convertor,
                                     &mtl_mxm_request->buf,
                                     &mtl_mxm_request->length,
                                     &mtl_mxm_request->free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    mxm_recv_req->base.state               = MXM_REQ_NEW;

    mxm_recv_req->base.flags               = 0;
    mxm_recv_req->base.data_type           = MXM_REQ_DATA_BUFFER;
    mxm_recv_req->base.data.buffer.ptr     = mtl_mxm_request->buf;
    mxm_recv_req->base.data.buffer.length  = mtl_mxm_request->length;
    mxm_recv_req->base.data.buffer.mkey    = MXM_MKEY_NONE;
    mxm_recv_req->base.context             = mtl_mxm_request;
    mxm_recv_req->base.completed_cb        = ompi_mtl_mxm_recv_completion_cb;

    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_irecv(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t *comm, int src, int tag,
                       struct opal_convertor_t *convertor,
                       struct mca_mtl_request_t *mtl_request)
{
    int ret;
    mxm_error_t err;
    mxm_recv_req_t *mxm_recv_req;
    mca_mtl_mxm_request_t * mtl_mxm_request;

    mtl_mxm_request = (mca_mtl_mxm_request_t*) mtl_request;
    mxm_recv_req = &mtl_mxm_request->mxm.recv;

    ompi_mtl_mxm_set_recv_envelope(mxm_recv_req, comm, src, tag);

    /* prepare a receive request embedded in the MTL request */
    ret = ompi_mtl_mxm_recv_init(mtl_mxm_request, convertor, mxm_recv_req);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* post-recv */
    err = mxm_req_recv(mxm_recv_req);
    if (OPAL_UNLIKELY(MXM_OK != err)) {
        orte_show_help("help-mtl-mxm.txt", "error posting receive", true,
                       mxm_error_string(err), mtl_mxm_request->buf, mtl_mxm_request->length);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_imrecv(struct mca_mtl_base_module_t* mtl,
                        struct opal_convertor_t *convertor,
                        struct ompi_message_t **message,
                        struct mca_mtl_request_t *mtl_request)
{
#if MXM_API >= 0x01010000
    int ret;
    mxm_error_t err;
    mxm_recv_req_t *mxm_recv_req;
    mca_mtl_mxm_request_t *mtl_mxm_request;

    ompi_mtl_mxm_message_t *msgp =
                        (ompi_mtl_mxm_message_t *) (*message)->req_ptr;

    mtl_mxm_request = (mca_mtl_mxm_request_t*) mtl_request;
    mxm_recv_req = &mtl_mxm_request->mxm.recv;

    /* prepare a receive request embedded in the MTL request */
    ret = ompi_mtl_mxm_recv_init(mtl_mxm_request, convertor, mxm_recv_req);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    mxm_recv_req->tag       = msgp->tag;
    mxm_recv_req->tag_mask  = msgp->tag_mask;
    mxm_recv_req->base.mq   = msgp->mq;
    mxm_recv_req->base.conn = msgp->conn;

    err = mxm_message_recv(mxm_recv_req, msgp->mxm_msg);
    if (OPAL_UNLIKELY(MXM_OK != err)) {
        orte_show_help("help-mtl-mxm.txt", "error posting message receive", true,
                       mxm_error_string(err), mtl_mxm_request->buf, mtl_mxm_request->length);
        return OMPI_ERROR;
    }

    OMPI_FREE_LIST_RETURN(&mca_mtl_mxm_component.mxm_messages,
                         (ompi_free_list_item_t *) msgp);

    ompi_message_return(*message);
    (*message) = MPI_MESSAGE_NULL;

    return OMPI_SUCCESS;
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
}

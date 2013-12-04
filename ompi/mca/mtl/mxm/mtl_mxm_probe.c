/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_mxm.h"
#include "mtl_mxm_types.h"

#include "ompi/message/message.h"
#include "ompi/communicator/communicator.h"

int ompi_mtl_mxm_iprobe(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm, int src, int tag,
                        int *flag, struct ompi_status_public_t *status)
{
    mxm_error_t err;
    mxm_recv_req_t req;

    req.base.state = MXM_REQ_NEW;
    ompi_mtl_mxm_set_recv_envelope(&req, comm, src, tag);

    err = mxm_req_probe(&req);
    if (MXM_OK == err) {
        *flag = 1;
        if (MPI_STATUS_IGNORE != status) {
            ompi_mtl_mxm_to_mpi_status(err, status);
            status->MPI_SOURCE = req.completion.sender_imm;
            status->MPI_TAG    = req.completion.sender_tag;
            status->_ucount    = req.completion.sender_len;
        }
        return OMPI_SUCCESS;
    } else if (MXM_ERR_NO_MESSAGE == err) {
        *flag = 0;
        return OMPI_SUCCESS;
    } else {
        return OMPI_ERROR;
    }
}


int ompi_mtl_mxm_improbe(struct mca_mtl_base_module_t *mtl,
                         struct ompi_communicator_t *comm,
                         int src,
                         int tag,
                         int *matched,
                         struct ompi_message_t **message,
                         struct ompi_status_public_t *status)
{
    mxm_error_t err;
    mxm_recv_req_t req;

    ompi_free_list_item_t *item;
    ompi_mtl_mxm_message_t *msgp;

    OMPI_FREE_LIST_WAIT_MT(&mca_mtl_mxm_component.mxm_messages, item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    msgp = (ompi_mtl_mxm_message_t *) item;

    req.base.state = MXM_REQ_NEW;
    ompi_mtl_mxm_set_recv_envelope(&req, comm, src, tag);

    msgp->mq       = req.base.mq;
    msgp->conn     = req.base.conn;
    msgp->tag      = req.tag;
    msgp->tag_mask = req.tag_mask;

    err = mxm_req_mprobe(&req, &msgp->mxm_msg);
    if (MXM_OK == err) {
        if (MPI_STATUS_IGNORE != status) {
            *matched = 1;
            ompi_mtl_mxm_to_mpi_status(err, status);
            status->MPI_SOURCE = req.completion.sender_imm;
            status->MPI_TAG    = req.completion.sender_tag;
            status->_ucount    = req.completion.sender_len;
        } else{
			*matched = 0;
			*message = MPI_MESSAGE_NULL;
			return OMPI_SUCCESS;
		}
    } else if (MXM_ERR_NO_MESSAGE == err) {
        *matched = 0;
        *message = MPI_MESSAGE_NULL;
        return OMPI_SUCCESS;
    } else {
        return OMPI_ERROR;
    }

	(*message) = ompi_message_alloc();
	if (OPAL_UNLIKELY(NULL == (*message))) {
        *matched = 0;
		*message = MPI_MESSAGE_NULL;
		return OMPI_ERR_OUT_OF_RESOURCE;
	}

	(*message)->comm = comm;
	(*message)->req_ptr = msgp;
	(*message)->peer = status->MPI_SOURCE;
	(*message)->count = status->_ucount;

    return OMPI_SUCCESS;
}

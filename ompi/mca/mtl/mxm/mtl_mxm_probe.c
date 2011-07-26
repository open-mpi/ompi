/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_mxm.h"
#include "mtl_mxm_types.h"

#include "ompi/communicator/communicator.h"

int ompi_mtl_mxm_iprobe(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm, int src, int tag,
                        int *flag, struct ompi_status_public_t *status)
{
    mxm_error_t err;
    mxm_req_t req;

    req.state      = MXM_REQ_NEW;
    req.mq         = (mxm_mq_h)comm->c_pml_comm;
    req.tag        = tag;
    req.tag_mask   = (tag == MPI_ANY_TAG) ? 0 : 0xffffffffU;
    req.conn       = (src == MPI_ANY_SOURCE) ? NULL : ompi_mtl_mxm_conn_lookup(comm, src);

    err = mxm_req_probe(&req);
    if (MXM_OK == err) {
        *flag = 1;
        if (MPI_STATUS_IGNORE != status) {
            status->MPI_SOURCE = *(int *)mxm_conn_get_context(req.conn);
            status->MPI_TAG    = req.completion.sender_tag;
            status->MPI_ERROR  = ompi_mtl_mxm_to_mpi_status(req.completion.status);
            status->_ucount    = req.completion.actual_len;
        }
        return OMPI_SUCCESS;
    } else if (MXM_ERR_NO_MESSAGE == err) {
        *flag = 0;
        return OMPI_SUCCESS;
    } else {
        return OMPI_ERROR;
    }
}

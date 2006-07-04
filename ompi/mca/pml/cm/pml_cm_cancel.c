/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/request/request.h"
#include "ompi/mca/pml/base/pml_base_request.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"
#include "pml_cm_recvreq.h"

int
mca_pml_cm_cancel(struct ompi_request_t *request, int flag)
{
    int ret;
    mca_pml_base_request_t *base_request = 
        (mca_pml_base_request_t*) request;
    
    switch (base_request->req_type) {
    case MCA_PML_REQUEST_SEND:
        {
            mca_pml_cm_send_request_t* sendreq = 
                (mca_pml_cm_send_request_t*) request;
            ret = OMPI_MTL_CALL(cancel(ompi_mtl,
                                       &sendreq->req_mtl,
                                       flag));
        }
        break;
    case MCA_PML_REQUEST_RECV:
        {
            mca_pml_cm_recv_request_t* recvreq = 
                (mca_pml_cm_recv_request_t*) request;
            ret = OMPI_MTL_CALL(cancel(ompi_mtl,
                                       &recvreq->req_mtl,
                                       flag));
        }
        break;
    default:
        ret = OMPI_SUCCESS;
    }

    return ret;
}

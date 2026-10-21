/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include "ompi/request/request.h"
#include "ompi/mca/pml/base/pml_base_request.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"
#include "pml_cm_recvreq.h"

int
mca_pml_cm_cancel(struct ompi_request_t *ompi_req, int flag)
{
    int ret;
    mca_pml_cm_request_t *base_request =
         (mca_pml_cm_request_t*) ompi_req;
    mca_mtl_request_t *mtl_req = NULL;

    /* A request parked until its peer's connection info arrives has
     * never reached the MTL, so cancelling it is ours to do. */
    if (mca_pml_cm_unstage_request(base_request)) {
        base_request->req_ompi.req_status._cancelled = 1;
        if (MCA_PML_CM_REQUEST_SEND_HEAVY == base_request->req_pml_type) {
            mca_pml_cm_hvy_send_request_t *sendreq =
                (mca_pml_cm_hvy_send_request_t *) base_request;
            MCA_PML_CM_HVY_SEND_REQUEST_PML_COMPLETE(sendreq);
        } else {
            mca_pml_cm_hvy_recv_request_t *recvreq =
                (mca_pml_cm_hvy_recv_request_t *) base_request;
            MCA_PML_CM_HVY_RECV_REQUEST_PML_COMPLETE(recvreq);
        }
        return OMPI_SUCCESS;
    }

    switch (base_request->req_pml_type) {
    case MCA_PML_CM_REQUEST_SEND_HEAVY:
        {
            mca_pml_cm_hvy_send_request_t *request =
                (mca_pml_cm_hvy_send_request_t*) base_request;
            mtl_req = &request->req_mtl;
        }
        break;

    case MCA_PML_CM_REQUEST_SEND_THIN:
        {
            mca_pml_cm_thin_send_request_t *request =
                (mca_pml_cm_thin_send_request_t*) base_request;
            mtl_req = &request->req_mtl;
        }
        break;

    case MCA_PML_CM_REQUEST_RECV_HEAVY:
        {
            mca_pml_cm_hvy_recv_request_t *request =
                (mca_pml_cm_hvy_recv_request_t*) base_request;
            mtl_req = &request->req_mtl;
        }
        break;

    case MCA_PML_CM_REQUEST_RECV_THIN:
        {
            mca_pml_cm_thin_recv_request_t *request =
                (mca_pml_cm_thin_recv_request_t*) base_request;
            mtl_req = &request->req_mtl;
        }
        break;

    default:
        ret = OMPI_ERROR;
    }

    ret = OMPI_MTL_CALL(cancel(ompi_mtl,
                               mtl_req,
                               flag));

    return ret;
}

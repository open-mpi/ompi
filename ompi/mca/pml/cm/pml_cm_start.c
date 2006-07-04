/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"
#include "pml_cm_recvreq.h"


int
mca_pml_cm_start(size_t count, ompi_request_t** requests)
{
    int rc;
    size_t i;

    for (i = 0 ; i < count ; i++) {
        mca_pml_base_request_t *pml_request = 
            (mca_pml_base_request_t*)requests[i];
        if (NULL == pml_request) continue;

        /* If the persistent request is currebtly active - obtain the
         * request lock and verify the status is incomplete. if the
         * pml layer has not completed the request - mark the request
         * as free called - so that it will be freed when the request
         * completes - and create a new request.
         */
        switch (pml_request->req_ompi.req_state) {
            case OMPI_REQUEST_INACTIVE:
                if (pml_request->req_pml_complete == true)
                    break;

            case OMPI_REQUEST_ACTIVE: {
                /* otherwise fall through */
                ompi_request_t *request;
                OPAL_THREAD_LOCK(&ompi_request_lock);
                if (pml_request->req_pml_complete == false) {
                    /* free request after it completes */
                    pml_request->req_free_called = true;
                } else {
                    /* can reuse the existing request */
                    OPAL_THREAD_UNLOCK(&ompi_request_lock);
                    break;
                }

                /* allocate a new request */
                switch (pml_request->req_type) {
                    case MCA_PML_REQUEST_SEND: {
                         mca_pml_base_send_mode_t sendmode = 
                             ((mca_pml_base_send_request_t*)pml_request)->req_send_mode;
                         rc = mca_pml_cm_isend_init(
                              pml_request->req_addr,
                              pml_request->req_count,
                              pml_request->req_datatype,
                              pml_request->req_peer,
                              pml_request->req_tag,
                              sendmode,
                              pml_request->req_comm,
                              &request);
                         break;
                    }
                    case MCA_PML_REQUEST_RECV:
                         rc = mca_pml_cm_irecv_init(
                              pml_request->req_addr,
                              pml_request->req_count,
                              pml_request->req_datatype,
                              pml_request->req_peer,
                              pml_request->req_tag,
                              pml_request->req_comm,
                              &request);
                         break;
                    default:
                         rc = OMPI_ERR_REQUEST;
                         break;
                }
                OPAL_THREAD_UNLOCK(&ompi_request_lock);
                if(OMPI_SUCCESS != rc)
                    return rc;
                pml_request = (mca_pml_base_request_t*)request;
                requests[i] = request;
                break;
            }
            default:
                return OMPI_ERR_REQUEST;
        }

        /* start the request */
        switch (pml_request->req_type) {
            case MCA_PML_REQUEST_SEND: 
            {
                mca_pml_cm_send_request_t* sendreq =
                    (mca_pml_cm_send_request_t*)pml_request;
                MCA_PML_CM_SEND_REQUEST_START(sendreq, rc);
                if(rc != OMPI_SUCCESS)
                    return rc;
                break;
            }
            case MCA_PML_REQUEST_RECV:
            {
                mca_pml_cm_recv_request_t* recvreq =
                    (mca_pml_cm_recv_request_t*)pml_request;
                MCA_PML_CM_RECV_REQUEST_START(recvreq, rc);
                if(rc != OMPI_SUCCESS)
                    return rc;
                break;
            }
            default:
                return OMPI_ERR_REQUEST;
        }
    }
    return OMPI_SUCCESS;

}

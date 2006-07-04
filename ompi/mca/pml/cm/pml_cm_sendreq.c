/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "pml_cm.h"
#include "pml_cm_sendreq.h"


/*
 * The free call mark the final stage in a request
 * life-cycle. Starting from this point the request is completed at
 * both PML and user level, and can be used for others p2p
 * communications. Therefore, in the case of the CM PML it should be
 * added to the free request list.
 */
static int
mca_pml_cm_send_request_free(struct ompi_request_t** request)
{
    mca_pml_cm_send_request_t* sendreq = *(mca_pml_cm_send_request_t**)request;
    
    assert( false == sendreq->req_send.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    sendreq->req_send.req_base.req_free_called = true;
    if( true == sendreq->req_send.req_base.req_pml_complete ) {
        MCA_PML_CM_SEND_REQUEST_RETURN( sendreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}


static void 
sendreq_construct(mca_pml_cm_send_request_t* sendreq)
{
    sendreq->req_mtl.ompi_req = (ompi_request_t*) sendreq;
    sendreq->req_mtl.completion_callback = mca_pml_cm_request_completion;

    sendreq->req_send.req_base.req_ompi.req_free = mca_pml_cm_send_request_free;
    sendreq->req_send.req_base.req_ompi.req_cancel = mca_pml_cm_cancel;
}


static void 
sendreq_destruct(mca_pml_cm_send_request_t* sendreq)
{
    sendreq->req_mtl.ompi_req = NULL;
    sendreq->req_mtl.completion_callback = NULL;
}


OBJ_CLASS_INSTANCE(mca_pml_cm_send_request_t,
                   mca_pml_base_send_request_t,
                   sendreq_construct,
                   sendreq_destruct);

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

#include "pml_cm.h"
#include "pml_cm_recvreq.h"


static int
mca_pml_cm_thin_recv_request_free(struct ompi_request_t** request)
{
    mca_pml_cm_request_t* recvreq = *(mca_pml_cm_request_t**)request; 

    assert( false == recvreq->req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_free_called = true;
    if( true == recvreq->req_pml_complete ) {
        MCA_PML_CM_THIN_RECV_REQUEST_RETURN((mca_pml_cm_thin_recv_request_t*)  recvreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
} 

static int
mca_pml_cm_hvy_recv_request_free(struct ompi_request_t** request)
{
    mca_pml_cm_request_t* recvreq = *(mca_pml_cm_request_t**)request; 
    
    assert( false == recvreq->req_free_called );
    
    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_free_called = true;
    if( true == recvreq->req_pml_complete ) {
        MCA_PML_CM_HVY_RECV_REQUEST_RETURN((mca_pml_cm_hvy_recv_request_t*)  recvreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
} 


static void 
mca_pml_cm_thin_recv_request_construct(mca_pml_cm_thin_recv_request_t* recvreq)
{
    recvreq->req_mtl.ompi_req = (ompi_request_t*) recvreq;
    recvreq->req_mtl.completion_callback = mca_pml_cm_thin_recv_request_completion;

    recvreq->req_base.req_ompi.req_free = mca_pml_cm_thin_recv_request_free;
    recvreq->req_base.req_ompi.req_cancel = mca_pml_cm_cancel;
    recvreq->req_base.req_pml_type = MCA_PML_CM_REQUEST_RECV_THIN;
}


static void 
mca_pml_cm_hvy_recv_request_construct(mca_pml_cm_hvy_recv_request_t* recvreq)
{
    recvreq->req_mtl.ompi_req = (ompi_request_t*) recvreq;
    recvreq->req_mtl.completion_callback = mca_pml_cm_hvy_recv_request_completion;

    recvreq->req_base.req_ompi.req_free = mca_pml_cm_hvy_recv_request_free;
    recvreq->req_base.req_ompi.req_cancel = mca_pml_cm_cancel;
    recvreq->req_base.req_pml_type = MCA_PML_CM_REQUEST_RECV_HEAVY;
}



OBJ_CLASS_INSTANCE(mca_pml_cm_thin_recv_request_t,
                   mca_pml_cm_request_t,
                   mca_pml_cm_thin_recv_request_construct,
                   NULL);


OBJ_CLASS_INSTANCE(mca_pml_cm_hvy_recv_request_t,
                   mca_pml_cm_request_t,
                   mca_pml_cm_hvy_recv_request_construct,
                   NULL);

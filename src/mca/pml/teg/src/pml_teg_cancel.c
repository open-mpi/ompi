/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_teg.h"

int mca_pml_teg_cancel(ompi_request_t* request)
{
    mca_pml_base_request_t* teg_request = (mca_pml_base_request_t*)request;

    if( true == request->req_complete ) {  /* way to late to cancel this one */
        return OMPI_SUCCESS;
    }
    /* we dont cancel send requests by now */
    if( MCA_PML_REQUEST_SEND == teg_request->req_type ) {
        return OMPI_SUCCESS;
    }
    request->req_status._cancelled = true;
    request->req_complete = true;  /* mark it as completed so all the test/wait  functions
                                    * on this particular request will finish */
    /* Now we have a problem if we are in a multi-threaded environment. We shou ld
     * broadcast the condition on the request in order to allow the other threa ds
     * to complete their test/wait functions.
     */
    if(ompi_request_waiting) {
        ompi_condition_broadcast(&ompi_request_cond);
    }
    return OMPI_SUCCESS;
}

int mca_pml_teg_cancelled(ompi_request_t* request, int* flag)
{
    if(NULL != flag)
        *flag = (true == request->req_status._cancelled ? 1 : 0);
    return OMPI_SUCCESS;
}


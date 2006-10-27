/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/request/grequest.h"


static int ompi_grequest_free(ompi_request_t** req)
{
    int rc = OMPI_SUCCESS;
    ompi_grequest_t* greq = *(ompi_grequest_t**)req;
    if(greq->greq_free.c_free != NULL)
        rc = greq->greq_free.c_free(greq->greq_state);
    if(rc == OMPI_SUCCESS) {
        OBJ_RELEASE(greq);
        *req = MPI_REQUEST_NULL;
    }
    return rc;
}

static int ompi_grequest_cancel(ompi_request_t* req, int flag)
{
    int rc = OMPI_SUCCESS;
    ompi_grequest_t* greq = (ompi_grequest_t*)req;
    if(greq->greq_cancel.c_cancel != NULL)
        rc = greq->greq_cancel.c_cancel(greq->greq_state, flag);
    return rc;
}

static void ompi_grequest_construct(ompi_grequest_t* greq)
{
    greq->greq_base.req_free     = ompi_grequest_free;
    greq->greq_base.req_cancel   = ompi_grequest_cancel;
    greq->greq_base.req_type = OMPI_REQUEST_GEN;
    greq->greq_base.req_mpi_object.comm = &ompi_mpi_comm_world;
}

static void ompi_grequest_destruct(ompi_grequest_t* greq)
{
    OMPI_REQUEST_FINI(&greq->greq_base);
}


OBJ_CLASS_INSTANCE(
    ompi_grequest_t,
    ompi_request_t,
    ompi_grequest_construct,
    ompi_grequest_destruct);


int ompi_grequest_start(
    MPI_Grequest_query_function *gquery_fn,
    MPI_Grequest_free_function *gfree_fn,
    MPI_Grequest_cancel_function *gcancel_fn,
    void* gstate,
    ompi_request_t** request)
{
    ompi_grequest_t *greq = OBJ_NEW(ompi_grequest_t);
    if(greq == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    greq->greq_base.req_state = OMPI_REQUEST_ACTIVE;
    greq->greq_state = gstate;
    greq->greq_query.c_query = gquery_fn;
    greq->greq_free.c_free = gfree_fn;
    greq->greq_cancel.c_cancel = gcancel_fn; 
    *request = &greq->greq_base;
    return OMPI_SUCCESS;
}

int ompi_grequest_complete(ompi_grequest_t* grequest)
{
    int rc = OMPI_SUCCESS;
    OPAL_THREAD_LOCK(&ompi_request_lock);
    grequest->greq_base.req_complete = true;
    grequest->greq_base.req_status = ompi_status_empty;
    if(grequest->greq_query.c_query != NULL)
        rc = grequest->greq_query.c_query(grequest->greq_state, &grequest->greq_base.req_status);
    if(ompi_request_waiting)
        opal_condition_signal(&ompi_request_cond);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    return rc;
}


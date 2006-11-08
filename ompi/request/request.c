/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_object.h"
#include "ompi/request/request.h"
#include "ompi/constants.h"

ompi_pointer_array_t             ompi_request_f_to_c_table;
size_t                           ompi_request_waiting = 0;
size_t                           ompi_request_completed = 0;
opal_mutex_t                     ompi_request_lock;
opal_condition_t                 ompi_request_cond;
ompi_request_t                   ompi_request_null;
ompi_request_t                   ompi_request_empty;
ompi_status_public_t             ompi_status_empty;


static void ompi_request_construct(ompi_request_t* req)
{
    OMPI_REQUEST_INIT(req, false);
    req->req_free = NULL;
    req->req_cancel = NULL;
    req->req_f_to_c_index = MPI_UNDEFINED;
    req->req_mpi_object.comm = (struct ompi_communicator_t*) NULL;
}

static void ompi_request_destruct(ompi_request_t* req)
{
    assert( MPI_UNDEFINED == req->req_f_to_c_index );
    assert( OMPI_REQUEST_INVALID == req->req_state );
}

static int ompi_request_null_free(ompi_request_t** request)
{
    return OMPI_SUCCESS;
}

static int ompi_request_null_cancel(ompi_request_t* request, int flag)
{
    return OMPI_SUCCESS;
}

static int ompi_request_empty_free(ompi_request_t** request)
{
    *request = &ompi_request_null;
    return OMPI_SUCCESS;
}

int ompi_request_persistent_proc_null_free(ompi_request_t** request)
{
    OMPI_REQUEST_FINI(*request);
    (*request)->req_state = OMPI_REQUEST_INVALID;
    OBJ_RELEASE(*request);
    *request = &ompi_request_null;
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(
    ompi_request_t,
    ompi_free_list_item_t,
    ompi_request_construct,
    ompi_request_destruct);


int ompi_request_init(void)
{
    OBJ_CONSTRUCT(&ompi_request_f_to_c_table, ompi_pointer_array_t);
    OBJ_CONSTRUCT(&ompi_request_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ompi_request_cond, opal_condition_t);
    OBJ_CONSTRUCT(&ompi_request_null, ompi_request_t);
    OBJ_CONSTRUCT(&ompi_request_empty, ompi_request_t);

    ompi_request_null.req_status.MPI_SOURCE = MPI_PROC_NULL;
    ompi_request_null.req_status.MPI_TAG = MPI_ANY_TAG;
    ompi_request_null.req_status.MPI_ERROR = MPI_SUCCESS;
    ompi_request_null.req_status._count = 0;
    ompi_request_null.req_status._cancelled = 0;

    ompi_request_null.req_state = OMPI_REQUEST_INACTIVE;
    ompi_request_null.req_complete = true;
    ompi_request_null.req_type = OMPI_REQUEST_NULL;
    ompi_request_null.req_free = ompi_request_null_free;
    ompi_request_null.req_cancel = ompi_request_null_cancel;
    ompi_request_null.req_f_to_c_index = 
        ompi_pointer_array_add(&ompi_request_f_to_c_table, &ompi_request_null);

    if (0 != ompi_request_null.req_f_to_c_index) {
        return OMPI_ERR_REQUEST;
    }

    /* We need a way to distinguish between the user provided
     * MPI_REQUEST_NULL to MPI_Wait* and a non-active (MPI_PROC_NULL)
     * request passed to any P2P non-blocking function.
     *
     * The main difference to ompi_request_null is
     * req_state being OMPI_REQUEST_ACTIVE, so that MPI_Waitall
     * does not set the status to ompi_status_empty and the different
     * req_free function, which resets the
     * request to MPI_REQUEST_NULL.
     * The req_cancel function need not be changed.
     */
    ompi_request_empty.req_status.MPI_SOURCE = MPI_PROC_NULL;
    ompi_request_empty.req_status.MPI_TAG = MPI_ANY_TAG;
    ompi_request_empty.req_status.MPI_ERROR = MPI_SUCCESS;
    ompi_request_empty.req_status._count = 0;
    ompi_request_empty.req_status._cancelled = 0;

    ompi_request_empty.req_state = OMPI_REQUEST_ACTIVE;
    ompi_request_empty.req_complete = true;
    ompi_request_empty.req_type = OMPI_REQUEST_NULL;
    ompi_request_empty.req_free = ompi_request_empty_free;
    ompi_request_empty.req_cancel = ompi_request_null_cancel;
    ompi_request_empty.req_f_to_c_index =
        ompi_pointer_array_add(&ompi_request_f_to_c_table, &ompi_request_empty);

    if (1 != ompi_request_empty.req_f_to_c_index) {
        return OMPI_ERR_REQUEST;
    }

    ompi_status_empty.MPI_SOURCE = MPI_ANY_SOURCE;
    ompi_status_empty.MPI_TAG = MPI_ANY_TAG;
    ompi_status_empty.MPI_ERROR = MPI_SUCCESS;
    ompi_status_empty._count = 0;
    ompi_status_empty._cancelled = 0;

    return OMPI_SUCCESS;
}


int ompi_request_finalize(void)
{
    OMPI_REQUEST_FINI( &ompi_request_null );
    OBJ_DESTRUCT( &ompi_request_null );
    OMPI_REQUEST_FINI( &ompi_request_empty );
    OBJ_DESTRUCT( &ompi_request_empty );
    OBJ_DESTRUCT( &ompi_request_cond );
    OBJ_DESTRUCT( &ompi_request_lock );
    OBJ_DESTRUCT( &ompi_request_f_to_c_table );
    return OMPI_SUCCESS;
}


int ompi_request_complete(ompi_request_t* request)
{
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_completed++;
    request->req_complete = true;
    if(ompi_request_waiting)
        opal_condition_signal(&ompi_request_cond);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    return OMPI_SUCCESS;
}


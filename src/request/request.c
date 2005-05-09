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

#include "class/ompi_object.h"
#include "request/request.h"
#include "include/constants.h"

ompi_pointer_array_t  ompi_request_f_to_c_table;
OMPI_DECLSPEC volatile int          ompi_request_waiting = 0;
int                   ompi_request_poll_iterations = 20000;
OMPI_DECLSPEC ompi_mutex_t          ompi_request_lock;
OMPI_DECLSPEC ompi_condition_t      ompi_request_cond;
OMPI_DECLSPEC ompi_request_t        ompi_request_null;
ompi_status_public_t  ompi_status_empty;


static void ompi_request_construct(ompi_request_t* req)
{
    OMPI_REQUEST_INIT(req);
    req->req_fini = NULL;
    req->req_free = NULL;
    req->req_cancel = NULL;
    req->req_f_to_c_index = MPI_UNDEFINED;
}

static void ompi_request_destruct(ompi_request_t* req)
{
    OMPI_REQUEST_FINI(req);
}

static int ompi_request_null_free(ompi_request_t** request)
{
    return OMPI_SUCCESS;
}

static int ompi_request_null_cancel(ompi_request_t* request, int flag)
{
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(
    ompi_request_t,
    ompi_list_item_t,
    ompi_request_construct,
    ompi_request_destruct);


int ompi_request_init(void)
{
    OBJ_CONSTRUCT(&ompi_request_f_to_c_table, ompi_pointer_array_t);
    OBJ_CONSTRUCT(&ompi_request_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&ompi_request_cond, ompi_condition_t);
    OBJ_CONSTRUCT(&ompi_request_null, ompi_request_t);

    ompi_request_null.req_status.MPI_SOURCE = MPI_PROC_NULL;
    ompi_request_null.req_status.MPI_TAG = MPI_ANY_TAG;
    ompi_request_null.req_status.MPI_ERROR = MPI_SUCCESS;
    ompi_request_null.req_status._count = 0;
    ompi_request_null.req_status._cancelled = 0;

    ompi_request_null.req_state = OMPI_REQUEST_INACTIVE;
    ompi_request_null.req_complete = true;
    ompi_request_null.req_type = OMPI_REQUEST_NULL;
    ompi_request_null.req_fini = ompi_request_null_free;
    ompi_request_null.req_free = ompi_request_null_free;
    ompi_request_null.req_cancel = ompi_request_null_cancel;
    ompi_request_null.req_f_to_c_index = 
        ompi_pointer_array_add(&ompi_request_f_to_c_table, &ompi_request_null);

    if (0 != ompi_request_null.req_f_to_c_index) {
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
    OBJ_DESTRUCT(&ompi_request_null);
    OBJ_DESTRUCT(&ompi_request_cond);
    OBJ_DESTRUCT(&ompi_request_lock);
    OBJ_DESTRUCT(&ompi_request_f_to_c_table);
    return OMPI_SUCCESS;
}


int ompi_request_complete(ompi_request_t* request)
{
    OMPI_THREAD_LOCK(&ompi_request_lock);
    request->req_complete = true;
    if(ompi_request_waiting)
        ompi_condition_signal(&ompi_request_cond);
    OMPI_THREAD_UNLOCK(&ompi_request_lock);
    return OMPI_SUCCESS;
}


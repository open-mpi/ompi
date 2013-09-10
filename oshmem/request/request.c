/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include "ompi/communicator/communicator.h" /* TODO: ompi_predefined_communicator_t*/
#include "opal/class/opal_object.h"
#include "oshmem/request/request.h"
#include "oshmem/constants.h"
#include "oshmem/proc/proc.h"

opal_pointer_array_t             oshmem_request_f_to_c_table;
size_t                           oshmem_request_waiting = 0;
size_t                           oshmem_request_completed = 0;
opal_mutex_t                     oshmem_request_lock;
opal_condition_t                 oshmem_request_cond;
oshmem_predefined_request_t        oshmem_request_null;
oshmem_request_t                   oshmem_request_empty;
oshmem_status_public_t             oshmem_status_empty;
oshmem_request_fns_t               oshmem_request_functions = {
    NULL, /*oshmem_request_default_test,*/
    NULL, /*oshmem_request_default_test_any,*/
    NULL, /*oshmem_request_default_test_all,*/
    NULL, /*oshmem_request_default_test_some,*/
    NULL, /*oshmem_request_default_wait,*/
    NULL, /*oshmem_request_default_wait_any,*/
    NULL, /*oshmem_request_default_wait_all,*/
    NULL, /*oshmem_request_default_wait_some*/
};

static void oshmem_request_construct(oshmem_request_t* req)
{
    /* don't call _INIT, we don't to set the request to _INACTIVE and there will
     * be no matching _FINI invocation */
    req->req_state        = OSHMEM_REQUEST_INVALID;
    req->req_complete     = false;
    req->req_persistent   = false;
    req->req_free = NULL;
    req->req_cancel = NULL;
    req->req_complete_cb = NULL;
    req->req_complete_cb_data = NULL;
    req->req_f_to_c_index = SHMEM_UNDEFINED;
    req->req_shmem_object.comm = (oshmem_group_t*) NULL; /* TODO: Implement*/
}

static void oshmem_request_destruct(oshmem_request_t* req)
{
    assert( SHMEM_UNDEFINED == req->req_f_to_c_index);
    assert( OSHMEM_REQUEST_INVALID == req->req_state);
}

static int oshmem_request_null_free(oshmem_request_t** request)
{
    return OSHMEM_SUCCESS;
}

static int oshmem_request_null_cancel(oshmem_request_t* request, int flag)
{
    return OSHMEM_SUCCESS;
}

static int oshmem_request_empty_free(oshmem_request_t** request)
{
    *request = &oshmem_request_null.request;
    return OSHMEM_SUCCESS;
}

int oshmem_request_persistent_proc_null_free(oshmem_request_t** request)
{
    OSHMEM_REQUEST_FINI(*request);
    (*request)->req_state = OSHMEM_REQUEST_INVALID;
    OBJ_RELEASE(*request);
    *request = &oshmem_request_null.request;
    return OSHMEM_SUCCESS;
}

/*TODO: define under class oshmem_free_list_item_t */
OBJ_CLASS_INSTANCE( oshmem_request_t,
                   ompi_free_list_item_t,
                   oshmem_request_construct,
                   oshmem_request_destruct);

int oshmem_request_init(void)
{
    OBJ_CONSTRUCT(&oshmem_request_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&oshmem_request_cond, opal_condition_t);

    OBJ_CONSTRUCT(&oshmem_request_null, oshmem_request_t);
    OBJ_CONSTRUCT(&oshmem_request_f_to_c_table, opal_pointer_array_t);
    if (OPAL_SUCCESS
            != opal_pointer_array_init(&oshmem_request_f_to_c_table,
                                       0,
                                       OMPI_FORTRAN_HANDLE_MAX,
                                       64)) {
        return OSHMEM_ERROR;
    }
    oshmem_request_null.request.req_type = OSHMEM_REQUEST_NULL;
    oshmem_request_null.request.req_status.SHMEM_SOURCE = SHMEM_PROC_NULL;
    oshmem_request_null.request.req_status.SHMEM_ERROR = SHMEM_SUCCESS;
    oshmem_request_null.request.req_status._count = 0;
    oshmem_request_null.request.req_status._cancelled = 0;

    oshmem_request_null.request.req_complete = true;
    oshmem_request_null.request.req_state = OSHMEM_REQUEST_INACTIVE;
    oshmem_request_null.request.req_persistent = false;
    oshmem_request_null.request.req_f_to_c_index =
            opal_pointer_array_add(&oshmem_request_f_to_c_table,
                                   &oshmem_request_null);
    oshmem_request_null.request.req_free = oshmem_request_null_free;
    oshmem_request_null.request.req_cancel = oshmem_request_null_cancel;
    oshmem_request_null.request.req_shmem_object.comm =
            (oshmem_group_t*) &ompi_mpi_comm_world.comm;

    if (0 != oshmem_request_null.request.req_f_to_c_index) {
        return OSHMEM_ERR_REQUEST;
    }

    /* We need a way to distinguish between the user provided
     * SHMEM_REQUEST_NULL to SHMEM_Wait* and a non-active (SHMEM_PROC_NULL)
     * request passed to any P2P non-blocking function.
     *
     * The main difference to oshmem_request_null is
     * req_state being OSHMEM_REQUEST_ACTIVE, so that SHMEM_Waitall
     * does not set the status to oshmem_status_empty and the different
     * req_free function, which resets the
     * request to SHMEM_REQUEST_NULL.
     * The req_cancel function need not be changed.
     */
    OBJ_CONSTRUCT(&oshmem_request_empty, oshmem_request_t);
    oshmem_request_empty.req_type = OSHMEM_REQUEST_NULL;
    oshmem_request_empty.req_status.SHMEM_SOURCE = SHMEM_PROC_NULL;
    oshmem_request_empty.req_status.SHMEM_ERROR = SHMEM_SUCCESS;
    oshmem_request_empty.req_status._count = 0;
    oshmem_request_empty.req_status._cancelled = 0;

    oshmem_request_empty.req_complete = true;
    oshmem_request_empty.req_state = OSHMEM_REQUEST_ACTIVE;
    oshmem_request_empty.req_persistent = false;
    oshmem_request_empty.req_f_to_c_index =
            opal_pointer_array_add(&oshmem_request_f_to_c_table,
                                   &oshmem_request_empty);
    oshmem_request_empty.req_free = oshmem_request_empty_free;
    oshmem_request_empty.req_cancel = oshmem_request_null_cancel;
    oshmem_request_empty.req_shmem_object.comm =
            (oshmem_group_t*) &ompi_mpi_comm_world.comm;

    if (1 != oshmem_request_empty.req_f_to_c_index) {
        return OSHMEM_ERR_REQUEST;
    }

    oshmem_status_empty.SHMEM_SOURCE = SHMEM_ANY_SOURCE;
    oshmem_status_empty.SHMEM_ERROR = SHMEM_SUCCESS;
    oshmem_status_empty._count = 0;
    oshmem_status_empty._cancelled = 0;

    return OSHMEM_SUCCESS;
}

int oshmem_request_finalize(void)
{
    OSHMEM_REQUEST_FINI( &oshmem_request_null.request);
    OBJ_DESTRUCT( &oshmem_request_null.request);
    OSHMEM_REQUEST_FINI( &oshmem_request_empty);
    OBJ_DESTRUCT( &oshmem_request_empty);
    OBJ_DESTRUCT( &oshmem_request_cond);
    OBJ_DESTRUCT( &oshmem_request_lock);
    OBJ_DESTRUCT( &oshmem_request_f_to_c_table);
    return OSHMEM_SUCCESS;
}

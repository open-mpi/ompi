/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/grequest.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"
#include "ompi/request/grequestx.h"

static bool requests_initialized = false;
static opal_list_t requests;
static int32_t active_requests = 0;
static bool in_progress = false;
static opal_mutex_t lock;

static int grequestx_progress(void) {
    ompi_grequest_t *request, *next;

    OPAL_THREAD_LOCK(&lock);
    if (!in_progress) {
        in_progress = true;

        OPAL_LIST_FOREACH_SAFE(request, next, &requests, ompi_grequest_t) {
            MPI_Status status;
            OPAL_THREAD_UNLOCK(&lock);
            request->greq_poll.c_poll(request->greq_state, &status);
            if (REQUEST_COMPLETE(&request->greq_base)) {
                OPAL_THREAD_LOCK(&lock);
                opal_list_remove_item(&requests, &request->greq_base.super.super);
                OPAL_THREAD_UNLOCK(&lock);
            }
            OPAL_THREAD_LOCK(&lock);
        }
    }
    OPAL_THREAD_UNLOCK(&lock);

    return OMPI_SUCCESS;
}

int ompi_grequestx_start(
    MPI_Grequest_query_function *gquery_fn,
    MPI_Grequest_free_function *gfree_fn,
    MPI_Grequest_cancel_function *gcancel_fn,
    ompi_grequestx_poll_function *gpoll_fn,
    void* extra_state,
    ompi_request_t** request)
{
    int rc;

    rc = ompi_grequest_start(gquery_fn, gfree_fn, gcancel_fn, extra_state, request);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
    ((ompi_grequest_t *)*request)->greq_poll.c_poll = gpoll_fn;

    if (!requests_initialized) {
        OBJ_CONSTRUCT(&requests, opal_list_t);
        OBJ_CONSTRUCT(&lock, opal_mutex_t);
        requests_initialized = true;
    }

    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&requests, &((*request)->super.super));
    OPAL_THREAD_UNLOCK(&lock);
    int32_t tmp = OPAL_THREAD_ADD_FETCH32(&active_requests, 1);
    if (1 == tmp) {
        opal_progress_register(grequestx_progress);
    }

    return OMPI_SUCCESS;
}


struct grequestx_class {
    opal_object_t super;
    MPI_Grequest_query_function *gquery_fn;
    MPI_Grequest_free_function *gfree_fn;
    MPI_Grequest_cancel_function *gcancel_fn;
    ompi_grequestx_poll_function *gpoll_fn;
    ompi_grequestx_wait_function *gwait_fn;
} ;

typedef struct grequestx_class grequestx_class;

static int next_class = 0;

static OBJ_CLASS_INSTANCE(grequestx_class, opal_object_t, NULL, NULL);

static opal_pointer_array_t classes;

int ompi_grequestx_class_create(
    MPI_Grequest_query_function *gquery_fn,
    MPI_Grequest_free_function *gfree_fn,
    MPI_Grequest_cancel_function *gcancel_fn,
    ompi_grequestx_poll_function *gpoll_fn,
    ompi_grequestx_wait_function *gwait_fn,
    ompi_grequestx_class *greq_class)
{
    grequestx_class * class = OBJ_NEW(grequestx_class);
    class->gquery_fn = gquery_fn;
    class->gfree_fn = gfree_fn;
    class->gcancel_fn = gcancel_fn;
    class->gpoll_fn = gpoll_fn;
    class->gwait_fn = gwait_fn;

    if (0 == next_class) {
        OBJ_CONSTRUCT(&classes, opal_pointer_array_t);
    }
    opal_pointer_array_add(&classes, class);
    next_class ++;

    return OMPI_SUCCESS;
}

int ompi_grequestx_class_allocate(
    ompi_grequestx_class greq_class,
    void *extra_state,
    ompi_request_t **request)
{
    grequestx_class *class = opal_pointer_array_get_item(&classes, greq_class);
    ompi_grequestx_start(class->gquery_fn, class->gfree_fn, class->gcancel_fn, class->gpoll_fn, extra_state, request);

    return OMPI_SUCCESS;
}

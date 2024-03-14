/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2008-2019 University of Houston. All rights reserved.
 * Copyright (c) 2022-2023 Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/class/opal_list.h"
#include "common_ompio_request.h"
#include "common_ompio_buffer.h"

static void mca_common_ompio_request_construct(mca_ompio_request_t* req);
static void mca_common_ompio_request_destruct(mca_ompio_request_t *req);

bool mca_common_ompio_progress_is_registered=false;
/*
 * Global list of requests for this component
 */
opal_list_t mca_common_ompio_pending_requests = {{0}};

static opal_mutex_t mca_common_ompio_mutex = OPAL_MUTEX_STATIC_INIT;

static int mca_common_ompio_request_free ( struct ompi_request_t **req)
{
    mca_ompio_request_t *ompio_req = ( mca_ompio_request_t *)*req;
    if ( NULL != ompio_req->req_tbuf ) {
        if ( MCA_OMPIO_REQUEST_READ == ompio_req->req_type ){
            struct iovec decoded_iov;
            uint32_t iov_count=1;
            size_t pos=0;

            decoded_iov.iov_base = ompio_req->req_tbuf;
            decoded_iov.iov_len  = ompio_req->req_size;
            opal_convertor_unpack (&ompio_req->req_convertor, &decoded_iov, &iov_count, &pos );
        }
        mca_common_ompio_release_buf ( NULL, ompio_req->req_tbuf );
    }
    if ( NULL != ompio_req->req_free_fn ) {
        ompio_req->req_free_fn (ompio_req );
    }
    opal_list_remove_item (&mca_common_ompio_pending_requests, &ompio_req->req_item);

    OBJ_RELEASE (*req);
    *req = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_common_ompio_request_cancel ( struct ompi_request_t *req, int flag)
{
    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_ompio_request_t, ompi_request_t,
                   mca_common_ompio_request_construct,
                   mca_common_ompio_request_destruct);

void mca_common_ompio_request_construct(mca_ompio_request_t* req)
{
    OMPI_REQUEST_INIT (&(req->req_ompi), false );
    req->req_ompi.req_free     = mca_common_ompio_request_free;
    req->req_ompi.req_cancel   = mca_common_ompio_request_cancel;
    req->req_ompi.req_type     = OMPI_REQUEST_IO;
    req->req_data              = NULL;
    req->req_tbuf              = NULL;
    req->req_size              = 0;
    req->req_max_data          = 0;
    req->req_progress_fn       = NULL;
    req->req_free_fn           = NULL;
    req->req_parent            = NULL;
    req->req_post_next_subreq  = NULL;
    req->req_num_subreqs       = 0;
    req->req_subreqs_completed = 0;
    req->req_fh                = NULL;
    req->req_fview             = NULL;
    req->req_post_followup     = false;

    OBJ_CONSTRUCT(&req->req_item, opal_list_item_t);
    opal_list_append (&mca_common_ompio_pending_requests, &req->req_item);
    return;
}

void mca_common_ompio_request_destruct(mca_ompio_request_t* req)
{
    OMPI_REQUEST_FINI ( &(req->req_ompi));
    OBJ_DESTRUCT (&req->req_item);
    if ( NULL != req->req_data ) {
        free (req->req_data);
    }
    if (NULL != req->req_fview) {
	free (req->req_fview->f_decoded_iov);
	free (req->req_fview);
    }
    return;
}

void mca_common_ompio_request_init ( void ) 
{
    /* Create the list of pending requests */
    OBJ_CONSTRUCT(&mca_common_ompio_pending_requests, opal_list_t);
    return;
}

void mca_common_ompio_request_fini ( void ) 
{
    /* Destroy the list of pending requests */
    /* JMS: Good opportunity here to list out all the IO requests that
       were not destroyed / completed upon MPI_FINALIZE */

    OBJ_DESTRUCT(&mca_common_ompio_pending_requests);
    if (mca_common_ompio_progress_is_registered) {
        OPAL_THREAD_LOCK (&mca_common_ompio_mutex);
        if (mca_common_ompio_progress_is_registered) {
            opal_progress_unregister(mca_common_ompio_progress);
            mca_common_ompio_progress_is_registered=false;
        }
        OPAL_THREAD_UNLOCK (&mca_common_ompio_mutex);
    }

    return;
}

void mca_common_ompio_request_alloc ( mca_ompio_request_t **req, mca_ompio_request_type_t type )
{
    mca_ompio_request_t *ompio_req = NULL;

    ompio_req = OBJ_NEW(mca_ompio_request_t);
    ompio_req->req_type = type;
    ompio_req->req_ompi.req_state = OMPI_REQUEST_ACTIVE;

    *req=ompio_req;

    return;
}

void mca_common_ompio_register_progress ( void ) 
{
    if (false == mca_common_ompio_progress_is_registered) {
        OPAL_THREAD_LOCK (&mca_common_ompio_mutex);
        if (mca_common_ompio_progress_is_registered) {
            OPAL_THREAD_UNLOCK (&mca_common_ompio_mutex);
            return;
        }
        opal_progress_register (mca_common_ompio_progress);
        mca_common_ompio_progress_is_registered=true;
        OPAL_THREAD_UNLOCK (&mca_common_ompio_mutex);
    }
    return;
}

int mca_common_ompio_progress ( void )
{
    mca_ompio_request_t *req=NULL;
    opal_list_item_t *litem=NULL;
    opal_list_item_t *lnext=NULL;
    int completed=0;

    /* The mca_common_ompio_mutex is used to
    ** avoid multiple progress threads potentially interfering
    ** with each other
    */
    if (!OPAL_THREAD_TRYLOCK(&mca_common_ompio_mutex)) {
        OPAL_LIST_FOREACH(litem, &mca_common_ompio_pending_requests, opal_list_item_t) {
            req = GET_OMPIO_REQ_FROM_ITEM(litem);
            if (REQUEST_COMPLETE(&req->req_ompi) ) {
                continue;
            }
            if (NULL != req->req_progress_fn) {
                if (req->req_progress_fn(req)) {
                    /**
                     * To support pipelined read/write operations, a user level request
                     * can contain multiple internal requests. These sub-requests
                     * contain a pointer to the parent request.
                     */
                    mca_ompio_request_t *parent = req->req_parent;
                    if (NULL != parent) {
                        /* This is a subrequest */
                        if (OMPI_SUCCESS != req->req_ompi.req_status.MPI_ERROR) {
                            parent->req_ompi.req_status.MPI_ERROR = req->req_ompi.req_status.MPI_ERROR;
                            ompi_request_complete (&parent->req_ompi, true);
                            continue;
                        }
                        parent->req_subreqs_completed++;
                        parent->req_ompi.req_status._ucount += req->req_ompi.req_status._ucount;
                        req->req_post_followup = true;
                    } else {
                        /* This is a request without subrequests */
                        completed++;
                        ompi_request_complete (&req->req_ompi, true);
                    }
                    /* The fbtl progress function is expected to set the
                     * status elements
                     */
                }
            } else {
                /* This is a request without a lower level progress function, .e.g
                 * a parent request
                 */
                if (req->req_num_subreqs == req->req_subreqs_completed) {
                    completed++;
                    ompi_request_complete (&req->req_ompi, true);
                }
            }
        }

        /**
         * Splitting the ompio progress loop is necessary to avoid that a pending operation
         * consisting of multiple subrequests is executed in a single invokation of the progress
         * function.
         *
         * Otherwise it can happen that the next subrequest is posted, which ends up at the tail
         * of the ompio_pending_requests_list, and would be processed in the same loop execution;
         * which then posts the next subrequest, which is also processed potentially right away
         * etc. This would make the ompio_progress function block for a long time, and prevent
         * overlapping operations.
         *
         * Splitting the loop into two parts, one checking for completion and one posting
         * the next subrequest if necessary avoids the problem.
         */
        OPAL_LIST_FOREACH_SAFE(litem, lnext, &mca_common_ompio_pending_requests, opal_list_item_t) {
            req = GET_OMPIO_REQ_FROM_ITEM(litem);
            if (true == req->req_post_followup) {
                /* This lock is used to serialize access to the
                ** file across multiple threads.
                */
                if (OPAL_THREAD_TRYLOCK(&req->req_fh->f_fh->f_lock)) {
                    continue;
                }
                mca_ompio_request_t *parent = req->req_parent;
                parent->req_post_next_subreq(parent, parent->req_subreqs_completed);
                OPAL_THREAD_UNLOCK(&req->req_fh->f_fh->f_lock);
                ompi_request_complete (&req->req_ompi, false);
                ompi_request_free ((ompi_request_t**)&req);
            }
        }
        OPAL_THREAD_UNLOCK(&mca_common_ompio_mutex);
    }

    return completed;
}

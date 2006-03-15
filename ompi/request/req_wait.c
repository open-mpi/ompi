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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/request/request.h"


int ompi_request_wait(
    ompi_request_t ** req_ptr,
    ompi_status_public_t * status)
{
    ompi_request_t *req = *req_ptr;

    if(req->req_complete == false) {

#if OMPI_ENABLE_PROGRESS_THREADS
        /* poll for completion */
        if(opal_progress_spin(&req->req_complete))
            goto finished;
#endif

        /* give up and sleep until completion */
        OPAL_THREAD_LOCK(&ompi_request_lock);
        ompi_request_waiting++;
        while (req->req_complete == false) {
            opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
        }
        ompi_request_waiting--;
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

#if OMPI_ENABLE_PROGRESS_THREADS
finished:
#endif

    /* return status */
    if( MPI_STATUS_IGNORE != status ) {
        *status = req->req_status;
    }
    if( req->req_state == OMPI_REQUEST_INACTIVE ) {
        return OMPI_SUCCESS;
    }
    if( req->req_persistent ) {
        req->req_state = OMPI_REQUEST_INACTIVE;
        return OMPI_SUCCESS;
    }

    /* return request to pool */
    return ompi_request_free(req_ptr);
}


int ompi_request_wait_any(
    size_t count,
    ompi_request_t ** requests,
    int *index,
    ompi_status_public_t * status)
{
#if OMPI_ENABLE_PROGRESS_THREADS
    int c;
#endif
    size_t i=0, num_requests_null_inactive=0;
    int rc = OMPI_SUCCESS;
    int completed = -1;
    ompi_request_t **rptr=NULL;
    ompi_request_t *request=NULL;

#if OMPI_ENABLE_PROGRESS_THREADS
    /* poll for completion */
    OPAL_THREAD_ADD32(&opal_progress_thread_count,1);
    for (c = 0; completed < 0 && c < opal_progress_spin_count; c++) {
        rptr = requests;
        num_requests_null_inactive = 0;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            /*
             * Check for null or completed persistent request.
             * For MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE
             */
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                num_requests_null_inactive++;
                continue;
            }
            if (true == request->req_complete) {
                completed = i;
                OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
                goto finished;
            }
        }
        if( num_requests_null_inactive == count ) {
            OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
            goto finished;
        }
        opal_progress();
    }
    OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
#endif

    /* give up and sleep until completion */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_waiting++;
    do {
        rptr = requests;
        num_requests_null_inactive = 0;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            /*
             * Check for null or completed persistent request.
             * For MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE.
             */
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                num_requests_null_inactive++;
                continue;
            }
            if (request->req_complete == true) {
                completed = i;
                break;
            }
        }
        if(num_requests_null_inactive == count)
            break;
        if (completed < 0) {
            opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
        }
    } while (completed < 0);
    ompi_request_waiting--;
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

#if OMPI_ENABLE_PROGRESS_THREADS
finished:
#endif  /* OMPI_ENABLE_PROGRESS_THREADS */

    if(num_requests_null_inactive == count) {
        *index = MPI_UNDEFINED;
        if (MPI_STATUS_IGNORE != status) {
            *status = ompi_status_empty;
        }
    } else {
        assert( true == request->req_complete );
        /* return status */
        if (MPI_STATUS_IGNORE != status) {
            *status = request->req_status;
        }
        if( request->req_persistent ) {
            request->req_state = OMPI_REQUEST_INACTIVE;
        } else {
            /* return request to pool */
            rc = ompi_request_free(rptr);
        }
        *index = completed;
    }
    return rc;
}


int ompi_request_wait_all(
    size_t count,
    ompi_request_t ** requests,
    ompi_status_public_t * statuses)
{
    size_t completed = 0, i;
    ompi_request_t **rptr;
    ompi_request_t *request;
    int mpi_error = OMPI_SUCCESS;

    rptr = requests;
    for (i = 0; i < count; i++) {
        request = *rptr++;
        if (request->req_complete == true) {
            completed++;
        }
    }

    /* if all requests have not completed -- defer acquiring lock
     * unless required
     */
    if (completed != count) {

        /*
         * acquire lock and test for completion - if all requests are
         * not completed pend on condition variable until a request
         * completes
         */
        OPAL_THREAD_LOCK(&ompi_request_lock);
        ompi_request_waiting++;
#if OMPI_HAVE_THREAD_SUPPORT
        /*
         * confirm the status of the pending requests. We have to do it before
         * taking the condition or otherwise we can miss some requests completion (the
         * one that happpens between our initial test and the aquisition of the lock).
         */
        rptr = requests;
        for( completed = i = 0; i < count; i++ ) {
            request = *rptr++;
            if (request->req_complete == true) {
                completed++;
            }
        }
#endif  /* OMPI_HAVE_THREAD_SUPPORT */
        while( completed != count ) {
            /* check number of pending requests */
            size_t start = ompi_request_completed;
            size_t pending = count - completed;
            /*
             * wait until at least pending requests complete
             */
            while (pending > ompi_request_completed - start) {
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            }
            /*
             * confirm that all pending operations have completed.
             */
            rptr = requests;
            for( completed = i = 0; i < count; i++ ) {
                request = *rptr++;
                if (request->req_complete == true) {
                    completed++;
                }
            }
        }
        ompi_request_waiting--;
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

    rptr = requests;
    if (MPI_STATUSES_IGNORE != statuses) {
        /* fill out status and free request if required */
        for( i = 0; i < count; i++, rptr++ ) {
            request = *rptr;
            assert( true == request->req_complete );
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                statuses[i] = ompi_status_empty;
            } else {
                statuses[i] = request->req_status;
            }
            if( request->req_persistent ) {
                request->req_state = OMPI_REQUEST_INACTIVE;
            } else {
	        (void)ompi_request_free(rptr);
            }
            if( statuses[i].MPI_ERROR != OMPI_SUCCESS) {
                mpi_error = OMPI_ERR_IN_ERRNO;
            }
        }
    } else {
        /* free request if required */
        for( i = 0; i < count; i++, rptr++ ) {
            int rc;
            request = *rptr;

            assert( true == request->req_complete );
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                rc = ompi_status_empty.MPI_ERROR;
            } else {
	        rc = request->req_status.MPI_ERROR;
            }
            if( request->req_persistent ) {
                request->req_state = OMPI_REQUEST_INACTIVE;
            } else {
	        (void)ompi_request_free(rptr);
            }
            if( rc != OMPI_SUCCESS) {
	        mpi_error = rc;
            }
	}
    }
    return mpi_error;
}


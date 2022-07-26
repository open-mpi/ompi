/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/request/request.h"
#include "ompi/request/request_default.h"
#include "ompi/request/grequest.h"

int ompi_request_default_wait(
    ompi_request_t ** req_ptr,
    ompi_status_public_t * status)
{
    ompi_request_t *req = *req_ptr;

    ompi_request_wait_completion(req);

#if OPAL_ENABLE_FT_MPI
    /* Special case for MPI_ANY_SOURCE */
    if( MPI_ERR_PROC_FAILED_PENDING == req->req_status.MPI_ERROR ) {
        if( MPI_STATUS_IGNORE != status ) {
            OMPI_COPY_STATUS(status, req->req_status, false);
        }
        return MPI_ERR_PROC_FAILED_PENDING;
    }
#endif /* OPAL_ENABLE_FT_MPI */

    /* return status.  If it's a generalized request, we *have* to
       invoke the query_fn, even if the user provided STATUS_IGNORE.
       MPI-2:8.2. */
    if (OMPI_REQUEST_GEN == req->req_type) {
        ompi_grequest_invoke_query(req, &req->req_status);
    }
    if( MPI_STATUS_IGNORE != status ) {
        OMPI_COPY_STATUS(status, req->req_status, false);
    }
    if( req->req_persistent ) {
        if( req->req_state == OMPI_REQUEST_INACTIVE ) {
            if (MPI_STATUS_IGNORE != status) {
                OMPI_COPY_STATUS(status, ompi_status_empty, false);
            }
            return OMPI_SUCCESS;
        }
        req->req_state = OMPI_REQUEST_INACTIVE;
        return req->req_status.MPI_ERROR;
    }

    /* If there was an error, don't free the request -- just return
       the single error. */
    if (MPI_SUCCESS != req->req_status.MPI_ERROR) {
        return req->req_status.MPI_ERROR;
    }

    /* If there's an error while freeing the request, assume that the
       request is still there.  Otherwise, Bad Things will happen
       later! */
    return ompi_request_free(req_ptr);
}


int ompi_request_default_wait_any(size_t count,
                                  ompi_request_t ** requests,
                                  int *index,
                                  ompi_status_public_t * status)
{
    size_t i, completed = count, num_requests_null_inactive = 0;
    int rc = OMPI_SUCCESS;
    ompi_request_t *request=NULL;
    ompi_wait_sync_t sync;

    if (OPAL_UNLIKELY(0 == count)) {
        *index = MPI_UNDEFINED;
        return OMPI_SUCCESS;
    }

recheck:
    WAIT_SYNC_INIT(&sync, 1);

    num_requests_null_inactive = 0;
    for (i = 0; i < count; i++) {
        void *_tmp_ptr = REQUEST_PENDING;

        request = requests[i];

        /* Check for null or completed persistent request. For
         * MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE.
         */
        if( request->req_state == OMPI_REQUEST_INACTIVE ) {
            num_requests_null_inactive++;
            continue;
        }

        if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, &sync) ) {
            if(OPAL_LIKELY( REQUEST_COMPLETE(request) )) {
                completed = i;
                *index = i;
                goto after_sync_wait;
            }
        }

#if OPAL_ENABLE_FT_MPI
        if(OPAL_UNLIKELY( ompi_request_is_failed(request) )) {
            completed = i;
            *index = i;
            goto after_sync_wait;
        }
#endif /* OPAL_ENABLE_FT_MPI */
    }

    if(num_requests_null_inactive == count) {
        *index = MPI_UNDEFINED;
        if (MPI_STATUS_IGNORE != status) {
            OMPI_COPY_STATUS(status, ompi_status_empty, false);
        }
        /* No signal-in-flight can be in this case */
        WAIT_SYNC_RELEASE_NOWAIT(&sync);
        return rc;
    }

    rc = SYNC_WAIT(&sync);

  after_sync_wait:
    /* recheck the complete status and clean up the sync primitives.
     * Do it backward to return the earliest complete request to the
     * user.
     */
    for(i = completed-1; (i+1) > 0; i--) {
        void *tmp_ptr = &sync;

        request = requests[i];

        if( request->req_state == OMPI_REQUEST_INACTIVE ) {
            continue;
        }
        /* Atomically mark the request as pending. If this succeed then
         * the request was not completed, and it is now marked as pending.
         * Otherwise, the request has been completed meanwhile, and it
         * has been atomically marked as REQUEST_COMPLETE.
         */
        if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &tmp_ptr, REQUEST_PENDING) ) {
            *index = i;
        }
    }

    /* Error path: SYNC_WAIT was interrupted by an error
     * We do this after the cleanup loop to make sure nobody is updating the
     * sync again while we are rearming it */
    if(OPAL_UNLIKELY( OMPI_SUCCESS != rc )) {
        rc = OMPI_SUCCESS;
        WAIT_SYNC_RELEASE(&sync);
        goto recheck;
    }

    if( *index == (int)completed ) {
        /* Only one request has triggered. There was no in-flight
         * completions. Drop the signalled flag so we won't block
         * in WAIT_SYNC_RELEASE 
         */
        WAIT_SYNC_SIGNALLED(&sync);
    }

    request = requests[*index];
#if OPAL_ENABLE_FT_MPI
    /* Special case for MPI_ANY_SOURCE */
    if( MPI_ERR_PROC_FAILED == request->req_status.MPI_ERROR ) {
        WAIT_SYNC_RELEASE(&sync);
        return MPI_ERR_PROC_FAILED_PENDING;
    }
#endif  /* OPAL_ENABLE_FT_MPI */
    assert( REQUEST_COMPLETE(request) );
    /* Per note above, we have to call gen request query_fn even
       if STATUS_IGNORE was provided */
    if (OMPI_REQUEST_GEN == request->req_type) {
        rc = ompi_grequest_invoke_query(request, &request->req_status);
    }
    if (MPI_STATUS_IGNORE != status) {
        OMPI_COPY_STATUS(status, request->req_status, false);
    }
    rc = request->req_status.MPI_ERROR;
    if( request->req_persistent ) {
        request->req_state = OMPI_REQUEST_INACTIVE;
    } else if (MPI_SUCCESS == rc) {
        /* Only free the request if there is no error on it */
        /* If there's an error while freeing the request,
           assume that the request is still there.  Otherwise,
           Bad Things will happen later! */
        rc = ompi_request_free(&requests[*index]);
    }

    WAIT_SYNC_RELEASE(&sync);
    return rc;
}


int ompi_request_default_wait_all( size_t count,
                                   ompi_request_t ** requests,
                                   ompi_status_public_t * statuses )
{
    size_t i, completed = 0, failed = 0;
    ompi_request_t **rptr;
    ompi_request_t *request;
    int mpi_error = OMPI_SUCCESS;
    ompi_wait_sync_t sync;

    if (OPAL_UNLIKELY(0 == count)) {
        return OMPI_SUCCESS;
    }

recheck:
    WAIT_SYNC_INIT(&sync, count);
    rptr = requests;
    for (i = 0; i < count; i++) {
        void *_tmp_ptr = REQUEST_PENDING;

        request = *rptr++;

        if( request->req_state == OMPI_REQUEST_INACTIVE ) {
            completed++;
            continue;
        }

        if (REQUEST_COMPLETE(request) || !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, &sync)) {
            if( OPAL_LIKELY( REQUEST_COMPLETE(request) ) ) {
                if( OPAL_UNLIKELY( MPI_SUCCESS != request->req_status.MPI_ERROR ) ) {
                    failed++;
                }
                completed++;
            }
        }

#if OPAL_ENABLE_FT_MPI
        if(OPAL_UNLIKELY( ompi_request_is_failed(request) )) {
            failed++;
            continue;
        }
#endif /* OPAL_ENABLE_FT_MPI */
    }
    if( failed > 0 ) {
        /* We are completing only one here, lets prevent blocking in the
         * SYNC_RELEASE by marking the sync as SIGNALED */
        WAIT_SYNC_SIGNALLED(&sync);
        goto finish;
    }

    if( 0 != completed ) {
        wait_sync_update(&sync, completed, OPAL_SUCCESS);
    }

    /* wait until all requests complete or until an error is triggered. */
    mpi_error = SYNC_WAIT(&sync);
    if( OPAL_SUCCESS != mpi_error ) {
        /* The sync triggered because of an error. The error may be for us, but
         * it may be for some other pending wait, so we have to recheck
         * our request status.
         *
         * We are going to rearm the sync, but first make sure it is not
         * updated by any progress thread meanwhile by removing it from all
         * requests it has been attached to.
         */
        rptr = requests;
        for (i = 0; i < count; i++) {
            void *_tmp_ptr = &sync;

            request = *rptr++;

            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                continue;
            }

            OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, REQUEST_PENDING);
        }
        /* The sync is now ready for rearming */
        WAIT_SYNC_RELEASE(&sync);
        failed = completed = 0;
        goto recheck;
    }

 finish:
    rptr = requests;
    if (MPI_STATUSES_IGNORE != statuses) {
        /* fill out status and free request if required */
        for( i = 0; i < count; i++, rptr++ ) {
            void *_tmp_ptr = &sync;

            request = *rptr;

            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                OMPI_COPY_STATUS(&statuses[i], ompi_status_empty, true);
                continue;
            }

            if( OPAL_UNLIKELY(0 < failed) ) {
                /* if we have failed requests we skipped the waiting on the sync. Thus,
                 * some of the requests might not be properly completed, in which case
                 * we must detach all requests from the sync. However, if we can successfully
                 * mark the request as pending then it is neither failed nor complete, and
                 * we must stop altering it.
                 */
                if( OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, REQUEST_PENDING ) ) {
                    /*
                     * Per MPI 2.2 p 60:
                     * Allows requests to be marked as MPI_ERR_PENDING if they are
                     * "neither failed nor completed." Which can only happen if
                     * there was an error in one of the other requests.
                     */
                    statuses[i].MPI_ERROR = MPI_ERR_PENDING;
#if OPAL_ENABLE_FT_MPI
                    /* PROC_FAILED_PENDING errors are also not completed yet */
                    if( MPI_ERR_PROC_FAILED_PENDING == requests[i]->req_status.MPI_ERROR ) {
                        statuses[i].MPI_ERROR = MPI_ERR_PROC_FAILED_PENDING;
                    }
#endif /* OPAL_ENABLE_FT_MPI */
                    mpi_error = MPI_ERR_IN_STATUS;
                    continue;
                }
            }
            assert( REQUEST_COMPLETE(request) );

            if (OMPI_REQUEST_GEN == request->req_type) {
                ompi_grequest_invoke_query(request, &request->req_status);
            }

            OMPI_COPY_STATUS(&statuses[i], request->req_status, true);

            if( request->req_persistent ) {
                request->req_state = OMPI_REQUEST_INACTIVE;
                continue;
            }
            /* Only free the request if there is no error on it */
            if (MPI_SUCCESS == request->req_status.MPI_ERROR) {
                /* If there's an error while freeing the request,
                   assume that the request is still there.
                   Otherwise, Bad Things will happen later! */
                int tmp = ompi_request_free(rptr);
                if (OMPI_SUCCESS == mpi_error && OMPI_SUCCESS != tmp) {
                    mpi_error = tmp;
                }
            }
            if( statuses[i].MPI_ERROR != OMPI_SUCCESS) {
                mpi_error = MPI_ERR_IN_STATUS;
            }
        }
    } else {
        int rc;
        /* free request if required */
        for( i = 0; i < count; i++, rptr++ ) {
            void *_tmp_ptr = &sync;

            request = *rptr;

            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                rc = ompi_status_empty.MPI_ERROR;
                goto absorb_error_and_continue;
            }
            /*
             * Assert only if no requests were failed.
             * Since some may still be pending.
             */
            if( OPAL_UNLIKELY(0 < failed) ) {
                /* If the request is still pending due to a failed request
                 * then skip it in this loop.
                 */
                 if( OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, REQUEST_PENDING ) ) {
                    /*
                     * Per MPI 2.2 p 60:
                     * Allows requests to be marked as MPI_ERR_PENDING if they are
                     * "neither failed nor completed." Which can only happen if
                     * there was an error in one of the other requests.
                     */
                    rc = MPI_ERR_PENDING;
#if OPAL_ENABLE_FT_MPI
                    /* PROC_FAILED_PENDING errors are also not completed yet */
                    if( MPI_ERR_PROC_FAILED_PENDING == requests[i]->req_status.MPI_ERROR ) {
                        rc = MPI_ERR_PROC_FAILED_PENDING;
                    }
#endif  /* OPAL_ENABLE_FT_MPI */
                    goto absorb_error_and_continue;
                 }
            }
            assert( REQUEST_COMPLETE(request) );

            /* Per note above, we have to call gen request query_fn
               even if STATUSES_IGNORE was provided */
            if (OMPI_REQUEST_GEN == request->req_type) {
                rc = ompi_grequest_invoke_query(request, &request->req_status);
            }

            rc = request->req_status.MPI_ERROR;

            if( request->req_persistent ) {
                request->req_state = OMPI_REQUEST_INACTIVE;
            } else if (MPI_SUCCESS == rc) {
                /* Only free the request if there is no error on it */
                int tmp = ompi_request_free(rptr);
                if (OMPI_SUCCESS == mpi_error && OMPI_SUCCESS != tmp) {
                    mpi_error = tmp;
                }
            }
    absorb_error_and_continue:
#if OPAL_ENABLE_FT_MPI
            if( (MPI_ERR_PROC_FAILED == rc) || (MPI_ERR_REVOKED == rc) ) {
                mpi_error = rc;
            }
#endif  /* OPAL_ENABLE_FT_MPI */
            /*
             * Per MPI 2.2 p34:
             * "It is possible for an MPI function to return MPI_ERR_IN_STATUS
             *  even when MPI_STATUS_IGNORE or MPI_STATUSES_IGNORE has been
             *  passed to that function."
             * So we should do so here as well.
             */
            if( OMPI_SUCCESS == mpi_error && rc != OMPI_SUCCESS) {
                mpi_error = MPI_ERR_IN_STATUS;
            }
        }
    }
    WAIT_SYNC_RELEASE(&sync);
    return mpi_error;
}


int ompi_request_default_wait_some(size_t count,
                                   ompi_request_t ** requests,
                                   int * outcount,
                                   int * indices,
                                   ompi_status_public_t * statuses)
{
    size_t num_requests_null_inactive, num_requests_done, num_active_reqs;
    int rc = MPI_SUCCESS;
    ompi_request_t **rptr = NULL;
    ompi_request_t *request = NULL;
    ompi_wait_sync_t sync;
    size_t sync_sets = 0, sync_unsets = 0;

    if (OPAL_UNLIKELY(0 == count)) {
        *outcount = MPI_UNDEFINED;
        return OMPI_SUCCESS;
    }

  recheck:
    WAIT_SYNC_INIT(&sync, 1);

    *outcount = 0;

    rptr = requests;
    num_requests_null_inactive = 0;
    num_requests_done = 0;
    num_active_reqs = 0;
    for (size_t i = 0; i < count; i++, rptr++) {
        void *_tmp_ptr = REQUEST_PENDING;

        request = *rptr;
        /*
         * Check for null or completed persistent request.
         * For MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE.
         */
        if( request->req_state == OMPI_REQUEST_INACTIVE ) {
            num_requests_null_inactive++;
            continue;
        }
        indices[num_active_reqs] = OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, &sync);
        if( !indices[num_active_reqs] ) {
            /* If the request is completed go ahead and mark it as such */
            if( REQUEST_COMPLETE(request) ) {
                num_requests_done++;
            }
        }

#if OPAL_ENABLE_FT_MPI
        if(OPAL_UNLIKELY( ompi_request_is_failed(request) )) {
            num_requests_done++;
            continue;
        }
#endif /* OPAL_ENABLE_FT_MPI */
        num_active_reqs++;
    }

    if(num_requests_null_inactive == count) {
        *outcount = MPI_UNDEFINED;
        /* nobody will signal us */
        WAIT_SYNC_RELEASE_NOWAIT(&sync);
        return rc;
    }

    sync_sets = num_active_reqs - num_requests_done;
    if( 0 == num_requests_done ) {
        /* One completed request is enough to satisfy the some condition */
        SYNC_WAIT(&sync);
    }

    /* Do the final counting and */
    /* Clean up the synchronization primitives */

    rptr = requests;
    num_requests_done = 0;
    num_active_reqs = 0;
    for (size_t i = 0; i < count; i++, rptr++) {
        void *_tmp_ptr = &sync;

        request = *rptr;

        if( request->req_state == OMPI_REQUEST_INACTIVE ) {
            continue;
        }
        /* Here we have 3 possibilities:
         * a) request was found completed in the first loop
         *    => ( indices[i] == 0 )
         * b) request was completed between first loop and this check
         *    => ( indices[i] == 1 ) and we can NOT atomically mark the 
         *    request as pending.
         * c) request wasn't finished yet
         *    => ( indices[i] == 1 ) and we CAN  atomically mark the 
         *    request as pending.
         * NOTE that in any case (i >= num_requests_done) as latter grows
         * either slowly (in case of partial completion)
         * OR in parallel with `i` (in case of full set completion)  
         */
        if( !indices[num_active_reqs] ) {
            indices[num_requests_done++] = i;
        } else if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, REQUEST_PENDING) ) {
            indices[num_requests_done++] = i;
        }
#if OPAL_ENABLE_FT_MPI
        /* Special case for MPI_ANY_SOURCE - Error managed below */
        else if(OPAL_UNLIKELY( ompi_request_is_failed(request) &&
                               MPI_ERR_PROC_FAILED_PENDING == request->req_status.MPI_ERROR )) {
            indices[num_requests_done++] = i;
        }
#endif /* OPAL_ENABLE_FT_MPI */
        num_active_reqs++;
    }
    sync_unsets = num_active_reqs - num_requests_done;

    if( sync_sets == sync_unsets ){
        /* nobody knows about us,
         * set signa-in-progress flag to false
         */
        WAIT_SYNC_SIGNALLED(&sync);
    }

    WAIT_SYNC_RELEASE(&sync);

    /* error path: no requests are done because the sync got triggered
     * We have nothing more to do here besides rearming the sync and trying
     * again */
    if(OPAL_UNLIKELY( 0 == num_requests_done )) {
        assert(OMPI_SUCCESS != sync.status);
        goto recheck;
    }

    *outcount = num_requests_done;

    for (size_t i = 0; i < num_requests_done; i++) {
        request = requests[indices[i]];
#if OPAL_ENABLE_FT_MPI
        /* Special case for MPI_ANY_SOURCE */
        if( MPI_ERR_PROC_FAILED_PENDING == request->req_status.MPI_ERROR ) {
            rc = MPI_ERR_IN_STATUS;
            if (MPI_STATUSES_IGNORE != statuses) {
                OMPI_COPY_STATUS(&statuses[i], request->req_status, true);
                statuses[i].MPI_ERROR = MPI_ERR_PROC_FAILED_PENDING;
            } else {
                if( (MPI_ERR_PROC_FAILED == request->req_status.MPI_ERROR) ||
                    (MPI_ERR_REVOKED == request->req_status.MPI_ERROR) ) {
                    rc = request->req_status.MPI_ERROR;
                }
            }
            continue;
        }
#endif /* OPAL_ENABLE_FT_MPI */
        assert( REQUEST_COMPLETE(request) );

        /* Per note above, we have to call gen request query_fn even
           if STATUS_IGNORE was provided */
        if (OMPI_REQUEST_GEN == request->req_type) {
            ompi_grequest_invoke_query(request, &request->req_status);
        }
        if (MPI_STATUSES_IGNORE != statuses) {
            OMPI_COPY_STATUS(&statuses[i], request->req_status, true);
        }

        if (MPI_SUCCESS != request->req_status.MPI_ERROR) {
            rc = MPI_ERR_IN_STATUS;
        }

        if( request->req_persistent ) {
            request->req_state = OMPI_REQUEST_INACTIVE;
        } else {
            /* Only free the request if there was no error */
            if (MPI_SUCCESS == request->req_status.MPI_ERROR) {
                int tmp;
                tmp = ompi_request_free(&(requests[indices[i]]));
                if (OMPI_SUCCESS != tmp) {
                    return tmp;
                }
            }
        }
    }

    return rc;
}

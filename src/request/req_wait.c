/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "include/constants.h"
#include "request/request.h"


int ompi_request_wait_any(
    size_t count,
    ompi_request_t ** requests,
    int *index, 
    ompi_status_public_t * status)
{
#if OMPI_HAVE_THREADS
    int c;
#endif
    size_t i, null_requests;
    int rc;
    int completed = -1;
    ompi_request_t **rptr;
    ompi_request_t *request;

#if OMPI_HAVE_THREADS
    /* poll for completion */
    ompi_atomic_mb();
    for (c = 0; completed < 0 && c < ompi_request_poll_iterations; c++) {
        rptr = requests;
        null_requests = 0;
        for (i = 0; i < count; i++) {
            request = *rptr;
            if (MPI_REQUEST_NULL == request) {
                if(++null_requests == count)
                    goto finished;
                continue;
            }
            if (true == request->req_complete) {
                completed = i;
                goto finished;
            } 
            rptr++;
        }
    }
#endif

    /* give up and sleep until completion */
    OMPI_THREAD_LOCK(&ompi_request_lock);
    do {
        ompi_request_waiting++;
        rptr = requests;
        null_requests = 0;
        for (i = 0; i < count; i++) {
            request = *rptr;
            if (MPI_REQUEST_NULL == request) {
                null_requests++;
                continue;
            }
            if (request->req_complete == true) {
                completed = i;
                break;
            }
            rptr++;
        }
        if(null_requests == count)
            break;
        if (completed < 0) {
            ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
        }
    } while (completed < 0);
    ompi_request_waiting--;
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

finished:

    if(null_requests == count) {
        *index = MPI_UNDEFINED;
        *status = ompi_request_null.req_status;
    } else {

        /* return status */
        if (MPI_STATUS_IGNORE != status) {
            *status = request->req_status;
        }
    
        /* return request to pool */ 
        rc = request->req_fini(rptr);
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
        OMPI_THREAD_LOCK(&ompi_request_lock);
        ompi_request_waiting++;
        do {
            completed = 0;
            rptr = requests;
            for (i = 0; i < count; i++) {
                request = *rptr++;
                if (request->req_complete == true) {
                    completed++;
                }
            }
            if (completed != count) {
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            }
        } while (completed != count);
        ompi_request_waiting--;
        OMPI_THREAD_UNLOCK(&ompi_request_lock);
    }

    if (MPI_STATUS_IGNORE != statuses) {
        /* fill out status and free request if required */
        rptr = requests;
        for (i = 0; i < count; i++) {
            int rc;
            request = *rptr;
            statuses[i] = request->req_status;
            rc = request->req_fini(rptr);
            rptr++;
        }
    } else {
        /* free request if required */
        rptr = requests;
        for (i = 0; i < count; i++) {
            int rc;
            request = *rptr;
            rc = request->req_fini(rptr);
            rptr++;
        }
    }
    return OMPI_SUCCESS;
}


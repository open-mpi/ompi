/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "include/constants.h"
#include "request/request.h"


int ompi_request_wait(
    size_t count,
    ompi_request_t ** requests,
    int *index, 
    ompi_status_public_t * status)
{
#if OMPI_HAVE_THREADS
    int c;
#endif
    int i, rc;
    int completed = -1;
    ompi_request_t **rptr;
    ompi_request_t *request;

#if OMPI_HAVE_THREADS
    /* poll for completion */
    ompi_atomic_mb();
    for (c = 0; completed < 0 && c < ompi_request_poll_iterations; c++) {
        rptr = requests;
        for (i = 0; i < count; i++) {
            request = *rptr;
            if (request->req_complete == true) {
                completed = i;
                break;
            } 
            rptr++;
        }
    }
#endif

    if (completed < 0) {
        /* give up and sleep until completion */
        OMPI_THREAD_LOCK(&ompi_request_lock);
        do {
            ompi_request_waiting++;
            rptr = requests;
            for (i = 0; i < count; i++) {
                request = *rptr;
                if (request->req_complete == true) {
                    completed = i;
                    break;
                }
                rptr++;
            }
            if (completed < 0) {
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            }
        } while (completed < 0);
        ompi_request_waiting--;
        OMPI_THREAD_UNLOCK(&ompi_request_lock);
    }

    /* return status */
    if (NULL != status) {
        *status = request->req_status;
    }
    
    /* return request to pool */ 
    rc = request->req_free(request);
    *rptr = NULL;
    *index = completed;
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
        request = *rptr;
        if (request == NULL || request->req_complete == true) {
            completed++;
        }
        rptr++;
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
                request = *rptr;
                if (request == NULL || request->req_complete == true) {
                    completed++;
                    continue;
                }
            }
            if (completed != count) {
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            }
        } while (completed != count);
        ompi_request_waiting--;
        OMPI_THREAD_UNLOCK(&ompi_request_lock);
    }

    if (NULL != statuses) {
        /* fill out status and free request if required */
        rptr = requests;
        for (i = 0; i < count; i++) {
            request = *rptr;
            if (NULL == request) {
                statuses[i] = ompi_request_null.req_status;
            } else {
                int rc;
                statuses[i] = request->req_status;
                rc = request->req_free(request);
                if(rc != OMPI_SUCCESS)
                    return rc;
                *rptr = NULL;
            } 
            rptr++;
        }
    } else {
        /* free request if required */
        rptr = requests;
        for (i = 0; i < count; i++) {
            request = *rptr;
            if (NULL != request) {
                int rc = request->req_free(request);
                if(rc != OMPI_SUCCESS)
                    return rc;
                *rptr = NULL;
            }
            rptr++;
        }
    }
    return OMPI_SUCCESS;
}

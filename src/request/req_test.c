/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "include/constants.h"
#include "request/request.h"


int ompi_request_test_any(
    size_t count,
    ompi_request_t ** requests,
    int *index,
    int *completed, 
    ompi_status_public_t * status)
{
    size_t i;
    size_t num_requests_null_inactive = 0;
    ompi_request_t **rptr;
    ompi_request_t *request;

    ompi_atomic_mb();
    rptr = requests;
    for (i = 0; i < count; i++) {
        request = *rptr;
        if (request == MPI_REQUEST_NULL ||
            request->req_state == OMPI_REQUEST_INACTIVE) {
            num_requests_null_inactive++;
            continue;
        }
        if (request->req_complete) {
            *index = i;
            *completed = true;
            if (MPI_STATUS_IGNORE != status) {
                *status = request->req_status;
            }
            return request->req_fini(rptr);
        }
        rptr++;
    }

    /* Only fall through here if we found nothing */
    *index = MPI_UNDEFINED;
    *completed = (num_requests_null_inactive == count) ? true : false;
    if (MPI_STATUS_IGNORE != status) {
        *status = ompi_status_empty;
    }
    return OMPI_SUCCESS;
}


int ompi_request_test_all(
    size_t count,
    ompi_request_t ** requests,
    int *completed, 
    ompi_status_public_t * statuses)
{
    size_t i;
    ompi_request_t **rptr;
    size_t num_completed;
    ompi_request_t *request;

    ompi_atomic_mb();
    rptr = requests;
    for (i = 0; i < count; i++) {
        request = *rptr;
        if (request == MPI_REQUEST_NULL || request->req_complete) {
            num_completed++;
        }
        rptr++;
    }

    if (num_completed != count) {
        *completed = false;
        return OMPI_SUCCESS;
    }

    *completed = true;
    if (MPI_STATUSES_IGNORE != statuses) {
        /* fill out completion status and free request if required */
        rptr = requests;
        for (i = 0; i < count; i++) {
            int rc;
            request  = *rptr;
            if(request == MPI_REQUEST_NULL || 
               request->req_state == OMPI_REQUEST_INACTIVE) {
                statuses[i] = ompi_status_empty;
            } else {
                statuses[i] = request->req_status;
            }
            rc = request->req_fini(rptr);
            if(rc != OMPI_SUCCESS)
                return rc;
            rptr++;
        }
    } else {
        /* free request if required */
        rptr = requests;
        for (i = 0; i < count; i++) {
            int rc;
            request = *rptr;
            rc = request->req_fini(rptr);
            if(rc != OMPI_SUCCESS)
                return rc;
            rptr++;
        }
    }
    return OMPI_SUCCESS;
}


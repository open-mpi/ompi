/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "include/constants.h"
#include "request/request.h"


int ompi_request_test(
    size_t count,
    ompi_request_t ** requests,
    int *index,
    int *completed, 
    ompi_status_public_t * status)
{
    int rc;
    size_t i;
    ompi_request_t **rptr;
    ompi_request_t *request;

    ompi_atomic_mb();
    rptr = requests;
    for (i = 0; i < count; i++) {
        request = *rptr;
        if (request == NULL)
            continue;
        if (request->req_complete) {
            *index = i;
            *completed = true;
            if (NULL != status) {
                *status = request->req_status;
            }
            rc = request->req_free(request);
            if(rc != OMPI_SUCCESS)
                return rc;
            *rptr = NULL;
            return OMPI_SUCCESS;
        }
        rptr++;
    }

    *index = MPI_UNDEFINED;
    *completed = false;
    if (NULL != status)
        *status = ompi_request_null.req_status;
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
        if (request == NULL || request->req_complete) {
            num_completed++;
        }
        rptr++;
    }

    if (num_completed != count) {
        *completed = false;
        return OMPI_SUCCESS;
    }

    *completed = true;
    if (NULL != statuses) {
        /* fill out completion status and free request if required */
        rptr = requests;
        for (i = 0; i < count; i++) {
            request  = *rptr;
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
            ompi_request_t *request = *rptr;
            if (NULL != request) {
                int rc = request->req_free(request);
                if(rc != OMPI_SUCCESS)
                    return rc;
            }
            rptr++;
        }
    }
    return OMPI_SUCCESS;
}


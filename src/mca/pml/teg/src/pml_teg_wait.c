/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_teg.h"
#include "pml_teg_sendreq.h"
#include "pml_teg_recvreq.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/pml/base/pml_base_request.h"
#include "mpi.h"  /* we need at least MPI_UNDEFINED and MPI_REQUEST_NULL */

int mca_pml_teg_wait(size_t count,
                     ompi_request_t ** request,
                     int *index, ompi_status_public_t * status)
{
#if OMPI_HAVE_THREADS
    int c;
#endif
    int i;
    int null_requests = 0;
    int completed = -1;
    mca_pml_base_request_t *pml_request = NULL;

#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_waits++;
#endif

#if OMPI_HAVE_THREADS
    /* poll for completion */
    ompi_atomic_mb();
    for (c = 0; completed < 0 && c < mca_pml_teg.teg_poll_iterations; c++) {
        for (i = 0; i < count; i++) {
            pml_request = (mca_pml_base_request_t *) request[i];
            if (MPI_REQUEST_NULL == (ompi_request_t*)pml_request) {
                ++null_requests;
                continue;
            }
            if (true == pml_request->req_mpi_done) {
                completed = i;
                break;
            }
        }
    }
#endif

    if ((completed < 0) || (null_requests != count)) {
        /* give up and sleep until completion */
        OMPI_THREAD_LOCK(&mca_pml_teg.teg_request_lock);
        mca_pml_teg.teg_request_waiting++;
        do {
            null_requests = 0;
            for (i = 0; i < count; i++) {
                pml_request = (mca_pml_base_request_t *) request[i];
                if (MPI_REQUEST_NULL == (ompi_request_t*)pml_request) {
                    ++null_requests;
                    continue;
                }
                if (pml_request->req_mpi_done == true) {
                    completed = i;
                    break;
                }
            }
            /* for performance reason I prefer to make the test only once not
             * on all ifs and whiles around here.
             */
            if (null_requests == count) break;
            if (completed < 0) {
#if MCA_PML_TEG_STATISTICS
                mca_pml_teg.teg_condition_waits++;
#endif
                ompi_condition_wait(&mca_pml_teg.teg_request_cond,
                                    &mca_pml_teg.teg_request_lock);
            }
        } while (completed < 0);
        mca_pml_teg.teg_request_waiting--;
        OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
    }
    
    if( null_requests == count ) {
        *index = MPI_UNDEFINED;
        *status = mca_pml_teg.teg_request_null.req_status;
    } else {
        /* return status */
        if (MPI_STATUS_IGNORE != status) {
            *status = pml_request->req_status;
        }
        
        *index = completed;
        /* return request to pool */
        MCA_PML_TEG_FINI( &(request[completed]) );
    }
    return OMPI_SUCCESS;
}


int mca_pml_teg_wait_all(size_t count,
                         ompi_request_t ** requests,
                         ompi_status_public_t * statuses)
{
    int completed = 0, i;
    for (i = 0; i < count; i++) {
        mca_pml_base_request_t *pml_request =
            (mca_pml_base_request_t *) requests[i];
        if ((MPI_REQUEST_NULL == (ompi_request_t*)pml_request) || 
            (pml_request->req_mpi_done == true)) {
            completed++;
        }
    }

    /* if all requests have not completed -- defer requiring lock
     * unless required */
    if (completed != count) {
        /*
         * acquire lock and test for completion - if all requests are
         * not completed pend on condition variable until a request
         * completes
         */
        OMPI_THREAD_LOCK(&mca_pml_teg.teg_request_lock);
        mca_pml_teg.teg_request_waiting++;
        do {
            completed = 0;
            for (i = 0; i < count; i++) {
                mca_pml_base_request_t *pml_request =
                    (mca_pml_base_request_t *) requests[i];
                if ((MPI_REQUEST_NULL == (ompi_request_t*)pml_request) ||
                    (pml_request->req_mpi_done == true)) {
                    completed++;
                    continue;
                }
            }
            if (completed != count)
                ompi_condition_wait(&mca_pml_teg.teg_request_cond,
                                    &mca_pml_teg.teg_request_lock);
        } while (completed != count);
        mca_pml_teg.teg_request_waiting--;
        OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
    }

    if (MPI_STATUSES_IGNORE != statuses) {
        /* fill out status and free request if required */
        for (i = 0; i < count; i++) {
            mca_pml_base_request_t *pml_request =
                (mca_pml_base_request_t *) requests[i];
            if (MPI_REQUEST_NULL == (ompi_request_t*)pml_request) {
                statuses[i] = mca_pml_teg.teg_request_null.req_status;
            } else {
                statuses[i] = pml_request->req_status;
                MCA_PML_TEG_FINI(requests + i);
            }
        }
    } else {
        /* free request if required */
        for (i = 0; i < count; i++) {
            mca_pml_base_request_t *pml_request =
                (mca_pml_base_request_t *) requests[i];
            if (MPI_REQUEST_NULL != (ompi_request_t*)pml_request) {
                MCA_PML_TEG_FINI(requests + i);
            }
        }
    }
    return OMPI_SUCCESS;
}

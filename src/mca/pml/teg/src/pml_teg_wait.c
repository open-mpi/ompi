#include "pml_teg.h"
#include "pml_teg_sendreq.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/pml/base/pml_base_request.h"



int mca_pml_teg_wait(
    size_t count,
    ompi_request_t** request,
    int *index,
    ompi_status_public_t* status)
{
#if OMPI_HAVE_THREADS
    int c;
#endif
    int i;
    int completed = -1;
    mca_pml_base_request_t* pml_request;

#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_waits++;
#endif

#if OMPI_HAVE_THREADS

    /* poll for completion */
    ompi_atomic_mb();
    for(c=0; completed < 0 && c < mca_pml_teg.teg_poll_iterations; c++) {
        for(i=0; i<count; i++) {
            pml_request = (mca_pml_base_request_t*)request[i];
            if(pml_request == NULL)
                continue; 
            if(pml_request->req_mpi_done == true) {
                completed = i;
                break;
            }
        }
    }
#endif

    if(completed < 0) {
        /* give up and sleep until completion */
        OMPI_THREAD_LOCK(&mca_pml_teg.teg_request_lock);
        mca_pml_teg.teg_request_waiting++;
        do {
            for(i=0; i<count; i++) {
                pml_request = (mca_pml_base_request_t*)request[i];
                if(pml_request->req_mpi_done == true) {
                    completed = i;
                    break;
                }
            }
            if(completed < 0) {
#if MCA_PML_TEG_STATISTICS
                mca_pml_teg.teg_condition_waits++;
#endif
                ompi_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            }
        } while(completed < 0);
        mca_pml_teg.teg_request_waiting--;
        OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
    }

    /* return status */
    if (NULL != status) {
       *status = pml_request->req_status;
    }

    /* return request to pool */
    MCA_PML_TEG_FINI(request);
    *index = completed;
    return OMPI_SUCCESS;
}


int mca_pml_teg_wait_all(
    size_t count,
    ompi_request_t** requests,
    ompi_status_public_t* statuses)
{
    int completed = 0, i;
    for(i=0; i<count; i++) {
        mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
        if(pml_request == NULL || pml_request->req_mpi_done == true) {
            completed++;
        } 
    }

    /* if all requests have not completed -- defer requiring lock unless required */
    if(completed != count) {
        /*
         * acquire lock and test for completion - if all requests are not completed
         * pend on condition variable until a request completes 
         */
        OMPI_THREAD_LOCK(&mca_pml_teg.teg_request_lock);
        mca_pml_teg.teg_request_waiting++;
        do {
            completed = 0;
            for(i=0; i<count; i++) {
                mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
                if(pml_request == NULL || pml_request->req_mpi_done == true) {
                    completed++;
                    continue;
                }
            } 
            if(completed != count)
                ompi_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
        } while (completed != count);
        mca_pml_teg.teg_request_waiting--;
        OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
    }

    if(NULL != statuses) {
        /* fill out status and free request if required */
        for(i=0; i<count; i++) {
            mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
            if (NULL == pml_request) {
                statuses[i] = mca_pml_teg.teg_request_null.req_status;
            } else {
                statuses[i] = pml_request->req_status;
                MCA_PML_TEG_FINI(requests+i);
            }
        }
    } else {
        /* free request if required */
        for(i=0; i<count; i++) {
            mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
            if (NULL != pml_request) {
                MCA_PML_TEG_FINI(requests+i);
            }
        }
    }
    return OMPI_SUCCESS;
}


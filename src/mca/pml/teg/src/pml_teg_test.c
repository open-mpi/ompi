#include "pml_teg.h"

                                                                                                                              
int mca_pml_teg_test(
    size_t count,
    lam_request_t** requests,
    int *index,
    int *completed,
    lam_status_public_t* status)
{
    size_t i;
    for(i=0; i<count; i++) {
        mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
        if(pml_request == NULL)
            continue;
        if(pml_request->req_mpi_done) {
            *index = i;
            *completed = true;
            if (NULL != status)
                *status = pml_request->req_status;
            if(false == pml_request->req_persistent)
                mca_pml_teg_free(requests+i);
        }
    }

    *index = MPI_UNDEFINED;
    *completed = false;
    if(NULL != status) 
        *status = mca_pml_teg.teg_request_null.req_status;
    return LAM_SUCCESS;
}


int mca_pml_teg_test_all(
    size_t count,
    lam_request_t** requests,
    int *completed,
    lam_status_public_t* statuses)
{
    size_t i;
    size_t num_completed;
    for(i=0; i<count; i++) {
        mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
        if(pml_request == NULL || pml_request->req_mpi_done)
            num_completed++;
    }

    if(num_completed != count) {
        *completed = false;
        return LAM_SUCCESS;
    }

    *completed = true;
    if(NULL != statuses) {
        /* fill out completion status and free request if required */
        for(i=0; i<count; i++) {
            mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
            if(NULL == pml_request) {
                statuses[i] = mca_pml_teg.teg_request_null.req_status;
            } else {
                statuses[i] = pml_request->req_status;
                if(false == pml_request->req_persistent)
                    mca_pml_teg_free(requests+i);
            }
        }
    } else {
        /* free request if required */
        for(i=0; i<count; i++) {
            mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
            if(NULL != pml_request && false == pml_request->req_persistent)
                mca_pml_teg_free(requests+i);
        }
    }
    return LAM_SUCCESS;
}


/*
 * $HEADER$
 */

#include "mca/mpi/pml/base/pml_base_sendreq.h"
#include "pml_teg.h"


int mca_pml_teg_isend(
    void *buf,
    size_t size,
    lam_datatype_t *datatype,
    int dest,
    int tag,
    lam_communicator_t* comm,
    mca_pml_base_request_type_t req_type,
    lam_request_t **request)
{
    int rc;
    mca_pml_base_send_request_t* sendreq = 
        (mca_pml_base_send_request_t*)lam_free_list_get(&mca_pml_teg.teg_send_free_list, &rc);
    if(sendreq == 0) 
        return rc;

    mca_pml_base_send_request_rinit(
        sendreq,
        buf,
        size,
        datatype,
        dest,
        tag,
        comm,
        req_type,
        false
        );
         
    if((rc = mca_pml_teg_send_request_start(sendreq)) != LAM_SUCCESS)
        return rc;
    *request = (lam_request_t*)sendreq;
    return LAM_SUCCESS;
}


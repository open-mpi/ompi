#include "pml_teg_recvreq.h"

                                                                                                             
int mca_pml_teg_irecv_init(
    void *addr,
    size_t count,
    lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    struct lam_request_t **request)
{
    int rc;
    mca_ptl_base_recv_request_t *recvreq = mca_pml_teg_recv_request_alloc(&rc);
    if(NULL == recvreq)
        return rc;
    
    mca_ptl_base_recv_request_init(
        recvreq,
        addr,
        count,
        datatype,
        src,
        tag,
        comm,
        true);

    *request = (lam_request_t*)recvreq;
    return LAM_SUCCESS;
}
                                                                                                                              
int mca_pml_teg_irecv(
    void *addr,
    size_t count,
    lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    struct lam_request_t **request)
{
    int rc;

    mca_ptl_base_recv_request_t *recvreq = mca_pml_teg_recv_request_alloc(&rc);
    if(NULL == recvreq)
        return rc;

    mca_ptl_base_recv_request_init(
        recvreq,
        addr,
        count,
        datatype,
        src,
        tag,
        comm,
        false);

    if((rc = mca_pml_teg_recv_request_start(recvreq)) != LAM_SUCCESS) {
        mca_pml_teg_recv_request_return(recvreq);
        return rc;
    }
    *request = (lam_request_t*)recvreq;
    return LAM_SUCCESS;
}


int mca_pml_teg_recv(
    void *addr,
    size_t count,
    lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    lam_status_public_t* status)
{
    int rc, index;
    mca_ptl_base_recv_request_t *recvreq = mca_pml_teg_recv_request_alloc(&rc);
    if(NULL == recvreq)
        return rc;
    
    mca_ptl_base_recv_request_init(
        recvreq,
        addr,
        count,
        datatype,
        src,
        tag,
        comm,
        false);

    if((rc = mca_pml_teg_recv_request_start(recvreq)) != LAM_SUCCESS) {
        mca_pml_teg_recv_request_return(recvreq);
        return rc;
    }
    return mca_pml_teg_wait(1, (lam_request_t**)&recvreq, &index, status);
}


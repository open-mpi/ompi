/*
 * $HEADER$
 */

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_isend_init(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    lam_communicator_t* comm,
    lam_request_t **request)
{
    int rc;

    mca_ptl_base_send_request_t* sendreq = mca_pml_teg_send_request_alloc(comm,dst,&rc);
    if(rc != LAM_SUCCESS)
        return rc;
 
    mca_ptl_base_send_request_init(
        sendreq,
        buf,
        count,
        datatype,
        dst,
        tag,
        comm,
        sendmode,
        true
        );
         
    *request = (lam_request_t*)sendreq;
    return LAM_SUCCESS;
}
         

int mca_pml_teg_isend(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    lam_communicator_t* comm,
    lam_request_t **request)
{
    int rc;
    mca_ptl_base_send_request_t* sendreq = mca_pml_teg_send_request_alloc(comm,dst,&rc);
#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_isends++;
#endif
    if(rc != LAM_SUCCESS)
        return rc;
    mca_ptl_base_send_request_init(
        sendreq,
        buf,
        count,
        datatype,
        dst,
        tag,
        comm,
        sendmode,
        false
        );
         
    if((rc = mca_pml_teg_send_request_start(sendreq)) != LAM_SUCCESS)
        return rc;
    *request = (lam_request_t*)sendreq;
    return LAM_SUCCESS;
}


int mca_pml_teg_send(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    lam_communicator_t* comm)
{
    int rc, index;
    mca_ptl_base_send_request_t* sendreq = mca_pml_teg_send_request_alloc(comm,dst,&rc);
#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_sends++;
#endif
    if(rc != LAM_SUCCESS)
        return rc;

    mca_ptl_base_send_request_init(
        sendreq,
        buf,
        count,
        datatype,
        dst,
        tag,
        comm,
        sendmode,
        false
        );
         
    if((rc = mca_pml_teg_send_request_start(sendreq)) != LAM_SUCCESS)
        return rc;
    return mca_pml_teg_wait(1, (lam_request_t**)&sendreq, &index, NULL);
}


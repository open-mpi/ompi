/*
 * $HEADER$
 */

#ifndef LAM_PML_TEG_SEND_REQUEST_H
#define LAM_PML_TEG_SEND_REQUEST_H

#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_sendfrag.h"


int mca_pml_teg_send_request_schedule(mca_ptl_base_send_request_t* req, bool* complete);
int mca_pml_teg_send_request_progress(void);


static inline int mca_pml_teg_send_request_alloc(
    lam_communicator_t* comm, 
    int dst,
    mca_ptl_base_send_request_t** sendreq)
{
    mca_pml_proc_t *proc = mca_pml_teg_proc_lookup_remote(comm,dst);
    mca_ptl_t* ptl;

    THREAD_SCOPED_LOCK(&proc->proc_lock,
        (ptl = mca_ptl_array_get_next_ptl(&proc->proc_ptl_first)));

    int rc = ptl->ptl_request_alloc(ptl,sendreq);
    if(rc != LAM_SUCCESS)
        return rc;
    (*sendreq)->req_first_ptl = ptl;
    return LAM_SUCCESS;
}


static inline int mca_pml_teg_send_request_start(
    mca_ptl_base_send_request_t* req)
{
    mca_ptl_t* ptl = req->req_first_ptl;
    size_t first_fragment_size = ptl->ptl_frag_first_size;

    // queue for pending acknowledgment
    THREAD_SCOPED_LOCK(&mca_pml_teg.teg_lock,
        lam_list_append(&mca_pml_teg.teg_pending_acks, (lam_list_item_t*)req));

    // start the first fragment
    if(req->req_length < first_fragment_size)
        first_fragment_size = req->req_length;
    return req->req_first_ptl->ptl_send(ptl, req, first_fragment_size);
}


#endif


/*
 * $HEADER$
 */

#ifndef LAM_PML_TEG_SEND_REQUEST_H
#define LAM_PML_TEG_SEND_REQUEST_H

#include "pml_teg_proc.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_sendfrag.h"


int mca_pml_teg_send_request_schedule(mca_ptl_base_send_request_t* req, bool* complete);


static inline mca_ptl_base_send_request_t* mca_pml_teg_send_request_alloc(
    lam_communicator_t* comm, 
    int dst,
    int *rc)
{
    mca_ptl_base_send_request_t* sendreq;
    mca_pml_proc_t *proc = mca_pml_teg_proc_lookup_remote(comm,dst);
    mca_ptl_proc_t* ptl_proc;
    mca_ptl_t* ptl;

    THREAD_SCOPED_LOCK(&proc->proc_lock,
        (ptl_proc = mca_ptl_array_get_next(&proc->proc_ptl_first)));
    ptl = ptl_proc->ptl;
    *rc = ptl->ptl_request_alloc(ptl,&sendreq);
    if(NULL != sendreq)
        sendreq->req_peer = ptl_proc->ptl_peer;
    return sendreq;
}

static inline void mca_ptl_base_send_request_return(
    mca_ptl_base_send_request_t* request)
{
    request->super.req_status = MCA_PML_STATUS_INVALID;
    request->req_owner->ptl_request_return(request->req_owner, request);
}

static inline int mca_pml_teg_send_request_start(
    mca_ptl_base_send_request_t* req)
{
    mca_ptl_t* ptl = req->req_owner;
    size_t first_fragment_size = ptl->ptl_first_frag_size;
    int rc;

    // start the first fragment
    if(req->req_length < first_fragment_size)
        first_fragment_size = req->req_length;
    rc = ptl->ptl_send(ptl, req->req_peer, req, first_fragment_size);
    if(rc != LAM_SUCCESS)
        return rc;
    return LAM_SUCCESS;
}

void mca_pml_teg_send_request_progress(
    mca_ptl_base_send_request_t* send_request,
    mca_ptl_base_send_frag_t* send_frag
);

#endif


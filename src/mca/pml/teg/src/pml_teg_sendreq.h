/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OMPI_PML_TEG_SEND_REQUEST_H
#define OMPI_PML_TEG_SEND_REQUEST_H

#include "pml_teg_proc.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_comm.h"


#define MCA_PML_TEG_SEND_REQUEST_ALLOC( \
    comm, \
    dst, \
    sendreq, \
    rc) \
{ \
    mca_pml_proc_t *proc = mca_pml_teg_proc_lookup_remote(comm,dst); \
    mca_ptl_proc_t* ptl_proc; \
    mca_ptl_t* ptl; \
\
    THREAD_SCOPED_LOCK(&proc->proc_lock, \
        (ptl_proc = mca_ptl_array_get_next(&proc->proc_ptl_first))); \
    ptl = ptl_proc->ptl; \
    rc = ptl->ptl_request_alloc(ptl,&sendreq); \
    if(NULL != sendreq) \
        sendreq->req_peer = ptl_proc->ptl_peer; \
}

#define MCA_PML_TEG_SEND_REQUEST_RETURN(request) \
    request->req_owner->ptl_request_return(request->req_owner, request);


static inline int mca_pml_teg_send_request_start(
    mca_pml_base_send_request_t* req)
{
    mca_ptl_t* ptl = req->req_owner;
    size_t first_fragment_size = ptl->ptl_first_frag_size;
    size_t offset = req->req_offset;
    int flags, rc;

    /* initialize request state and message sequence number */
    req->super.super.req_state = OMPI_REQUEST_ACTIVE;
    req->super.req_sequence = mca_pml_ptl_comm_send_sequence(
        req->super.req_comm->c_pml_comm,
        req->super.req_peer);

    /* start the first fragment */
    if(first_fragment_size == 0 || req->req_bytes_packed <= first_fragment_size) {
        first_fragment_size = req->req_bytes_packed;
        flags = (req->req_send_mode == MCA_PML_BASE_SEND_SYNCHRONOUS) ?
        MCA_PTL_FLAGS_ACK_MATCHED : 0;
    } else {
        /* require match for first fragment of a multi-fragment */
        flags = MCA_PTL_FLAGS_ACK_MATCHED;
    }
    rc = ptl->ptl_put(ptl, req->req_peer, req, offset, first_fragment_size, flags);
    if(rc != OMPI_SUCCESS)
        return rc;
    return OMPI_SUCCESS;
}


void mca_pml_teg_send_request_schedule(mca_pml_base_send_request_t* req);


void mca_pml_teg_send_request_progress(
    struct mca_ptl_t* ptl,
    mca_pml_base_send_request_t* send_request,
    mca_ptl_base_send_frag_t* send_frag
);

#endif


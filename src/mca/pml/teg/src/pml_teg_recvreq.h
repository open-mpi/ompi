/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef LAM_PML_TEG_RECV_REQUEST_H
#define LAM_PML_TEG_RECV_REQUEST_H

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"


/**
 *  Allocate a recv request from the modules free list.
 *
 *  @param rc (OUT)  LAM_SUCCESS or error status on failure.
 *  @return          Receive request.
 */
static inline mca_ptl_base_recv_request_t* mca_pml_teg_recv_request_alloc(int *rc)
{
    return (mca_ptl_base_recv_request_t*)lam_free_list_get(&mca_pml_teg.teg_recv_requests, rc);
}

/**
 *  Return a recv request to the modules free list.
 *
 *  @param request (IN)  Receive request.
 */
static inline void mca_pml_teg_recv_request_return(mca_ptl_base_recv_request_t* request)
{
    lam_free_list_return(&mca_pml_teg.teg_recv_requests, (lam_list_item_t*)request);
}

/**
 * Start an initialized request.
 *
 * @param request  Receive request.
 * @return         LAM_SUCESS or error status on failure.
 */
static inline int mca_pml_teg_recv_request_start(mca_ptl_base_recv_request_t* request)
{
    if(request->super.req_peer == LAM_ANY_SOURCE) {
        mca_ptl_base_recv_request_match_wild(request);
    } else {
        mca_ptl_base_recv_request_match_specific(request);
    }
    return LAM_SUCCESS;
}

/**
 *  Update status of a recv request based on the completion status of 
 *  the receive fragment.
 *
 *  @param request (IN)  Receive request.
 *  @param frag (IN)     Receive fragment.
 */
void mca_pml_teg_recv_request_progress(
    mca_ptl_base_recv_request_t* request,
    mca_ptl_base_recv_frag_t* frag
);


#endif


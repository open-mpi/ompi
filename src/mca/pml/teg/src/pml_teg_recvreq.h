/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OMPI_PML_TEG_RECV_REQUEST_H
#define OMPI_PML_TEG_RECV_REQUEST_H

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"


/**
 *  Allocate a recv request from the modules free list.
 *
 *  @param rc (OUT)  OMPI_SUCCESS or error status on failure.
 *  @return          Receive request.
 */
#define MCA_PML_TEG_RECV_REQUEST_ALLOC(recvreq, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_pml_teg.teg_recv_requests, item, rc); \
    recvreq = (mca_pml_base_recv_request_t*)item; \
    }

/**
 *  Return a recv request to the modules free list.
 *
 *  @param request (IN)  Receive request.
 */
#define MCA_PML_TEG_RECV_REQUEST_RETURN(request) \
    OMPI_FREE_LIST_RETURN(&mca_pml_teg.teg_recv_requests, (ompi_list_item_t*)request);
                                                                                                                                    
/**
 * Attempt to match the request against the unexpected fragment list
 * for all source ranks w/in the communicator.
 *
 * @param request (IN)   Request to match.
 */
void mca_pml_teg_recv_request_match_wild(mca_pml_base_recv_request_t* request);
                                                                                                                                 
/**
 * Attempt to match the request against the unexpected fragment list
 * for a specific source rank.
 *
 * @param request (IN)   Request to match.
 */
void mca_pml_teg_recv_request_match_specific(mca_pml_base_recv_request_t* request);
                                                                                                                                 
/**
 * Start an initialized request.
 *
 * @param request  Receive request.
 * @return         OMPI_SUCESS or error status on failure.
 */
static inline int mca_pml_teg_recv_request_start(mca_pml_base_recv_request_t* request)
{
    request->req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;
    if(request->req_base.req_peer == OMPI_ANY_SOURCE) {
        mca_pml_teg_recv_request_match_wild(request);
    } else {
        mca_pml_teg_recv_request_match_specific(request);
    }
    return OMPI_SUCCESS;
}

/**
 *  Update status of a recv request based on the completion status of 
 *  the receive fragment.
 *
 *  @param ptl (IN)              The PTL pointer.
 *  @param request (IN)          Receive request.
 *  @param bytes_received (IN)   Bytes received from peer.
 *  @param bytes_delivered (IN)  Bytes delivered to application.
 */
void mca_pml_teg_recv_request_progress(
    struct mca_ptl_t* ptl,
    mca_pml_base_recv_request_t* request,
    size_t bytes_received,
    size_t bytes_delivered
);

#endif


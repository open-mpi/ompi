/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OMPI_PML_OB1_RECV_REQUEST_H
#define OMPI_PML_OB1_RECV_REQUEST_H

#include "pml_ob1.h"
#include "pml_ob1_proc.h"
#include "mca/pml/base/pml_base_recvreq.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct  mca_pml_ob1_recv_request_t {
    mca_pml_base_recv_request_t req_recv;
    size_t req_bytes_received;
    size_t req_bytes_delivered;

    /* note that we allocate additional space for the recv
     * request to increase the array size based on run-time
     * parameters for the pipeline depth. So... this MUST be
     * the last element of this struct.
    */
    mca_bmi_base_descriptor_t *req_pipeline[1];
};
typedef struct mca_pml_ob1_recv_request_t mca_pml_ob1_recv_request_t;


OBJ_CLASS_DECLARATION(mca_pml_ob1_recv_request_t);


/**
 *  Allocate a recv request from the modules free list.
 *
 *  @param rc (OUT)  OMPI_SUCCESS or error status on failure.
 *  @return          Receive request.
 */
#define MCA_PML_OB1_RECV_REQUEST_ALLOC(recvreq, rc)                 \
    do {                                                             \
        ompi_list_item_t* item;                                      \
        rc = OMPI_SUCCESS;                                           \
        OMPI_FREE_LIST_GET(&mca_pml_ob1.recv_requests, item, rc);   \
        recvreq = (mca_pml_ob1_recv_request_t*)item;                \
    } while(0)


/**
 * Initialize a receive request with call parameters.
 *
 * @param request (IN)       Receive request.
 * @param addr (IN)          User buffer.
 * @param count (IN)         Number of elements of indicated datatype.
 * @param datatype (IN)      User defined datatype.
 * @param src (IN)           Source rank w/in the communicator.
 * @param tag (IN)           User defined tag.
 * @param comm (IN)          Communicator.
 * @param persistent (IN)    Is this a ersistent request.
 */
#define MCA_PML_OB1_RECV_REQUEST_INIT(                               \
    request,                                                          \
    addr,                                                             \
    count,                                                            \
    datatype,                                                         \
    src,                                                              \
    tag,                                                              \
    comm,                                                             \
    persistent)                                                       \
{                                                                     \
    MCA_PML_BASE_RECV_REQUEST_INIT(                                   \
        &(request)->req_recv,                                         \
        addr,                                                         \
        count,                                                        \
        datatype,                                                     \
        src,                                                          \
        tag,                                                          \
        comm,                                                         \
        persistent);                                                  \
}

/**
 *  Return a recv request to the modules free list.
 *
 *  @param request (IN)  Receive request.
 */
#define MCA_PML_OB1_RECV_REQUEST_RETURN(request)                                       \
    do {                                                                                \
        MCA_PML_BASE_RECV_REQUEST_FINI(&request->req_recv);                           \
        OMPI_FREE_LIST_RETURN(&mca_pml_ob1.recv_requests, (ompi_list_item_t*)request); \
    } while(0)

/**
 * Attempt to match the request against the unexpected fragment list
 * for all source ranks w/in the communicator.
 *
 * @param request (IN)   Request to match.
 */
void mca_pml_ob1_recv_request_match_wild(mca_pml_ob1_recv_request_t* request);
                                                                                                                                 
/**
 * Attempt to match the request against the unexpected fragment list
 * for a specific source rank.
 *
 * @param request (IN)   Request to match.
 */
void mca_pml_ob1_recv_request_match_specific(mca_pml_ob1_recv_request_t* request);
                                                                                                                                 
/**
 * Start an initialized request.
 *
 * @param request  Receive request.
 * @return         OMPI_SUCESS or error status on failure.
 */
#define MCA_PML_OB1_RECV_REQUEST_START(request)                                   \
{                                                                                 \
    /* init/re-init the request */                                                \
    (request)->req_bytes_received = 0;                                            \
    (request)->req_bytes_delivered = 0;                                           \
    (request)->req_recv.req_base.req_pml_complete = false;                        \
    (request)->req_recv.req_base.req_ompi.req_complete = false;                   \
    (request)->req_recv.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;        \
                                                                                  \
    /* always set the req_status.MPI_TAG to ANY_TAG before starting the           \
     * request. This field is used if cancelled to find out if the request        \
     * has been matched or not.                                                   \
     */                                                                           \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_TAG = OMPI_ANY_TAG;      \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;    \
    (request)->req_recv.req_base.req_ompi.req_status._cancelled = 0;              \
                                                                                  \
    /* attempt to match posted recv */                                            \
    if((request)->req_recv.req_base.req_peer == OMPI_ANY_SOURCE) {                \
        mca_pml_ob1_recv_request_match_wild(request);                             \
    } else {                                                                      \
        mca_pml_ob1_recv_request_match_specific(request);                         \
    }                                                                             \
}


/**
 *
 */

#define MCA_PML_OB1_RECV_REQUEST_UNPACK(                                          \
    request,                                                                      \
    segments,                                                                     \
    num_segments,                                                                 \
    seg_offset,                                                                   \
    data_offset,                                                                  \
    bytes_received,                                                               \
    bytes_delivered)                                                              \
{                                                                                 \
} 


/**
 *
 */

void mca_pml_ob1_recv_request_progress(
    mca_pml_ob1_recv_request_t* req,
    mca_bmi_base_module_t* bmi,
    mca_bmi_base_segment_t* segments,
    size_t num_segments);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif


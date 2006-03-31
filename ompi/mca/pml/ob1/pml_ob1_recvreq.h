/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "pml_ob1_rdma.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_ob1_recv_request_t {
    mca_pml_base_recv_request_t req_recv;
    ompi_ptr_t req_send;
#if OMPI_HAVE_THREAD_SUPPORT
    volatile int32_t req_lock;
#else
    int32_t req_lock;
#endif
    size_t  req_pipeline_depth;
    size_t  req_bytes_received;
    size_t  req_bytes_delivered;
    size_t  req_rdma_offset;
    mca_pml_ob1_rdma_btl_t req_rdma[MCA_PML_OB1_MAX_RDMA_PER_REQUEST];
    uint32_t req_rdma_cnt;
    uint32_t req_rdma_idx;
};
typedef struct mca_pml_ob1_recv_request_t mca_pml_ob1_recv_request_t;


OBJ_CLASS_DECLARATION(mca_pml_ob1_recv_request_t);


/**
 *  Allocate a recv request from the modules free list.
 *
 *  @param rc (OUT)  OMPI_SUCCESS or error status on failure.
 *  @return          Receive request.
 */
#define MCA_PML_OB1_RECV_REQUEST_ALLOC(recvreq, rc)                \
do {                                                               \
   opal_list_item_t* item;                                         \
   rc = OMPI_SUCCESS;                                              \
   OMPI_FREE_LIST_GET(&mca_pml_ob1.recv_requests, item, rc);       \
   recvreq = (mca_pml_ob1_recv_request_t*)item;                    \
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
#define MCA_PML_OB1_RECV_REQUEST_INIT( request,                     \
                                       addr,                        \
                                       count,                       \
                                       datatype,                    \
                                       src,                         \
                                       tag,                         \
                                       comm,                        \
                                       persistent)                  \
do {                                                                \
    MCA_PML_BASE_RECV_REQUEST_INIT( &(request)->req_recv,           \
                                    addr,                           \
                                    count,                          \
                                    datatype,                       \
                                    src,                            \
                                    tag,                            \
                                    comm,                           \
                                    persistent);                    \
} while(0)

/**
 * Mark the request as completed at MPI level for internal purposes.
 *
 *  @param recvreq (IN)  Receive request.
 */
#define MCA_PML_OB1_RECV_REQUEST_MPI_COMPLETE( recvreq )                              \
    do {                                                                              \
       PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_COMPLETE,                             \
                                &(recvreq->req_recv.req_base), PERUSE_RECV );         \
        MCA_PML_BASE_REQUEST_MPI_COMPLETE( &(recvreq->req_recv.req_base.req_ompi) );  \
    } while (0)

/**
 *  Return a recv request to the modules free list.
 *
 *  @param recvreq (IN)  Receive request.
 */
#define MCA_PML_OB1_RECV_REQUEST_PML_COMPLETE(recvreq)                          \
do {                                                                            \
    size_t r;                                                                   \
                                                                                \
    assert( false == recvreq->req_recv.req_base.req_pml_complete );             \
                                                                                \
    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_END,                          \
                             &(recvreq->req_recv.req_base), PERUSE_RECV );      \
                                                                                \
    for( r = 0; r < recvreq->req_rdma_cnt; r++ ) {                              \
        mca_mpool_base_registration_t* btl_reg = recvreq->req_rdma[r].btl_reg;  \
        if( NULL != btl_reg ) {                                                 \
            btl_reg->mpool->mpool_release( btl_reg->mpool, btl_reg );           \
        }                                                                       \
    }                                                                           \
    recvreq->req_rdma_cnt = 0;                                                  \
                                                                                \
    OPAL_THREAD_LOCK(&ompi_request_lock);                                       \
                                                                                \
    if( true == recvreq->req_recv.req_base.req_free_called ) {                  \
        MCA_PML_OB1_RECV_REQUEST_RETURN( recvreq );                             \
    } else {                                                                    \
        /* initialize request status */                                         \
        recvreq->req_recv.req_base.req_pml_complete = true;                     \
        recvreq->req_recv.req_base.req_ompi.req_status._count =                 \
            (recvreq->req_bytes_received < recvreq->req_bytes_delivered ?       \
             recvreq->req_bytes_received : recvreq->req_bytes_delivered);       \
        MCA_PML_OB1_RECV_REQUEST_MPI_COMPLETE( recvreq );                       \
    }                                                                           \
    OPAL_THREAD_UNLOCK(&ompi_request_lock);                                     \
} while(0)

/*
 *  Free the PML receive request
 */
#define MCA_PML_OB1_RECV_REQUEST_RETURN(recvreq) \
{ \
    MCA_PML_BASE_RECV_REQUEST_FINI(&(recvreq)->req_recv);  \
    OMPI_FREE_LIST_RETURN( &mca_pml_ob1.recv_requests,     \
                           (opal_list_item_t*)(recvreq));  \
}

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
 *  Initialize diagnostic code for tracing rdma protocol timing
 */

/**
 * Start an initialized request.
 *
 * @param request  Receive request.
 * @return         OMPI_SUCESS or error status on failure.
 */
#define MCA_PML_OB1_RECV_REQUEST_START(request)                                   \
do {                                                                              \
    /* init/re-init the request */                                                \
    (request)->req_bytes_received = 0;                                            \
    (request)->req_bytes_delivered = 0;                                           \
    (request)->req_lock = 0;                                                      \
    (request)->req_pipeline_depth = 0;                                            \
    (request)->req_rdma_idx = 0;                                                  \
                                                                                  \
    MCA_PML_BASE_RECV_START( &(request)->req_recv.req_base );                     \
                                                                                  \
    /* attempt to match posted recv */                                            \
    if((request)->req_recv.req_base.req_peer == OMPI_ANY_SOURCE) {                \
        mca_pml_ob1_recv_request_match_wild(request);                             \
    } else {                                                                      \
        mca_pml_ob1_recv_request_match_specific(request);                         \
    }                                                                             \
} while (0)


/**
 *
 */

#define MCA_PML_OB1_RECV_REQUEST_MATCHED( request, hdr )                           \
do {                                                                               \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_TAG = (hdr)->hdr_tag;     \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE = (hdr)->hdr_src;  \
                                                                                   \
    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_MSG_MATCH_POSTED_REQ,                     \
                             &((request)->req_recv.req_base), PERUSE_RECV );       \
                                                                                   \
    if((request)->req_recv.req_bytes_packed != 0) {                                \
        ompi_convertor_copy_and_prepare_for_recv(                                  \
                         (request)->req_recv.req_base.req_proc->proc_convertor,    \
                         (request)->req_recv.req_base.req_datatype,                \
                         (request)->req_recv.req_base.req_count,                   \
                         (request)->req_recv.req_base.req_addr,                    \
                         0,                                                        \
                         &(request)->req_recv.req_convertor );                     \
        ompi_convertor_get_unpacked_size( &(request)->req_recv.req_convertor,      \
                                          &(request)->req_bytes_delivered );       \
    }                                                                              \
    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_XFER_BEGIN,                           \
                             &(recvreq->req_recv.req_base), PERUSE_RECV);          \
} while (0)


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
do {                                                                              \
    if(request->req_recv.req_bytes_packed > 0) {                                  \
        struct iovec iov[MCA_BTL_DES_MAX_SEGMENTS];                               \
        uint32_t iov_count = 0;                                                   \
        size_t max_data = bytes_received;                                         \
        int32_t free_after = 0;                                                   \
        size_t n, offset = seg_offset;                                            \
                                                                                  \
        for(n=0; n<num_segments; n++) {                                           \
            mca_btl_base_segment_t* segment = segments+n;                         \
            if(offset >= segment->seg_len) {                                      \
                offset -= segment->seg_len;                                       \
            } else {                                                              \
                iov[iov_count].iov_len = segment->seg_len - seg_offset;           \
                iov[iov_count].iov_base = (void*)((unsigned char*)segment->seg_addr.pval + seg_offset); \
                iov_count++;                                                      \
            }                                                                     \
        }                                                                         \
        ompi_convertor_set_position(                                              \
            &(request->req_recv.req_convertor),                                   \
            &data_offset);                                                        \
        ompi_convertor_unpack(                                                    \
            &(request)->req_recv.req_convertor,                                   \
            iov,                                                                  \
            &iov_count,                                                           \
            &max_data,                                                            \
            &free_after);                                                         \
        bytes_delivered = max_data;                                               \
    } else {                                                                      \
        bytes_delivered = 0;                                                      \
    }                                                                             \
} while (0)


/**
 *
 */

void mca_pml_ob1_recv_request_progress(
    mca_pml_ob1_recv_request_t* req,
    struct mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments);

/**
 *
 */

void mca_pml_ob1_recv_request_matched_probe(
    mca_pml_ob1_recv_request_t* req,
    struct mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments);

/**
 *
 */

void mca_pml_ob1_recv_request_schedule(
    mca_pml_ob1_recv_request_t* req);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif


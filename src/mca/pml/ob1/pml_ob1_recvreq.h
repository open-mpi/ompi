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
#include "mca/mpool/base/base.h"
#include "mca/pml/base/pml_base_recvreq.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_pml_ob1_registration_t {
    struct mca_pml_ob1_endpoint_t* endpoint;
    struct mca_mpool_base_registration_t* registration;
};
typedef struct mca_pml_ob1_registration_t mca_pml_ob1_registration_t;


struct  mca_pml_ob1_recv_request_t {
    mca_pml_base_recv_request_t req_recv;
    struct mca_pml_proc_t *req_proc;
    struct mca_mpool_base_chunk_t* req_chunk;
    struct mca_mpool_base_reg_mpool_t* req_mpool;
    ompi_ptr_t req_send;
    int32_t req_lock;
    size_t  req_pipeline_depth;
    size_t  req_bytes_received;
    size_t  req_bytes_delivered;
    size_t  req_rdma_offset;
    mca_pml_ob1_registration_t req_reg[MCA_MPOOL_BASE_MAX_REG];
    size_t req_num_reg;

#if MCA_PML_OB1_TIMESTAMPS
    unsigned long long ack;
    unsigned long long pin1[MCA_PML_OB1_NUM_TSTAMPS];
    unsigned long long pin2[MCA_PML_OB1_NUM_TSTAMPS];
    unsigned long long fin1[MCA_PML_OB1_NUM_TSTAMPS];
    unsigned long long fin2[MCA_PML_OB1_NUM_TSTAMPS];
    int pin_index;
    int fin_index;
#endif
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
   ompi_list_item_t* item;                                         \
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
#define MCA_PML_OB1_RECV_REQUEST_INIT(                             \
    request,                                                       \
    addr,                                                          \
    count,                                                         \
    datatype,                                                      \
    src,                                                           \
    tag,                                                           \
    comm,                                                          \
    persistent)                                                    \
do {                                                               \
    MCA_PML_BASE_RECV_REQUEST_INIT(                                \
        &(request)->req_recv,                                      \
        addr,                                                      \
        count,                                                     \
        datatype,                                                  \
        src,                                                       \
        tag,                                                       \
        comm,                                                      \
        persistent);                                               \
} while(0)

/**
 *  Return a recv request to the modules free list.
 *
 *  @param request (IN)  Receive request.
 */
#define MCA_PML_OB1_RECV_REQUEST_RETURN(recvreq)                                     \
do {                                                                                 \
    if(NULL != (recvreq)->req_chunk) {                                               \
        mca_mpool_base_reg_mpool_t* reg = (recvreq)->req_chunk->mpools;              \
        while(NULL != reg->mpool) {                                                  \
            OBJ_RELEASE(reg->mpool_registration);                                    \
        }                                                                            \
        OBJ_RELEASE((recvreq)->req_chunk);                                           \
    }                                                                                \
                                                                                     \
    MCA_PML_BASE_RECV_REQUEST_FINI(&(recvreq)->req_recv);                            \
    OMPI_FREE_LIST_RETURN(&mca_pml_ob1.recv_requests, (ompi_list_item_t*)(recvreq)); \
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
 *  Initialize diagnostic code for tracing rdma protocol timing
 */
#if MCA_PML_OB1_TIMESTAMPS 
#define MCA_PML_OB1_RECV_REQUEST_TSTAMPS_INIT(recvreq)                            \
    (request)->fin_index = 0;                                                     \
    (request)->pin_index = 0;                                                    
#else
#define MCA_PML_OB1_RECV_REQUEST_TSTAMPS_INIT(recvreq)
#endif

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
    (request)->req_chunk = NULL;                                                  \
    (request)->req_mpool = NULL;                                                  \
    (request)->req_pipeline_depth = 0;                                            \
    (request)->req_recv.req_base.req_pml_complete = false;                        \
    (request)->req_recv.req_base.req_ompi.req_complete = false;                   \
    (request)->req_recv.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;        \
    MCA_PML_OB1_RECV_REQUEST_TSTAMPS_INIT(request);                               \
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
} while (0)


/**
 *
 */

#define MCA_PML_OB1_RECV_REQUEST_MATCHED(                                            \
    request,                                                                         \
    hdr)                                                                             \
do {                                                                                 \
    (request)->req_recv.req_bytes_packed = (hdr)->hdr_msg_length;                    \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_TAG = (hdr)->hdr_tag;       \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE = (hdr)->hdr_src;    \
                                                                                     \
    if((request)->req_recv.req_bytes_packed != 0) {                                  \
        ompi_proc_t *proc =                                                          \
            ompi_comm_peer_lookup(                                                   \
                (request)->req_recv.req_base.req_comm, (hdr)->hdr_src);              \
                                                                                     \
        (request)->req_proc = proc->proc_pml;                                        \
        ompi_convertor_copy_and_prepare_for_recv( proc->proc_convertor,              \
                                         (request)->req_recv.req_base.req_datatype,  \
                                         (request)->req_recv.req_base.req_count,     \
                                         (request)->req_recv.req_base.req_addr,      \
                                         &(request)->req_recv.req_convertor );       \
    } else {                                                                         \
        (request)->req_proc = NULL;                                                  \
    }                                                                                \
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
    if(request->req_recv.req_base.req_count > 0) {                                \
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
    }                                                                             \
} while (0)


/**
 *
 */

void mca_pml_ob1_recv_request_progress(
    mca_pml_ob1_recv_request_t* req,
    mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments);

/**
 *
 */

void mca_pml_ob1_recv_request_schedule(
    mca_pml_ob1_recv_request_t* req);

/**
 *
 */

void mca_pml_ob1_recv_request_fin(
    mca_pml_ob1_recv_request_t* req,
    size_t rdma_length);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif


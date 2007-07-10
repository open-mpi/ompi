/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
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
#ifndef OMPI_PML_DR_RECV_REQUEST_H
#define OMPI_PML_DR_RECV_REQUEST_H

#include "ompi_config.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"

#include "pml_dr.h"
#include "pml_dr_hdr.h"
#include "pml_dr_vfrag.h"
#include "pml_dr_comm.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_dr_recv_request_t {
    mca_pml_base_recv_request_t req_recv;
    size_t  req_bytes_received;
    size_t  req_bytes_delivered;
    bool    req_acked;

    /* filled in after match */
    struct mca_pml_dr_comm_proc_t* req_proc;
    struct mca_pml_dr_endpoint_t* req_endpoint;
    opal_mutex_t* req_mutex;

    /* vfrag state */
    mca_pml_dr_vfrag_t *req_vfrag;
    mca_pml_dr_vfrag_t req_vfrag0;
    opal_list_t  req_vfrags;
};
typedef struct mca_pml_dr_recv_request_t mca_pml_dr_recv_request_t;


OBJ_CLASS_DECLARATION(mca_pml_dr_recv_request_t);


/**
 *  Allocate a recv request from the modules free list.
 *
 *  @param rc (OUT)  OMPI_SUCCESS or error status on failure.
 *  @return          Receive request.
 */
#define MCA_PML_DR_RECV_REQUEST_ALLOC(recvreq, rc)                 \
do {                                                               \
   ompi_free_list_item_t* item;                                    \
   rc = OMPI_SUCCESS;                                              \
   OMPI_FREE_LIST_GET(&mca_pml_base_recv_requests, item, rc);      \
   recvreq = (mca_pml_dr_recv_request_t*)item;                     \
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
#define MCA_PML_DR_RECV_REQUEST_INIT(                              \
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
 *  Mark a recv request complete.
 *
 *  @param request (IN)  Receive request.
 */

#define MCA_PML_DR_RECV_REQUEST_PML_COMPLETE(recvreq)                                  \
do {                                                                                   \
    ompi_free_list_item_t* item;                                                       \
    assert( false == recvreq->req_recv.req_base.req_pml_complete );                    \
    OPAL_THREAD_LOCK((recvreq)->req_mutex);                                            \
    while(NULL != (item = (ompi_free_list_item_t*)                                     \
          opal_list_remove_first(&(recvreq)->req_vfrags))) {                           \
        OMPI_FREE_LIST_RETURN(&mca_pml_dr.vfrags, item);                               \
    }                                                                                  \
    OPAL_THREAD_UNLOCK((recvreq)->req_mutex);                                          \
                                                                                       \
    opal_list_remove_item(&(recvreq)->req_proc->matched_receives,                      \
                          (opal_list_item_t*)(recvreq));                               \
                                                                                       \
    /* initialize request status */                                                    \
    recvreq->req_recv.req_base.req_pml_complete = true;                                \
    if (recvreq->req_bytes_received > recvreq->req_bytes_delivered) {   \
        recvreq->req_recv.req_base.req_ompi.req_status._count =         \
            recvreq->req_bytes_delivered;                               \
        recvreq->req_recv.req_base.req_ompi.req_status.MPI_ERROR =      \
            MPI_ERR_TRUNCATE;                                           \
    } else {                                                            \
        recvreq->req_recv.req_base.req_ompi.req_status._count =         \
            recvreq->req_bytes_received;                                \
    }                                                                   \
    MCA_PML_BASE_REQUEST_MPI_COMPLETE( &(recvreq->req_recv.req_base.req_ompi) );       \
                                                                                       \
    if( true == recvreq->req_recv.req_base.req_free_called ) {                         \
        MCA_PML_DR_RECV_REQUEST_RETURN( recvreq );                                     \
    }                                                                                  \
} while(0)


/**
 *  Return a recv request to the modules free list.
 *
 *  @param request (IN)  Receive request.
 */
#define MCA_PML_DR_RECV_REQUEST_RETURN(recvreq)                                     \
do {                                                                                \
    /* decrement reference counts */                                                \
    MCA_PML_BASE_RECV_REQUEST_FINI(&(recvreq)->req_recv);                           \
    OMPI_FREE_LIST_RETURN(&mca_pml_base_recv_requests,                              \
                          (ompi_free_list_item_t*)(recvreq)); \
} while(0)

/**
 * Attempt to match the request against the unexpected fragment list
 * for all source ranks w/in the communicator.
 *
 * @param request (IN)   Request to match.
 */
void mca_pml_dr_recv_request_match_wild(mca_pml_dr_recv_request_t* request);

/**
 * Attempt to match the request against the unexpected fragment list
 * for a specific source rank.
 *
 * @param request (IN)   Request to match.
 */
void mca_pml_dr_recv_request_match_specific(mca_pml_dr_recv_request_t* request);


/**
 * Ack a matched request.
 */
void mca_pml_dr_recv_request_ack(
                                 mca_btl_base_module_t* blt,
                                 mca_pml_dr_recv_request_t* recvreq,
                                 mca_pml_dr_common_hdr_t* hdr,
                                 ompi_ptr_t src_ptr,
                                 size_t vlen,
                                 uint64_t vmask);

/**
 * Start an initialized request.
 *
 * @param request  Receive request.
 * @return         OMPI_SUCESS or error status on failure.
 */
#define MCA_PML_DR_RECV_REQUEST_START(request)                                    \
do {                                                                              \
    /* init/re-init the request */                                                \
    (request)->req_bytes_received = 0;                                            \
    (request)->req_bytes_delivered = 0;                                           \
    (request)->req_acked = false;                                                 \
    (request)->req_recv.req_base.req_pml_complete = false;                        \
    (request)->req_recv.req_base.req_ompi.req_complete = false;                   \
    (request)->req_recv.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;        \
    (request)->req_vfrag = &(request)->req_vfrag0;                                \
    (request)->req_proc = NULL;                                                   \
    (request)->req_endpoint = NULL;                                               \
                                                                                  \
    /* always set the req_status.MPI_TAG to ANY_TAG before starting the           \
     * request. This field is used if cancelled to find out if the request        \
     * has been matched or not.                                                   \
     */                                                                           \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_TAG = OMPI_ANY_TAG;      \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;    \
    (request)->req_recv.req_base.req_ompi.req_status._cancelled = 0;              \
                                                                                  \
    /* attempt to match unexpected recv */                                        \
    if((request)->req_recv.req_base.req_peer == OMPI_ANY_SOURCE) {                \
        mca_pml_dr_recv_request_match_wild(request);                              \
    } else {                                                                      \
        mca_pml_dr_recv_request_match_specific(request);                          \
    }                                                                             \
} while (0)


/**
 * Initialize request when match is made
 */

#define MCA_PML_DR_RECV_REQUEST_MATCHED(request,comm,proc,hdr)                       \
do {                                                                                 \
    (request)->req_mutex = &comm->matching_lock;                                     \
    (request)->req_proc = proc;                                                      \
    (request)->req_endpoint = (mca_pml_dr_endpoint_t*)proc->ompi_proc->proc_pml;     \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_TAG = (hdr)->hdr_tag;       \
    (request)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE =  proc->comm_rank;  \
    (request)->req_vfrag0.vf_id = (hdr)->hdr_common.hdr_vid;                         \
    opal_list_append(&proc->matched_receives, (opal_list_item_t*)request);           \
    ompi_seq_tracker_insert(&request->req_endpoint->seq_recvs_matched,               \
                           (hdr)->hdr_common.hdr_vid);                               \
} while(0)


/**
 * Setup convertor if message length is non-zero
 */

#define MCA_PML_DR_RECV_REQUEST_BYTES_PACKED(request, bytes_packed)                  \
do {                                                                                 \
    bool do_csum = mca_pml_dr.enable_csum &&                                         \
        (request->req_endpoint->bml_endpoint->btl_flags_or & MCA_BTL_FLAGS_NEED_CSUM); \
    (request)->req_recv.req_bytes_packed = bytes_packed;                             \
    if((request)->req_recv.req_bytes_packed != 0) {                                  \
        ompi_proc_t *proc = (request)->req_proc->ompi_proc;                          \
        ompi_convertor_copy_and_prepare_for_recv( proc->proc_convertor,              \
                                         (request)->req_recv.req_base.req_datatype,  \
                                         (request)->req_recv.req_base.req_count,     \
                                         (request)->req_recv.req_base.req_addr,      \
                                         (do_csum ? CONVERTOR_WITH_CHECKSUM: 0),     \
                                         &(request)->req_recv.req_base.req_convertor ); \
    }                                                                                \
} while (0)


/**
 *
 */

#define MCA_PML_DR_RECV_REQUEST_UNPACK(                                           \
    request,                                                                      \
    segments,                                                                     \
    num_segments,                                                                 \
    seg_offset,                                                                   \
    data_offset,                                                                  \
    bytes_received,                                                               \
    bytes_delivered,                                                              \
    csum)                                                                         \
do {                                                                              \
    if(request->req_recv.req_bytes_packed > 0) {                                  \
        struct iovec iov[MCA_BTL_DES_MAX_SEGMENTS];                               \
        uint32_t iov_count = 0;                                                   \
        size_t max_data = bytes_received;                                         \
        size_t n, offset = seg_offset;                                            \
        bool do_csum = mca_pml_dr.enable_csum &&                                  \
            (request->req_endpoint->bml_endpoint->btl_flags_or & MCA_BTL_FLAGS_NEED_CSUM); \
                                                                                  \
        for(n=0; n<num_segments; n++) {                                           \
            mca_btl_base_segment_t* segment = segments+n;                         \
            if(offset >= segment->seg_len) {                                      \
                offset -= segment->seg_len;                                       \
            } else {                                                              \
                iov[iov_count].iov_len = segment->seg_len - offset;               \
                iov[iov_count].iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval + offset); \
                offset = 0;                                                       \
                iov_count++;                                                      \
            }                                                                     \
        }                                                                         \
        ompi_convertor_set_position( &(request->req_recv.req_base.req_convertor), \
                                     &data_offset);                               \
        assert((request->req_recv.req_base.req_convertor.flags & CONVERTOR_COMPLETED) == 0); \
        ompi_convertor_unpack( &(request)->req_recv.req_base.req_convertor,       \
                               iov,                                               \
                               &iov_count,                                        \
                               &max_data);                                        \
        bytes_delivered = max_data;                                               \
        if(bytes_received && !bytes_delivered) assert(0);                         \
        csum = (do_csum ?                                                         \
                request->req_recv.req_base.req_convertor.checksum : OPAL_CSUM_ZERO); \
    } else {                                                                      \
        bytes_delivered = 0;                                                      \
        csum = OPAL_CSUM_ZERO;                                                    \
    }                                                                             \
} while (0)


/**
 *
 */

void mca_pml_dr_recv_request_progress(
    mca_pml_dr_recv_request_t* req,
    struct mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments);

/**
 *
 */

void mca_pml_dr_recv_request_matched_probe(
    mca_pml_dr_recv_request_t* req,
    struct mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments);

/**
 *
 */

void mca_pml_dr_recv_request_schedule(
    mca_pml_dr_recv_request_t* req);

/**
 *  Look for matched receive.
 *  Must be called w/ matching lock held.
 */
                                                                                                            
static inline struct mca_pml_dr_recv_request_t* mca_pml_dr_comm_proc_check_matched(
    mca_pml_dr_comm_proc_t* dr_proc,
    uint32_t vfrag_id)
{
    opal_list_item_t* item;
    for(item  = opal_list_get_first(&dr_proc->matched_receives);
        item != opal_list_get_end(&dr_proc->matched_receives);
        item  = opal_list_get_next(item)) {
        struct mca_pml_dr_recv_request_t* recvreq = (struct mca_pml_dr_recv_request_t*)item;
        if(recvreq->req_vfrag0.vf_id == vfrag_id)
            return recvreq;
    }
    return NULL;
}

/*
 *
 */

#define MCA_PML_DR_RECV_REQUEST_VFRAG_LOOKUP(recvreq,hdr,vfrag)               \
do {                                                                          \
   if((recvreq)->req_vfrag->vf_id == (hdr)->hdr_common.hdr_vid) {             \
       vfrag = (recvreq)->req_vfrag;                                          \
   } else {                                                                   \
       opal_list_item_t* item;                                                \
       int rc;                                                                \
                                                                              \
       vfrag = NULL;                                                          \
       OPAL_THREAD_LOCK(recvreq->req_mutex);                                  \
       for(item =  opal_list_get_first(&(recvreq)->req_vfrags);               \
           item != opal_list_get_end(&(recvreq)->req_vfrags);                 \
           item =  opal_list_get_next(item)) {                                \
           mca_pml_dr_vfrag_t* vf = (mca_pml_dr_vfrag_t*)item;                \
           if(vf->vf_id == (hdr)->hdr_common.hdr_vid) {                       \
               vfrag = vf;                                                    \
               break;                                                         \
           }                                                                  \
       }                                                                      \
       if(NULL == vfrag) {                                                    \
           MCA_PML_DR_VFRAG_ALLOC(vfrag,rc);                                  \
           if(NULL != vfrag) {                                                \
               MCA_PML_DR_VFRAG_INIT(vfrag);                                  \
               (vfrag)->vf_id = (hdr)->hdr_common.hdr_vid;                    \
               (vfrag)->vf_len = (hdr)->hdr_vlen;                             \
               if((hdr)->hdr_vlen == 64) {                                    \
                   (vfrag)->vf_mask = ~(uint64_t)0;                           \
               } else {                                                       \
                   (vfrag)->vf_mask = (((uint64_t)1 << (hdr)->hdr_vlen)-1);   \
               }                                                              \
               opal_list_append(&(recvreq)->req_vfrags, (opal_list_item_t*)vfrag); \
               (recvreq)->req_vfrag = vfrag;                            \
           }                                                                  \
       }                                                                      \
       OPAL_THREAD_UNLOCK(recvreq->req_mutex);                                \
   }                                                                          \
} while(0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif


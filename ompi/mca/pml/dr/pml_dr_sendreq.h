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
#ifndef OMPI_PML_DR_SEND_REQUEST_H
#define OMPI_PML_DR_SEND_REQUEST_H

#include "opal/util/crc.h"
#include "ompi_config.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/bml/bml.h" 

#include "pml_dr_proc.h"
#include "pml_dr_comm.h"
#include "pml_dr_hdr.h"
#include "pml_dr_vfrag.h"
#include "opal/event/event.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_dr_send_request_t {
    mca_pml_base_send_request_t req_send;
    /* ompi_proc_t* req_proc;  */
    mca_bml_base_endpoint_t* req_endpoint;
#if OMPI_HAVE_THREAD_SUPPORT
    volatile int32_t req_state;
    volatile int32_t req_lock;
#else
    int32_t req_state;
    int32_t req_lock;
#endif
    size_t req_pipeline_depth;
    size_t req_bytes_delivered;
    size_t req_send_offset;

    mca_pml_dr_vfrag_t* req_vfrag;
    mca_pml_dr_vfrag_t  req_vfrag0;
    opal_list_t         req_pending;
    opal_list_t         req_retrans;
    opal_mutex_t        req_mutex;
    size_t              req_num_acks;
    size_t              req_num_vfrags;
    
    
};
typedef struct mca_pml_dr_send_request_t mca_pml_dr_send_request_t;


OBJ_CLASS_DECLARATION(mca_pml_dr_send_request_t);


#define MCA_PML_DR_SEND_REQUEST_ALLOC(                                     \
    comm,                                                                  \
    dst,                                                                   \
    sendreq,                                                               \
    rc)                                                                    \
{                                                                          \
    ompi_proc_t *proc =                                                    \
         comm->c_pml_procs[dst]->proc_ompi;                                \
    opal_list_item_t* item;                                                \
                                                                           \
    if(NULL == proc) {                                                     \
        rc = OMPI_ERR_OUT_OF_RESOURCE;                                     \
    } else {                                                               \
        rc = OMPI_SUCCESS;                                                 \
        OMPI_FREE_LIST_WAIT(&mca_pml_dr.send_requests, item, rc);          \
        sendreq = (mca_pml_dr_send_request_t*)item;                        \
        sendreq->req_send.req_base.req_proc = proc;                        \
    }                                                                      \
}


#define MCA_PML_DR_SEND_REQUEST_INIT(                                      \
    sendreq,                                                               \
    buf,                                                                   \
    count,                                                                 \
    datatype,                                                              \
    dst,                                                                   \
    tag,                                                                   \
    comm,                                                                  \
    sendmode,                                                              \
    persistent)                                                            \
{                                                                          \
    MCA_PML_BASE_SEND_REQUEST_INIT(&sendreq->req_send,                     \
        buf,                                                               \
        count,                                                             \
        datatype,                                                          \
        dst,                                                               \
        tag,                                                               \
        comm,                                                              \
        sendmode,                                                          \
        persistent);                                                       \
}


/**
 * Start a send request. 
 */

#define MCA_PML_DR_SEND_REQUEST_START(sendreq, rc)                                        \
do {                                                                                      \
    mca_pml_dr_comm_t* comm = sendreq->req_send.req_base.req_comm->c_pml_comm;            \
    mca_pml_dr_comm_proc_t* proc = comm->procs + sendreq->req_send.req_base.req_peer;     \
    mca_bml_base_endpoint_t* endpoint =                                                   \
                 (mca_bml_base_endpoint_t*)sendreq->req_send.req_base.req_proc->proc_pml; \
    mca_bml_base_btl_t* bml_btl;                                                          \
    size_t size = sendreq->req_send.req_bytes_packed;                                     \
    size_t eager_limit;                                                                   \
    if(endpoint == NULL) {                                                                \
        rc = OMPI_ERR_UNREACH;                                                            \
        break;                                                                            \
    }                                                                                     \
                                                                                          \
    sendreq->req_lock = 0;                                                                \
    sendreq->req_pipeline_depth = 0;                                                      \
    sendreq->req_bytes_delivered = 0;                                                     \
    sendreq->req_state = 0;                                                               \
    sendreq->req_send_offset = 0;                                                         \
    sendreq->req_send.req_base.req_pml_complete = false;                                  \
    sendreq->req_send.req_base.req_ompi.req_complete = false;                             \
    sendreq->req_send.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;                  \
    sendreq->req_send.req_base.req_ompi.req_status._cancelled = 0;                        \
    sendreq->req_send.req_base.req_sequence = OPAL_THREAD_ADD32(&proc->send_sequence,1);  \
    sendreq->req_endpoint = endpoint;                                                     \
    sendreq->req_vfrag0.vf_id = OPAL_THREAD_ADD32(&proc->vfrag_id,1);                     \
    sendreq->req_vfrag = &sendreq->req_vfrag0;                                            \
    sendreq->req_num_acks = 0;                                                            \
    sendreq->req_num_vfrags = 0;                                                          \
                                                                                          \
    /* select a btl */                                                                    \
    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);                      \
    eager_limit = bml_btl->btl_eager_limit - sizeof(mca_pml_dr_hdr_t);                    \
    if(size <= eager_limit) {                                                             \
        switch(sendreq->req_send.req_send_mode) {                                         \
        case MCA_PML_BASE_SEND_SYNCHRONOUS:                                               \
            rc = mca_pml_dr_send_request_start_rndv(sendreq, bml_btl, size, 0);           \
            break;                                                                        \
        case MCA_PML_BASE_SEND_BUFFERED:                                                  \
            rc = mca_pml_dr_send_request_start_copy(sendreq, bml_btl, size);              \
            break;                                                                        \
        default:                                                                          \
            if(size == 0) {                                                               \
                mca_btl_base_descriptor_t* descriptor;                                    \
                mca_btl_base_segment_t* segment;                                          \
                mca_pml_dr_hdr_t* hdr;                                                    \
                /* allocate a descriptor */                                               \
                MCA_PML_DR_DES_ALLOC(bml_btl, descriptor, sizeof(mca_pml_dr_match_hdr_t));        \
                if(NULL == descriptor) {                                                          \
                    return OMPI_ERR_OUT_OF_RESOURCE;                                              \
                }                                                                                 \
                segment = descriptor->des_src;                                                    \
                /* setup vfrag */                                                                 \
                sendreq->req_vfrag0.vf_size = 0;                                                  \
                                                                                                  \
                /* build hdr */                                                                   \
                hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;                                  \
                hdr->hdr_common.hdr_flags = 0;                                                    \
                hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_MATCH;                             \
                hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;        \
                hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;          \
                hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;                      \
                hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;                 \
                hdr->hdr_match.hdr_src_req.pval = sendreq;                                        \
                hdr->hdr_match.hdr_vid = sendreq->req_vfrag0.vf_id;                               \
                hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_match_hdr_t));        \
                                                                                                  \
                /* short message */                                                               \
                descriptor->des_cbfunc = mca_pml_dr_match_completion_cache;                       \
                descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;                              \
                descriptor->des_cbdata = sendreq;                                                 \
                                                                                                  \
                /* request is complete at mpi level */                                            \
                OPAL_THREAD_LOCK(&ompi_request_lock);                                             \
                MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);                                    \
                OPAL_THREAD_UNLOCK(&ompi_request_lock);                                           \
                                                                                                  \
                /* send */                                                                        \
                rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);                     \
                if(OMPI_SUCCESS != rc) {                                                          \
                    mca_bml_base_free(bml_btl, descriptor );                                      \
                }                                                                                 \
                                                                                                  \
            } else if (bml_btl->btl_flags & MCA_BTL_FLAGS_SEND_INPLACE) {                         \
                rc = mca_pml_dr_send_request_start_prepare(sendreq, bml_btl, size);   \
            } else {                                                                  \
                rc = mca_pml_dr_send_request_start_copy(sendreq, bml_btl, size);      \
            }                                                                         \
            break;                                                                    \
        }                                                                             \
    } else {                                                                          \
        size = eager_limit;                                                           \
        if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {           \
            rc = mca_pml_dr_send_request_start_buffered(sendreq, bml_btl, size);      \
        } else {                                                                      \
            rc = mca_pml_dr_send_request_start_rndv(sendreq, bml_btl, size, 0);       \
        }                                                                             \
    }                                                                                 \
 } while (0)


/*
 * Mark a send request as completed at the MPI level.
 */

#define MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq)                                     \
do {                                                                                      \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_SOURCE =                          \
       (sendreq)->req_send.req_base.req_comm->c_my_rank;                                  \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_TAG =                             \
        (sendreq)->req_send.req_base.req_tag;                                             \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;             \
   (sendreq)->req_send.req_base.req_ompi.req_status._count =                              \
        (sendreq)->req_send.req_bytes_packed;                                             \
   (sendreq)->req_send.req_base.req_ompi.req_complete = true;                             \
   ompi_request_completed++;                                                              \
   if(ompi_request_waiting) {                                                             \
       opal_condition_broadcast(&ompi_request_cond);                                      \
   }                                                                                      \
} while(0)

/*
 * The PML has completed a send request. Note that this request
 * may have been orphaned by the user or have already completed
 * at the MPI level. 
 */
#define MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq)                                     \
do {                                                                                      \
    /* request completed at pml level */                                                  \
    (sendreq)->req_send.req_base.req_pml_complete = true;                                 \
                                                                                          \
    /* user has already released the request so simply free it */                         \
    if((sendreq)->req_send.req_base.req_free_called) {                                    \
        /* if buffered send - release any resources */                                    \
        if ((sendreq)->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&            \
            (sendreq)->req_send.req_addr != (sendreq)->req_send.req_base.req_addr) {      \
            mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);                    \
        }                                                                                 \
        MCA_PML_DR_SEND_REQUEST_RETURN(sendreq);                                          \
    /* is request complete at mpi level */                                                \
    } else if ((sendreq)->req_send.req_base.req_ompi.req_complete == false) {             \
        MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);                                    \
    /* buffered send - release any resources */                                           \
    } else if ((sendreq)->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&         \
               (sendreq)->req_send.req_addr != (sendreq)->req_send.req_base.req_addr) {   \
        mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);                        \
    }                                                                                     \
} while (0)


/*
 * Advance a pending send request. Note that the initial descriptor must complete
 * and the acknowledment received before the request can complete or be scheduled.
 * However, these events may occur in either order.
 */

#define MCA_PML_DR_SEND_REQUEST_ADVANCE(sendreq)                                          \
do {                                                                                      \
    bool schedule = false;                                                                \
                                                                                          \
    /* has an acknowledgment been received */                                             \
    if(OPAL_THREAD_ADD32(&sendreq->req_state, 1) == 2) {                                  \
        OPAL_THREAD_LOCK(&ompi_request_lock);                                             \
        if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {          \
            MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);                                \
        } else {                                                                          \
            schedule = true;                                                              \
        }                                                                                 \
        OPAL_THREAD_UNLOCK(&ompi_request_lock);                                           \
    }                                                                                     \
                                                                                          \
    /* additional data to schedule */                                                     \
    if(schedule == true) {                                                                \
        mca_pml_dr_send_request_schedule(sendreq);                                        \
    }                                                                                     \
} while (0)

/*
 * Release resources associated with a request
 */

#define MCA_PML_DR_SEND_REQUEST_RETURN(sendreq)                                         \
do {                                                                                    \
    /*  Let the base handle the reference counts */                                     \
    MCA_PML_BASE_SEND_REQUEST_FINI((&(sendreq)->req_send));                             \
    OMPI_FREE_LIST_RETURN(                                                              \
        &mca_pml_dr.send_requests, (opal_list_item_t*)sendreq);                         \
} while(0)

/*
 * Lookup/allocate a vfrag for the pending send
 */

#define MCA_PML_DR_SEND_REQUEST_VFRAG_INIT(sendreq, endpoint, size, vfrag)              \
do {                                                                                    \
    mca_pml_dr_comm_t* comm = sendreq->req_send.req_base.req_comm->c_pml_comm;          \
    mca_pml_dr_comm_proc_t* proc = comm->procs + sendreq->req_send.req_base.req_peer;   \
    size_t max_send_size = endpoint->btl_max_send_size - sizeof(mca_pml_dr_frag_hdr_t); \
    size_t div = size / max_send_size;                                                  \
                                                                                        \
    if(div == 0) {                                                                      \
        vfrag->vf_len = 1;                                                              \
        vfrag->vf_size = size;                                                          \
        vfrag->vf_mask = 1;                                                             \
    } else if(div > 64) {                                                               \
        vfrag->vf_len = 64;                                                             \
        vfrag->vf_size = (max_send_size << 6); /* size * 64 */                          \
        vfrag->vf_mask = ~(uint64_t)0;                                                  \
    } else if (div == 64) {                                                             \
        size_t mod = size % max_send_size;                                              \
        vfrag->vf_len = 64;                                                             \
        vfrag->vf_size = (mod ? (size - mod) : size);                                   \
        vfrag->vf_mask = ~(uint64_t)0;                                                  \
    } else {                                                                            \
        size_t mod = size % max_send_size;                                              \
        vfrag->vf_len = div + (mod ? 1 : 0);                                            \
        vfrag->vf_size = size;                                                          \
        vfrag->vf_mask = (((uint64_t)1 << vfrag->vf_len) - (uint64_t)1);                \
    }                                                                                   \
                                                                                        \
    vfrag->vf_mask_processed = 0;                                                       \
    vfrag->vf_id = OPAL_THREAD_ADD32(&proc->vfrag_id,1);                                \
    vfrag->vf_ack = 0;                                                                  \
    vfrag->vf_offset = sendreq->req_send_offset;                                        \
    vfrag->vf_idx = 0;                                                                  \
    vfrag->vf_max_send_size = max_send_size;                                            \
    opal_list_append(&sendreq->req_pending, (opal_list_item_t*)vfrag);                  \
    sendreq->req_vfrag = vfrag;                                                         \
} while(0)


/*
 *
 */
                                                                                                             
#define MCA_PML_DR_SEND_REQUEST_VFRAG_PENDING(sendreq,hdr,vfrag)                \
do {                                                                            \
   if ((hdr)->hdr_vid == (sendreq)->req_vfrag0.vf_id) {                         \
       vfrag = &(sendreq)->req_vfrag0;                                          \
   } else if((sendreq)->req_vfrag->vf_id == (hdr)->hdr_vid) {                   \
       vfrag = (sendreq)->req_vfrag;                                            \
       opal_list_remove_item(&(sendreq)->req_pending,(opal_list_item_t*)vfrag); \
   } else {                                                                     \
       opal_list_item_t* item;                                                  \
       vfrag = NULL;                                                            \
       OPAL_THREAD_LOCK(&(sendreq)->req_mutex);                                 \
       for(item =  opal_list_get_first(&(sendreq)->req_pending);                \
           item != opal_list_get_end(&(sendreq)->req_pending);                  \
           item =  opal_list_get_next(item)) {                                  \
           mca_pml_dr_vfrag_t* vf = (mca_pml_dr_vfrag_t*)item;                  \
           if(vf->vf_id == (hdr)->hdr_vid) {                                    \
               opal_list_remove_item(&(sendreq)->req_pending,item);             \
               vfrag = vf;                                                      \
               break;                                                           \
           }                                                                    \
       }                                                                        \
       OPAL_THREAD_UNLOCK(&(sendreq)->req_mutex);                               \
   }                                                                            \
} while(0)
                                                                                                             
/*
 *
 */

#define MCA_PML_DR_SEND_REQUEST_VFRAG_RETRANS(sendreq,hdr,vfrag)          \
do {                                                                      \
   opal_list_item_t* item;                                                \
   vfrag = NULL;                                                          \
   OPAL_THREAD_LOCK(&(sendreq)->req_mutex);                               \
   for(item =  opal_list_get_first(&(sendreq)->req_retrans);              \
       item != opal_list_get_end(&(sendreq)->req_retrans);                \
       item =  opal_list_get_next(item)) {                                \
       mca_pml_dr_vfrag_t* vf = (mca_pml_dr_vfrag_t*)item;                \
       if(vf->vf_id == (hdr)->hdr_vid) {                                  \
           vfrag = vf;                                                    \
           break;                                                         \
       }                                                                  \
   }                                                                      \
   OPAL_THREAD_UNLOCK(&(sendreq)->req_mutex);                             \
} while(0)
                                                                                                             

/*
 * Update bytes delivered on request based on supplied descriptor
 */

#define MCA_PML_DR_SEND_REQUEST_SET_BYTES_DELIVERED(sendreq, descriptor, hdrlen)    \
do {                                                                                \
   size_t i;                                                                        \
   mca_btl_base_segment_t* segments = descriptor->des_src;                          \
                                                                                    \
   for(i=0; i<descriptor->des_src_cnt; i++) {                                       \
       sendreq->req_bytes_delivered += segments[i].seg_len;                         \
   }                                                                                \
   sendreq->req_bytes_delivered -= hdrlen;                                          \
                                                                                    \
} while(0)
                                                                                                                          
/*
 * Attempt to process any pending requests
 */

#define MCA_PML_DR_SEND_REQUEST_PROCESS_PENDING()                     \
do {                                                                  \
    /* advance pending requests */                                    \
    while(opal_list_get_size(&mca_pml_dr.send_pending)) {             \
        mca_pml_dr_send_request_t* sendreq;                           \
        OPAL_THREAD_LOCK(&mca_pml_dr.lock);                           \
        sendreq = (mca_pml_dr_send_request_t*)                        \
            opal_list_remove_first(&mca_pml_dr.send_pending);         \
        OPAL_THREAD_UNLOCK(&mca_pml_dr.lock);                         \
        if(NULL == sendreq)                                           \
            break;                                                    \
        mca_pml_dr_send_request_schedule(sendreq);                    \
    }                                                                 \
} while (0)


/**
 *  Start the specified request
 */

int mca_pml_dr_send_request_start_buffered(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size);

int mca_pml_dr_send_request_start_copy(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size);

int mca_pml_dr_send_request_start_prepare(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size);

int mca_pml_dr_send_request_start_rndv(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size,
    int flags);

/**
 *  Schedule additional fragments 
 */
int mca_pml_dr_send_request_schedule(
    mca_pml_dr_send_request_t* sendreq);

int mca_pml_dr_send_request_reschedule(
    mca_pml_dr_send_request_t* sendreq,
    mca_pml_dr_vfrag_t* vfrag);

/**
 *  Acknowledgment of vfrag 
 */
void mca_pml_dr_send_request_acked(
    mca_pml_dr_send_request_t* sendreq,
    mca_pml_dr_ack_hdr_t*);

void mca_pml_dr_send_request_nacked(
    mca_pml_dr_send_request_t* sendreq,
    mca_pml_dr_ack_hdr_t*);

/**
 *  Completion callback on match header
 *  Cache descriptor.
 */
void mca_pml_dr_match_completion_cache(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status);

/**
 *  Completion callback on match header
 *  Free descriptor.
 */
void mca_pml_dr_match_completion_free(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status);

 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif


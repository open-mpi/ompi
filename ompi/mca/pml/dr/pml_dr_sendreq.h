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
    mca_pml_dr_comm_proc_t* req_proc;
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
    opal_list_t         req_retrans;
    mca_btl_base_descriptor_t* descriptor; /* descriptor for first frag, retransmission */
    
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


#define MCA_PML_DR_SEND_REQUEST_INIT(                                               \
    sendreq,                                                                        \
    addr,                                                                           \
    count,                                                                          \
    datatype,                                                                       \
    peer,                                                                           \
    tag,                                                                            \
    comm,                                                                           \
    sendmode,                                                                       \
    persistent)                                                                     \
do {                                                                                \
    /* increment reference counts */                                                \
    OBJ_RETAIN(comm);                                                               \
    OBJ_RETAIN(datatype);                                                           \
                                                                                    \
    OMPI_REQUEST_INIT(&(sendreq)->req_send.req_base.req_ompi, persistent);          \
    (sendreq)->req_send.req_addr = addr;                                            \
    (sendreq)->req_send.req_count = count;                                          \
    (sendreq)->req_send.req_datatype = datatype;                                    \
    (sendreq)->req_send.req_send_mode = sendmode;                                   \
    (sendreq)->req_send.req_base.req_addr = addr;                                   \
    (sendreq)->req_send.req_base.req_count = count;                                 \
    (sendreq)->req_send.req_base.req_datatype = datatype;                           \
    (sendreq)->req_send.req_base.req_peer = (int32_t)peer;                          \
    (sendreq)->req_send.req_base.req_tag = (int32_t)tag;                            \
    (sendreq)->req_send.req_base.req_comm = comm;                                   \
    (sendreq)->req_send.req_base.req_pml_complete = (persistent ? true : false);    \
    (sendreq)->req_send.req_base.req_free_called = false;                           \
    (sendreq)->req_send.req_base.req_ompi.req_status._cancelled = 0;                \
                                                                                    \
    /* initialize datatype convertor for this request */                            \
    if(count > 0) {                                                                 \
        /* We will create a convertor specialized for the        */                 \
        /* remote architecture and prepared with the datatype.   */                 \
        ompi_convertor_copy_and_prepare_for_send(                                   \
                            (sendreq)->req_send.req_base.req_proc->proc_convertor,  \
                            (sendreq)->req_send.req_base.req_datatype,              \
                            (sendreq)->req_send.req_base.req_count,                 \
                            (sendreq)->req_send.req_base.req_addr,                  \
                            CONVERTOR_WITH_CHECKSUM,                                \
                            &(sendreq)->req_send.req_convertor );                   \
        ompi_convertor_get_packed_size(&(sendreq)->req_send.req_convertor,          \
                                       &((sendreq)->req_send.req_bytes_packed) );   \
    } else {                                                                        \
         (sendreq)->req_send.req_bytes_packed = 0;                                  \
    }                                                                               \
} while(0)


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
    sendreq->req_proc = proc;                                                             \
    MCA_PML_DR_VFRAG_INIT(&sendreq->req_vfrag0);                                          \
    sendreq->req_vfrag0.vf_id = OPAL_THREAD_ADD32(&proc->vfrag_id,1);                     \
    sendreq->req_vfrag = &sendreq->req_vfrag0;                                            \
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
        case MCA_PML_BASE_SEND_COMPLETE:                                                  \
            rc = mca_pml_dr_send_request_start_prepare(sendreq, bml_btl, size);           \
            break;                                                                        \
        default:                                                                          \
            if (bml_btl->btl_flags & MCA_BTL_FLAGS_SEND_INPLACE) {                        \
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

#define MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq)                            \
do {                                                                              \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_SOURCE =                  \
       (sendreq)->req_send.req_base.req_comm->c_my_rank;                          \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_TAG =                     \
        (sendreq)->req_send.req_base.req_tag;                                     \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;     \
   (sendreq)->req_send.req_base.req_ompi.req_status._count =                      \
        (sendreq)->req_send.req_bytes_packed;                                     \
   MCA_PML_BASE_REQUEST_MPI_COMPLETE( &((sendreq)->req_send.req_base.req_ompi) ); \
} while(0)

/*
 * The request fini is responsible for releasing all ressources at the PML
 * level. It will never be called directly from the upper level, as it should
 * only be an internal call to the PML. However, in the case when the user
 * already lost the MPI reference to the request (MPI_Request_free was called)
 * fini should completely free the MPI request.
 */

#define MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq)                               \
do {                                                                                \
    assert( false == sendreq->req_send.req_base.req_pml_complete );                 \
    if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&            \
        sendreq->req_send.req_addr != sendreq->req_send.req_base.req_addr) {        \
        mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);                  \
    }                                                                               \
                                                                                    \
    OPAL_THREAD_LOCK(&ompi_request_lock);                                           \
    if( false == sendreq->req_send.req_base.req_ompi.req_complete ) {               \
        /* Should only be called for long messages (maybe synchronous) */           \
        MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);                              \
    }                                                                               \
    sendreq->req_send.req_base.req_pml_complete = true;                             \
                                                                                    \
    if( sendreq->req_send.req_base.req_free_called ) {                              \
        MCA_PML_DR_SEND_REQUEST_RETURN( sendreq );                                  \
    } else {                                                                        \
        if(sendreq->req_send.req_base.req_ompi.req_persistent) {                    \
            /* rewind convertor */                                                  \
            size_t offset = 0;                                                      \
            ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset); \
        }                                                                           \
    }                                                                               \
    OPAL_THREAD_UNLOCK(&ompi_request_lock);                                         \
} while (0)

/*
 * Release resources associated with a request
 */

#define MCA_PML_DR_SEND_REQUEST_RETURN(sendreq)                         \
    do {                                                                \
    /*  Let the base handle the reference counts */                     \
    MCA_PML_BASE_SEND_REQUEST_FINI((&(sendreq)->req_send));             \
    OMPI_FREE_LIST_RETURN( &mca_pml_dr.send_requests,                   \
                           (opal_list_item_t*)sendreq );                \
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
        if(vfrag->vf_len == 64)                                                         \
            vfrag->vf_mask = ~(uint64_t)0;                                              \
        else                                                                            \
            vfrag->vf_mask = (((uint64_t)1 << vfrag->vf_len) - (uint64_t)1);            \
    }                                                                                   \
                                                                                        \
    vfrag->vf_id = OPAL_THREAD_ADD32(&proc->vfrag_id,1);                                \
    vfrag->vf_offset = sendreq->req_send_offset;                                        \
    vfrag->vf_ack = 0;                                                                  \
    vfrag->vf_idx = 0;                                                                  \
    vfrag->vf_mask_pending = 0;                                                         \
    vfrag->vf_retrans = 0;                                                              \
    vfrag->vf_retry_cnt = 0;                                                            \
    vfrag->vf_max_send_size = max_send_size;                                            \
    vfrag->vf_send.pval = sendreq;                                                      \
    sendreq->req_vfrag = vfrag;                                                         \
} while(0)

/*
 * Update bytes delivered on request based on supplied descriptor
 */

#define MCA_PML_DR_SEND_REQUEST_SET_BYTES_DELIVERED(sendreq, vfrag, hdrlen)         \
do {                                                                                \
   sendreq->req_bytes_delivered += vfrag->vf_size;     \
} while(0)
/*
 * Attempt to process any pending requests
 */

#define MCA_PML_DR_SEND_REQUEST_PROCESS_PENDING()                     \
do {                                                                  \
    /* advance pending requests */                                    \
    while(opal_list_get_size(&mca_pml_dr.send_pending)) {             \
        mca_pml_dr_send_request_t* sendreq;                           \
        OPAL_THREAD_LOCK(&ompi_request_lock);                         \
        sendreq = (mca_pml_dr_send_request_t*)                        \
            opal_list_remove_first(&mca_pml_dr.send_pending);         \
        OPAL_THREAD_UNLOCK(&ompi_request_lock);                       \
        if(NULL == sendreq)                                           \
            break;                                                    \
        mca_pml_dr_send_request_schedule(sendreq);                    \
    }                                                                 \
} while (0)


/*
 * Requeue first fragment of message for retransmission 
 */

#define MCA_PML_DR_SEND_REQUEST_EAGER_RETRY(sendreq, vfrag)          \
do {                                                                 \
    mca_bml_base_endpoint_t* endpoint = sendreq->req_endpoint;       \
    mca_bml_base_btl_t* bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager); \
    mca_btl_base_descriptor_t *des_old, *des_new;                    \
    vfrag->vf_retry_cnt ++;                                          \
    if(vfrag->vf_retry_cnt > mca_pml_dr.timer_ack_max_count) {      \
        opal_output(0, "%s:%d,%s  retry count exceeded! FATAL", __FILE__, __LINE__, __func__);  \
        orte_errmgr.abort();                                         \
    }                                                                \
                                                                     \
    OPAL_OUTPUT((0, "%s:%d:%s, retransmitting eager\n", __FILE__, __LINE__, __func__)); \
    assert(sendreq->descriptor->des_src != NULL);                    \
    MCA_PML_DR_VFRAG_RESET(vfrag);                                   \
    des_old = sendreq->descriptor;                                   \
    mca_bml_base_alloc(bml_btl, &des_new, des_old->des_src->seg_len);\
    memcpy(des_new->des_src->seg_addr.pval, \
           des_old->des_src->seg_addr.pval, \
           des_old->des_src->seg_len);                               \
    des_new->des_flags = des_old->des_flags;                         \
    des_new->des_cbdata = des_old->des_cbdata;                       \
    des_new->des_cbfunc = des_old->des_cbfunc;                       \
    MCA_PML_DR_VFRAG_ACK_START(vfrag);                               \
    mca_bml_base_send(bml_btl, des_new, MCA_BTL_TAG_PML);            \
} while(0)        

/*
 * Requeue first fragment of message for retransmission 
 */

#define MCA_PML_DR_SEND_REQUEST_RNDV_PROBE(sendreq, vfrag)           \
do {                                                                 \
    mca_bml_base_endpoint_t* endpoint = sendreq->req_endpoint;       \
    mca_bml_base_btl_t* bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager); \
    mca_btl_base_descriptor_t *des_old, *des_new;                    \
    mca_pml_dr_hdr_t *hdr;                                           \
    vfrag->vf_retry_cnt ++;                                          \
    if(vfrag->vf_retry_cnt > mca_pml_dr.timer_ack_max_count) {      \
        opal_output(0, "%s:%d,%s  retry count exceeded! FATAL", __FILE__, __LINE__, __func__);  \
        orte_errmgr.abort();                                         \
    }                                                                \
                                                                     \
    opal_output(0, "%s:%d:%s, (re)transmitting rndv probe\n", __FILE__, __LINE__, __func__); \
    assert(sendreq->descriptor->des_src != NULL);                    \
    MCA_PML_DR_VFRAG_RESET(vfrag);                                   \
    mca_bml_base_alloc(bml_btl, &des_new,                            \
                       sizeof(mca_pml_dr_rendezvous_hdr_t));         \
    des_old = sendreq->descriptor;                                   \
    /* build hdr */                                                  \
    hdr = (mca_pml_dr_hdr_t*)des_new->des_src->seg_addr.pval;        \
    hdr->hdr_common.hdr_flags = 0;                                   \
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_RNDV;             \
    hdr->hdr_common.hdr_dst = sendreq->req_send.req_base.req_peer;   \
    hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid; \
    hdr->hdr_common.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;   \
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;                \
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;           \
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;                     \
    hdr->hdr_match.hdr_csum = OPAL_CSUM_ZERO;                                   \
    hdr->hdr_common.hdr_vid =  sendreq->req_vfrag0.vf_id;                       \
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;          \
    hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_rendezvous_hdr_t)); \
    des_new->des_flags = des_old->des_flags;                                    \
    des_new->des_cbdata = des_old->des_cbdata;                                  \
    des_new->des_cbfunc = des_old->des_cbfunc;                                  \
    OPAL_THREAD_UNLOCK(&ompi_request_lock);                                     \
    MCA_PML_DR_VFRAG_ACK_START(vfrag);                                          \
    mca_bml_base_send(bml_btl, des_new, MCA_BTL_TAG_PML);                       \
} while(0)        

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
 *  Acknowledgment of vfrags
 */
void mca_pml_dr_send_request_match_ack(
    mca_btl_base_module_t* btl,
    mca_pml_dr_ack_hdr_t*);

void mca_pml_dr_send_request_rndv_ack(
    mca_btl_base_module_t* btl,
    mca_pml_dr_ack_hdr_t*);

void mca_pml_dr_send_request_frag_ack(
    mca_btl_base_module_t* btl,
    mca_pml_dr_ack_hdr_t*);

 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif


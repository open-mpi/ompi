/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
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
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/bml/bml.h" 
#include "ompi/mca/btl/btl.h"

#include "pml_dr_comm.h"
#include "pml_dr_hdr.h"
#include "pml_dr_vfrag.h"
#include "pml_dr_endpoint.h"
#include "opal/mca/event/event.h"

BEGIN_C_DECLS

struct mca_pml_dr_send_request_t {
    mca_pml_base_send_request_t req_send;
    mca_pml_dr_comm_proc_t* req_proc;
    mca_pml_dr_endpoint_t* req_endpoint;
#if OPAL_HAVE_THREAD_SUPPORT
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
    mca_btl_base_descriptor_t* req_descriptor; /* descriptor for first frag, retransmission */
    
};
typedef struct mca_pml_dr_send_request_t mca_pml_dr_send_request_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_send_request_t);


#define MCA_PML_DR_SEND_REQUEST_ALLOC(                                     \
    comm,                                                                  \
    dst,                                                                   \
    sendreq,                                                               \
    rc)                                                                    \
{                                                                          \
    ompi_proc_t *proc = ompi_comm_peer_lookup( comm, dst );                \
    ompi_free_list_item_t* item;                                           \
                                                                           \
    if(NULL == proc) {                                                     \
        rc = OMPI_ERR_OUT_OF_RESOURCE;                                     \
    } else {                                                               \
        rc = OMPI_SUCCESS;                                                 \
        OMPI_FREE_LIST_WAIT(&mca_pml_base_send_requests, item, rc);        \
        sendreq = (mca_pml_dr_send_request_t*)item;                        \
        sendreq->req_send.req_base.req_proc = proc;                        \
        opal_list_append(&mca_pml_dr.send_active,                          \
                         (opal_list_item_t*) sendreq);                     \
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
    mca_bml_base_endpoint_t* endpoint =                                             \
        sendreq->req_send.req_base.req_proc->proc_bml;                              \
    bool do_csum = mca_pml_dr.enable_csum &&                                        \
        (endpoint->btl_flags_or & MCA_BTL_FLAGS_NEED_CSUM);                         \
    /* increment reference counts */                                                \
    OBJ_RETAIN(comm);                                                               \
    OBJ_RETAIN(datatype);                                                           \
                                                                                    \
    OMPI_REQUEST_INIT(&(sendreq)->req_send.req_base.req_ompi, persistent);          \
    (sendreq)->req_send.req_addr = addr;                                            \
    (sendreq)->req_send.req_send_mode = sendmode;                                   \
    (sendreq)->req_send.req_base.req_addr = addr;                                   \
    (sendreq)->req_send.req_base.req_count = count;                                 \
    (sendreq)->req_send.req_base.req_datatype = datatype;                           \
    (sendreq)->req_send.req_base.req_peer = (int32_t)peer;                          \
    (sendreq)->req_send.req_base.req_tag = (int32_t)tag;                            \
    (sendreq)->req_send.req_base.req_comm = comm;                                   \
    (sendreq)->req_send.req_base.req_pml_complete = OPAL_INT_TO_BOOL(persistent);   \
    (sendreq)->req_send.req_base.req_free_called = false;                           \
    (sendreq)->req_send.req_base.req_ompi.req_status._cancelled = 0;                \
                                                                                    \
    /* initialize datatype convertor for this request */                            \
/*     if(count > 0) {      */                                                      \
        /* We will create a convertor specialized for the        */                 \
        /* remote architecture and prepared with the datatype.   */                 \
        opal_convertor_copy_and_prepare_for_send(                                   \
                            (sendreq)->req_send.req_base.req_proc->proc_convertor,  \
                            &((sendreq)->req_send.req_base.req_datatype->super),    \
                            (sendreq)->req_send.req_base.req_count,                 \
                            (sendreq)->req_send.req_base.req_addr,                  \
                            (do_csum ? CONVERTOR_WITH_CHECKSUM: 0),                 \
                            &(sendreq)->req_send.req_base.req_convertor );          \
        opal_convertor_get_packed_size(&(sendreq)->req_send.req_base.req_convertor, \
                                       &((sendreq)->req_send.req_bytes_packed) );   \
   /*  } else {   */                                                                \
   /*       (sendreq)->req_send.req_bytes_packed = 0;   */                          \
   /*  }  */                                                                        \
} while(0)

#define MCA_PML_DR_SEND_REQUEST_START(sendreq, rc)                                        \
do {                                                                                      \
    mca_pml_dr_comm_t* comm = sendreq->req_send.req_base.req_comm->c_pml_comm;            \
    mca_pml_dr_endpoint_t* pml_endpoint =                                                 \
                 (mca_pml_dr_endpoint_t*)sendreq->req_send.req_base.req_proc->proc_pml;   \
    mca_bml_base_endpoint_t* bml_endpoint =                                               \
                 sendreq->req_send.req_base.req_proc->proc_bml;                           \
    mca_pml_dr_comm_proc_t* proc =                                                        \
                 comm->procs + sendreq->req_send.req_base.req_peer;                       \
    mca_bml_base_btl_t* bml_btl;                                                          \
    size_t size = sendreq->req_send.req_bytes_packed;                                     \
    size_t eager_limit;                                                                   \
    if(pml_endpoint == NULL || bml_endpoint == NULL) {                                    \
        rc = OMPI_ERR_UNREACH;                                                            \
        break;                                                                            \
    }                                                                                     \
                                                                                          \
    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);                  \
    MCA_PML_DR_VFRAG_INIT(&sendreq->req_vfrag0);                                          \
    sendreq->req_vfrag0.vf_id = OPAL_THREAD_ADD32(&pml_endpoint->vfrag_seq,1);            \
    sendreq->req_vfrag0.bml_btl = bml_btl;                                                \
    sendreq->req_vfrag = &sendreq->req_vfrag0;                                            \
    sendreq->req_endpoint = pml_endpoint;                                                 \
    assert(pml_endpoint->bml_endpoint == bml_endpoint);                                   \
    sendreq->req_proc = proc;                                                             \
                                                                                          \
    sendreq->req_lock = 0;                                                                \
    sendreq->req_pipeline_depth = 1;                                                      \
    sendreq->req_bytes_delivered = 0;                                                     \
    sendreq->req_state = 0;                                                               \
    sendreq->req_send_offset = 0;                                                         \
    sendreq->req_send.req_base.req_pml_complete = false;                                  \
    sendreq->req_send.req_base.req_ompi.req_complete = false;                             \
    sendreq->req_send.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;                  \
    sendreq->req_send.req_base.req_ompi.req_status._cancelled = 0;                        \
    sendreq->req_send.req_base.req_sequence = OPAL_THREAD_ADD32(&proc->send_sequence,1);  \
                                                                                          \
    /* select a btl */                                                                    \
    assert(bml_btl->btl->btl_eager_limit >= sizeof(mca_pml_dr_hdr_t));                    \
    eager_limit = bml_btl->btl->btl_eager_limit - sizeof(mca_pml_dr_hdr_t);               \
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

#define MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq)                             \
do {                                                                              \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_SOURCE =                  \
       (sendreq)->req_send.req_base.req_comm->c_my_rank;                          \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_TAG =                     \
        (sendreq)->req_send.req_base.req_tag;                                     \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;     \
   (sendreq)->req_send.req_base.req_ompi.req_status._ucount =                     \
        (sendreq)->req_send.req_bytes_packed;                                     \
   ompi_request_complete( &((sendreq)->req_send.req_base.req_ompi), true );       \
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
    opal_list_remove_item(&mca_pml_dr.send_active,                                  \
                          (opal_list_item_t*) sendreq);                             \
    if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&            \
        sendreq->req_send.req_addr != sendreq->req_send.req_base.req_addr) {        \
        mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);                  \
    }                                                                               \
                                                                                    \
    if( false == sendreq->req_send.req_base.req_ompi.req_complete ) {               \
        /* Should only be called for long messages (maybe synchronous) */           \
        MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);                              \
    }                                                                               \
    sendreq->req_send.req_base.req_pml_complete = true;                             \
                                                                                    \
    if( sendreq->req_send.req_base.req_free_called ) {                              \
        MCA_PML_DR_SEND_REQUEST_RETURN( sendreq );                                  \
    } else {                                                                        \
        if(sendreq->req_send.req_base.req_ompi.req_persistent &&                    \
           (0 != sendreq->req_send.req_bytes_packed) ) {                            \
            /* rewind convertor */                                                  \
            size_t offset = 0;                                                      \
            opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor,  \
                                        &offset);                                   \
        }                                                                           \
    }                                                                               \
} while (0)

/*
 * Release resources associated with a request
 */

#define MCA_PML_DR_SEND_REQUEST_RETURN(sendreq)                         \
    do {                                                                \
    /*  Let the base handle the reference counts */                     \
    MCA_PML_BASE_SEND_REQUEST_FINI((&(sendreq)->req_send));             \
    OMPI_FREE_LIST_RETURN( &mca_pml_base_send_requests,                 \
                           (ompi_free_list_item_t*)sendreq );           \
} while(0)

/*
 * Lookup/allocate a vfrag for the pending send
 */

#define MCA_PML_DR_SEND_REQUEST_VFRAG_INIT(sendreq, pml_endpoint, size, vfrag) \
do {                                                                                    \
    size_t max_send_size = pml_endpoint->bml_endpoint->btl_max_send_size -              \
                             sizeof(mca_pml_dr_frag_hdr_t);                             \
    size_t div = size / max_send_size;                                                  \
                                                                                        \
    MCA_PML_DR_VFRAG_INIT(vfrag);                                                       \
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
    vfrag->vf_id = OPAL_THREAD_ADD32(&pml_endpoint->vfrag_seq,1);                       \
    vfrag->vf_offset = sendreq->req_send_offset;                                        \
    vfrag->vf_max_send_size = max_send_size;                                            \
    vfrag->vf_send.pval = sendreq;                                                      \
    sendreq->req_vfrag = vfrag;                                         \
} while(0)

/*
 * Reschedule unacked fragments
 */

#define MCA_PML_DR_SEND_REQUEST_VFRAG_RETRANS(sendreq, vfrag)                 \
do {                                                                          \
    if(((vfrag)->vf_state & MCA_PML_DR_VFRAG_RETRANS) == 0) {                 \
        opal_list_append(&(sendreq)->req_retrans, (opal_list_item_t*)(vfrag));\
        (vfrag)->vf_state |= MCA_PML_DR_VFRAG_RETRANS;                        \
    }                                                                         \
    (vfrag)->vf_state &= ~MCA_PML_DR_VFRAG_NACKED;                            \
    (vfrag)->vf_idx = 0;                                                      \
} while(0)

/*
 * Update bytes delivered on request based on supplied descriptor
 */

#define MCA_PML_DR_SEND_REQUEST_SET_BYTES_DELIVERED(sendreq, vfrag, hdrlen)   \
do {                                                                          \
   sendreq->req_bytes_delivered += vfrag->vf_size;                            \
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
        opal_list_append(&mca_pml_dr.send_active,                     \
                         (opal_list_item_t*) sendreq);                \
        mca_pml_dr_send_request_schedule(sendreq);                    \
    }                                                                 \
} while (0)


/*
 * Requeue first fragment of message for retransmission 
 */

#define MCA_PML_DR_SEND_REQUEST_EAGER_RETRY(sendreq, vfrag)             \
    do {                                                                \
        mca_btl_base_descriptor_t *des_old, *des_new;                   \
        OPAL_THREAD_ADD64(&vfrag->vf_pending,1);                        \
        MCA_PML_DR_DEBUG(0,                                             \
                         (0, "%s:%d:%s: retransmitting eager\n",        \
                          __FILE__, __LINE__, __func__));               \
        assert(sendreq->req_descriptor->des_src != NULL);               \
                                                                        \
        OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,1);         \
        OPAL_THREAD_ADD64(&(vfrag)->vf_pending,1);                      \
        (vfrag)->vf_state &= ~MCA_PML_DR_VFRAG_NACKED;                  \
                                                                        \
        des_old = sendreq->req_descriptor;                              \
        mca_bml_base_alloc(vfrag->bml_btl, &des_new,                    \
                           MCA_BTL_NO_ORDER, des_old->des_src->seg_len, \
                           des_old->des_flags);                         \
        sendreq->req_descriptor = des_new;                              \
        memcpy(des_new->des_src->seg_addr.pval,                         \
               des_old->des_src->seg_addr.pval,                         \
               des_old->des_src->seg_len);                              \
        des_new->des_flags = des_old->des_flags;                        \
        des_new->des_cbdata = des_old->des_cbdata;                      \
        des_new->des_cbfunc = des_old->des_cbfunc;                      \
        mca_bml_base_send(vfrag->bml_btl, des_new, MCA_BTL_TAG_PML);    \
    } while(0)        

/*
 * Requeue first fragment of message for retransmission 
 */

#define MCA_PML_DR_SEND_REQUEST_RNDV_PROBE(sendreq, vfrag)           \
do {                                                                 \
    mca_pml_dr_endpoint_t* endpoint = sendreq->req_endpoint;         \
    mca_bml_base_btl_t* bml_btl =                                    \
        mca_bml_base_btl_array_get_next(&endpoint->bml_endpoint->btl_eager);  \
    mca_btl_base_descriptor_t *des_old, *des_new;                    \
    mca_pml_dr_hdr_t *hdr;                                           \
    bool do_csum = mca_pml_dr.enable_csum &&                         \
        (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM);              \
    MCA_PML_DR_DEBUG(0,(0, "%s:%d:%s: (re)transmitting rndv probe\n",   \
                        __FILE__, __LINE__, __func__));                 \
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,1);          \
    OPAL_THREAD_ADD64(&vfrag->vf_pending,1);                         \
    (vfrag)->vf_state &= ~MCA_PML_DR_VFRAG_NACKED;                   \
                                                                     \
    assert(sendreq->req_descriptor->des_src != NULL);                \
    des_old = sendreq->req_descriptor;                               \
    mca_bml_base_alloc(bml_btl, &des_new,                            \
                       MCA_BTL_NO_ORDER,                             \
                       sizeof(mca_pml_dr_rendezvous_hdr_t),          \
                       des_old->des_flags);                          \
    /* build hdr */                                                  \
    hdr = (mca_pml_dr_hdr_t*)des_new->des_src->seg_addr.pval;        \
    hdr->hdr_common.hdr_flags = 0;                                   \
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_RNDV;             \
    hdr->hdr_common.hdr_dst = endpoint->dst;                         \
    hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid; \
    hdr->hdr_common.hdr_src = endpoint->src;                                    \
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;                \
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;           \
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;                     \
    hdr->hdr_match.hdr_csum = OPAL_CSUM_ZERO;                                   \
    hdr->hdr_common.hdr_vid =  sendreq->req_vfrag0.vf_id;                       \
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;          \
    hdr->hdr_common.hdr_csum = (do_csum ?                                       \
          opal_csum(hdr, sizeof(mca_pml_dr_rendezvous_hdr_t)): OPAL_CSUM_ZERO); \
    des_new->des_flags = des_old->des_flags;                                    \
    des_new->des_cbdata = des_old->des_cbdata;                                  \
    des_new->des_cbfunc = des_old->des_cbfunc;                                  \
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

void mca_pml_dr_sendreq_cleanup_active(mca_btl_base_module_t* btl);

 
END_C_DECLS
#endif


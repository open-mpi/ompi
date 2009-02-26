/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "ompi_config.h"
#include "opal/util/crc.h"

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h" 
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_dr_comm.h"
#include "pml_dr_recvreq.h"
#include "pml_dr_recvfrag.h"
#include "pml_dr_sendreq.h"
#include "ompi/mca/bml/base/base.h" 


/* 
 * this macro is needed for MATCH/RNDV headers, 
 *  as we need to put the match back on the list if the checksum 
 *  fails for later matching 
 */ 
#define MCA_PML_DR_RECV_REQUEST_MATCH_ACK(btl,do_csum,recvreq,hdr,csum,bytes_received) \
if(do_csum && csum != hdr->hdr_match.hdr_csum) {                     \
    /* failed the csum, put the request back on the list for         \
     * matching later on retransmission                              \
     */                                                              \
    if(recvreq->req_recv.req_base.req_peer == OMPI_ANY_SOURCE) {     \
        mca_pml_dr_recv_request_match_wild(recvreq);                 \
    } else {                                                         \
        mca_pml_dr_recv_request_match_specific(recvreq);             \
    }                                                                \
    mca_pml_dr_recv_frag_ack(btl,                                    \
                             recvreq->req_endpoint->bml_endpoint,    \
                             &hdr->hdr_common,                       \
                             hdr->hdr_match.hdr_src_ptr.pval,        \
                             0, 0);                                  \
    MCA_PML_DR_DEBUG(0,(0, "%s:%d: [rank %d -> rank %d] "            \
                        "data checksum failed 0x%08x != 0x%08x\n",   \
                        __FILE__, __LINE__,                          \
                        hdr->hdr_common.hdr_src, hdr->hdr_common.hdr_dst, \
                        csum, hdr->hdr_match.hdr_csum));                \
    bytes_received = bytes_delivered = 0;                            \
} else if (recvreq->req_acked == false) {                            \
    MCA_PML_DR_DEBUG(1,(0, "%s:%d: sending ack, vfrag ID %d",           \
                        __FILE__, __LINE__, recvreq->req_vfrag0.vf_id)); \
    mca_pml_dr_recv_request_ack(btl, recvreq, &hdr->hdr_common,         \
                                hdr->hdr_match.hdr_src_ptr, bytes_received, 1); \
}


static mca_pml_dr_recv_frag_t* mca_pml_dr_recv_request_match_specific_proc(
    mca_pml_dr_recv_request_t* request, mca_pml_dr_comm_proc_t* proc);


static int mca_pml_dr_recv_request_free(struct ompi_request_t** request)
{
    mca_pml_dr_recv_request_t* recvreq = *(mca_pml_dr_recv_request_t**)request;
    assert( false == recvreq->req_recv.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_recv.req_base.req_free_called = true;
    if( true == recvreq->req_recv.req_base.req_pml_complete ) {
        MCA_PML_DR_RECV_REQUEST_RETURN( recvreq );
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_dr_recv_request_cancel(struct ompi_request_t* ompi_request, int complete)
{
    mca_pml_dr_recv_request_t* request = (mca_pml_dr_recv_request_t*)ompi_request;
    mca_pml_dr_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;

    if( true == ompi_request->req_complete ) { /* way to late to cancel this one */
       return OMPI_SUCCESS;
    }

    /* The rest should be protected behind the match logic lock */
    OPAL_THREAD_LOCK(&comm->matching_lock);
    if( OMPI_ANY_TAG == ompi_request->req_status.MPI_TAG ) { /* the match has not been already done */
       if( request->req_recv.req_base.req_peer == OMPI_ANY_SOURCE ) {
          opal_list_remove_item( &comm->wild_receives, (opal_list_item_t*)request );
       } else {
           mca_pml_dr_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
           opal_list_remove_item(&proc->specific_receives, (opal_list_item_t*)request);
       }
    }
    OPAL_THREAD_UNLOCK(&comm->matching_lock);

    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request->req_status._cancelled = true;
    /* This macro will set the req_complete to true so the MPI Test/Wait* functions
     * on this request will be able to complete. As the status is marked as
     * cancelled the cancel state will be detected.
     */
    ompi_request_complete(ompi_request, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    return OMPI_SUCCESS;
}

static void mca_pml_dr_recv_request_construct(mca_pml_dr_recv_request_t* request)
{
    OBJ_CONSTRUCT(&request->req_vfrag0, mca_pml_dr_vfrag_t);
    OBJ_CONSTRUCT(&request->req_vfrags, opal_list_t);

    request->req_vfrag0.vf_len = 1;
    request->req_vfrag0.vf_mask = 1;
    request->req_vfrag0.vf_recv.pval = request;
    request->req_recv.req_base.req_type = MCA_PML_REQUEST_RECV;
    request->req_recv.req_base.req_ompi.req_free = mca_pml_dr_recv_request_free;
    request->req_recv.req_base.req_ompi.req_cancel = mca_pml_dr_recv_request_cancel;
}

static void mca_pml_dr_recv_request_destruct(mca_pml_dr_recv_request_t* request)
{
    OBJ_DESTRUCT(&request->req_vfrag0);
    OBJ_DESTRUCT(&request->req_vfrags);
}


OBJ_CLASS_INSTANCE(
    mca_pml_dr_recv_request_t,
    mca_pml_base_recv_request_t,
    mca_pml_dr_recv_request_construct,
    mca_pml_dr_recv_request_destruct);


/*
 * Release resources.
 */

static void mca_pml_dr_ctl_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_bml_base_free(bml_btl, des);
}

/*
 * Generate an ack to the peer.
 */

void mca_pml_dr_recv_request_ack(
                                 mca_btl_base_module_t* btl,
                                 mca_pml_dr_recv_request_t* recvreq,
                                 mca_pml_dr_common_hdr_t* hdr,
                                 ompi_ptr_t src_ptr,
                                 size_t vlen,
                                 uint64_t mask)
{
    mca_btl_base_descriptor_t* des;
    mca_bml_base_btl_t* bml_btl;
    mca_pml_dr_ack_hdr_t* ack;
    int rc;
    bool do_csum;
    
    
    /* use the same BTL for ACK's makes failover SANE */
    bml_btl = mca_bml_base_btl_array_find(&recvreq->req_endpoint->bml_endpoint->btl_eager,
                                          btl);
    /* allocate descriptor */
    do_csum = mca_pml_dr.enable_csum && 
        (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM);
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
            sizeof(mca_pml_dr_ack_hdr_t), MCA_BTL_DES_FLAGS_PRIORITY);
    if(NULL == des) {
        return;
    }

    /* fill out header */
    ack = (mca_pml_dr_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_ACK | hdr->hdr_type;
    ack->hdr_common.hdr_flags = 0;
    ack->hdr_common.hdr_dst = recvreq->req_endpoint->dst;
    ack->hdr_common.hdr_src = recvreq->req_endpoint->src;
    ack->hdr_common.hdr_ctx = recvreq->req_recv.req_base.req_comm->c_contextid;
    ack->hdr_common.hdr_vid = hdr->hdr_vid;
    ack->hdr_vlen = vlen;
    ack->hdr_vmask = mask;
    ack->hdr_src_ptr = src_ptr;
    ack->hdr_dst_ptr.pval = recvreq;
    ack->hdr_common.hdr_csum = (uint16_t)(do_csum? 
                                opal_csum(ack, sizeof(mca_pml_dr_ack_hdr_t)) :
                                OPAL_CSUM_ZERO);

    /* initialize descriptor */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbfunc = mca_pml_dr_ctl_completion;

    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(rc != OMPI_SUCCESS) {
        mca_bml_base_free(bml_btl, des);
    } else {
        recvreq->req_acked = true;
    }
}


 /*
  * Update the recv request status to reflect the number of bytes
  * received and actually delivered to the application. 
  */

void mca_pml_dr_recv_request_progress(
     mca_pml_dr_recv_request_t* recvreq,
     mca_btl_base_module_t* btl,
     mca_btl_base_segment_t* segments,
     size_t num_segments)
{
     size_t bytes_received = 0;
     size_t bytes_delivered = 0;
     size_t data_offset = 0;
     mca_pml_dr_hdr_t* hdr = (mca_pml_dr_hdr_t*)segments->seg_addr.pval;
     size_t i;
     uint32_t csum = OPAL_CSUM_ZERO;
     uint64_t bit;
     mca_pml_dr_vfrag_t* vfrag;
     bool do_csum = mca_pml_dr.enable_csum && 
         (btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM);

     
     for(i=0; i<num_segments; i++)
         bytes_received += segments[i].seg_len;

     switch(hdr->hdr_common.hdr_type) {
         case MCA_PML_DR_HDR_TYPE_MATCH:

             bytes_received -= sizeof(mca_pml_dr_match_hdr_t);
             recvreq->req_vfrag0.vf_send = hdr->hdr_match.hdr_src_ptr;
             MCA_PML_DR_RECV_REQUEST_BYTES_PACKED(recvreq, bytes_received);

             MCA_PML_DR_RECV_REQUEST_UNPACK(
                 recvreq,
                 segments,
                 num_segments,
                 sizeof(mca_pml_dr_match_hdr_t),
                 data_offset,
                 bytes_received,
                 bytes_delivered,
                 csum);
             MCA_PML_DR_RECV_REQUEST_MATCH_ACK(btl,do_csum, recvreq,hdr,csum,bytes_received);
                
             break;

         case MCA_PML_DR_HDR_TYPE_RNDV:

             bytes_received -= sizeof(mca_pml_dr_rendezvous_hdr_t);
             recvreq->req_vfrag0.vf_send = hdr->hdr_match.hdr_src_ptr;
             MCA_PML_DR_RECV_REQUEST_BYTES_PACKED(recvreq, hdr->hdr_rndv.hdr_msg_length);
             MCA_PML_DR_RECV_REQUEST_UNPACK(
                 recvreq,
                 segments,
                 num_segments,
                 sizeof(mca_pml_dr_rendezvous_hdr_t),
                 data_offset,
                 bytes_received,
                 bytes_delivered,
                 csum);
             MCA_PML_DR_RECV_REQUEST_MATCH_ACK(btl,do_csum, recvreq,hdr,csum,bytes_received);
             
            break;

        case MCA_PML_DR_HDR_TYPE_FRAG:
            bit = ((uint64_t)1 << hdr->hdr_frag.hdr_frag_idx); 
            MCA_PML_DR_RECV_REQUEST_VFRAG_LOOKUP(recvreq, &hdr->hdr_frag, vfrag);
            if(vfrag->vf_ack & bit) { 
                if(vfrag->vf_ack == vfrag->vf_mask) { 
                    MCA_PML_DR_DEBUG(1,(0, "%s:%d: sending ack, vfrag ID %d",
                                        __FILE__, __LINE__, vfrag->vf_id));
                    mca_pml_dr_recv_request_ack(btl,
                                                recvreq, &hdr->hdr_common, 
                                                hdr->hdr_frag.hdr_src_ptr, 
                                                vfrag->vf_size, 
                                                vfrag->vf_mask);
                }
                return;
            }
            bytes_received -= sizeof(mca_pml_dr_frag_hdr_t);
            data_offset = hdr->hdr_frag.hdr_frag_offset;
            
            MCA_PML_DR_RECV_REQUEST_UNPACK(
                                           recvreq,
                                           segments,
                                           num_segments,
                                           sizeof(mca_pml_dr_frag_hdr_t),
                                           data_offset,
                                           bytes_received,
                                           bytes_delivered,
                                           csum);
            
            /* update the mask to show that this vfrag was received, 
             * note that it might still fail the checksum though 
             */
            vfrag->vf_pending |= bit;
            if(!do_csum || csum == hdr->hdr_frag.hdr_frag_csum) { 
                /* this part of the vfrag passed the checksum, 
                   mark it so that we ack it after receiving the 
                   entire vfrag */
                vfrag->vf_ack |= bit;
                if((vfrag->vf_pending & vfrag->vf_mask) == vfrag->vf_mask) { 
                    /* we have received all the pieces of the vfrag, ack 
                       everything that passed the checksum */ 
                    ompi_seq_tracker_insert(&recvreq->req_endpoint->seq_recvs, vfrag->vf_id);
                    MCA_PML_DR_DEBUG(1,(0, "%s:%d: sending ack, vfrag ID %d",
                                        __FILE__, __LINE__, vfrag->vf_id));
                    mca_pml_dr_recv_request_ack(btl, recvreq, &hdr->hdr_common, 
                                                hdr->hdr_frag.hdr_src_ptr, 
                                                vfrag->vf_size, vfrag->vf_mask);
                }
            } else {
                MCA_PML_DR_DEBUG(0,(0, "%s:%d received corrupted fragment 0x%08x != 0x%08x\n",
                    __FILE__,__LINE__,csum,hdr->hdr_frag.hdr_frag_csum));
                bytes_received = bytes_delivered = 0;
            }
            break;

        default:
            break;
    }

    /* check completion status */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_bytes_received += bytes_received;
    recvreq->req_bytes_delivered += bytes_delivered;
    if (recvreq->req_bytes_received == recvreq->req_recv.req_bytes_packed) {
        MCA_PML_DR_RECV_REQUEST_PML_COMPLETE(recvreq);
    } 
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}


/**
 * Handle completion of a probe request
 */

void mca_pml_dr_recv_request_matched_probe(
    mca_pml_dr_recv_request_t* recvreq,
    mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments)
{
    size_t bytes_packed = 0;
    mca_pml_dr_hdr_t* hdr = (mca_pml_dr_hdr_t*)segments->seg_addr.pval;
    size_t i;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_DR_HDR_TYPE_MATCH:

            for(i=0; i<num_segments; i++)
                bytes_packed += segments[i].seg_len;
            bytes_packed -= sizeof(mca_pml_dr_match_hdr_t);
            break;

        case MCA_PML_DR_HDR_TYPE_RNDV:

            bytes_packed = hdr->hdr_rndv.hdr_msg_length;
            break;
    }

    /* check completion status */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_TAG = hdr->hdr_match.hdr_tag;
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE = recvreq->req_proc->comm_rank;
    recvreq->req_bytes_received = bytes_packed;
    recvreq->req_bytes_delivered = bytes_packed;
    
    ompi_request_completed++;
    if(ompi_request_waiting) {
        opal_condition_broadcast(&ompi_request_cond);
    }

    /* mark probe request completed */
    MCA_PML_DR_RECV_REQUEST_PML_COMPLETE(recvreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}

/*
 * This routine is used to match a posted receive when the source process 
 * is specified.
*/

void mca_pml_dr_recv_request_match_specific(mca_pml_dr_recv_request_t* request)
{
    mca_pml_dr_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_dr_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
    mca_pml_dr_recv_frag_t* frag;
   
    /* check for a specific match */
    OPAL_THREAD_LOCK(&comm->matching_lock);

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    if (opal_list_get_size(&proc->unexpected_frags) > 0 &&
        (frag = mca_pml_dr_recv_request_match_specific_proc(request, proc)) != NULL) {
        /* has the request already been matched */
        if(frag->hdr.hdr_common.hdr_type == MCA_PML_DR_HDR_TYPE_MATCH) 
            request->req_acked = true;
        MCA_PML_DR_RECV_REQUEST_MATCHED(request,comm,proc,&frag->hdr.hdr_match);
        OPAL_THREAD_UNLOCK(&comm->matching_lock);
        
        if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
              (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
            mca_pml_dr_recv_request_progress(request,frag->btl,frag->segments,frag->num_segments);
            MCA_PML_DR_RECV_FRAG_RETURN(frag);
        } else {
            mca_pml_dr_recv_request_matched_probe(request,frag->btl,frag->segments,frag->num_segments);
        }
        return; /* match found */
    }

    /* We didn't find any matches.  Record this irecv so we can match 
     * it when the message comes in.
    */
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE) { 
        opal_list_append(&proc->specific_receives, (opal_list_item_t*)request);
    }
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
}


/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/

void mca_pml_dr_recv_request_match_wild(mca_pml_dr_recv_request_t* request)
{
    mca_pml_dr_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_dr_comm_proc_t* proc = comm->procs;
    size_t proc_count = comm->num_procs;
    size_t i;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
    */
    OPAL_THREAD_LOCK(&comm->matching_lock);

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    for (i = 0; i < proc_count; i++) {
        mca_pml_dr_recv_frag_t* frag;

        /* continue if no frags to match */
        if (opal_list_get_size(&proc->unexpected_frags) == 0) {
            proc++;
            continue;
        }

        /* loop over messages from the current proc */
        if ((frag = mca_pml_dr_recv_request_match_specific_proc(request, proc)) != NULL) {
            /* has the request already been matched */
            if(frag->hdr.hdr_common.hdr_type == MCA_PML_DR_HDR_TYPE_MATCH) 
                request->req_acked = true;
            MCA_PML_DR_RECV_REQUEST_MATCHED(request,comm,proc,&frag->hdr.hdr_match);
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
                  (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
                mca_pml_dr_recv_request_progress(request,frag->btl,frag->segments,frag->num_segments);
                MCA_PML_DR_RECV_FRAG_RETURN(frag);
            } else {
                mca_pml_dr_recv_request_matched_probe(request,frag->btl,frag->segments,frag->num_segments);
            }
            return; /* match found */
        }
        proc++;
    } 

    /* We didn't find any matches.  Record this irecv so we can match to
     * it when the message comes in.
    */
 
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE)
        opal_list_append(&comm->wild_receives, (opal_list_item_t*)request);
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
}


/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. This
 *  function has to be called with the communicator matching lock held.
*/

static mca_pml_dr_recv_frag_t* mca_pml_dr_recv_request_match_specific_proc(
    mca_pml_dr_recv_request_t* request, 
    mca_pml_dr_comm_proc_t* proc)
{
    opal_list_t* unexpected_frags = &proc->unexpected_frags;
    mca_pml_dr_recv_frag_t* frag;
    mca_pml_dr_match_hdr_t* hdr;
    int tag = request->req_recv.req_base.req_tag;

    if( OMPI_ANY_TAG == tag ) {
        for (frag =  (mca_pml_dr_recv_frag_t*)opal_list_get_first(unexpected_frags);
             frag != (mca_pml_dr_recv_frag_t*)opal_list_get_end(unexpected_frags);
             frag =  (mca_pml_dr_recv_frag_t*)opal_list_get_next(frag)) {
            hdr = &(frag->hdr.hdr_match);
            
            /* check first frag - we assume that process matching has been done already */
            if( hdr->hdr_tag >= 0 ) {
                goto find_fragment;
            } 
        }
    } else {
        for (frag =  (mca_pml_dr_recv_frag_t*)opal_list_get_first(unexpected_frags);
             frag != (mca_pml_dr_recv_frag_t*)opal_list_get_end(unexpected_frags);
             frag =  (mca_pml_dr_recv_frag_t*)opal_list_get_next(frag)) {
            hdr = &(frag->hdr.hdr_match);
            
            /* check first frag - we assume that process matching has been done already */
            if ( tag == hdr->hdr_tag ) {
                /* we assume that the tag is correct from MPI point of view (ie. >= 0 ) */
                goto find_fragment;
            } 
        }
    }
    return NULL;
 find_fragment:
    if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
          (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
        opal_list_remove_item(unexpected_frags, (opal_list_item_t*)frag);
        frag->request = request;
    } 
    return frag;
}



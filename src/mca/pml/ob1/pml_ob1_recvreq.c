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

#include "ompi_config.h"

#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/mpool/mpool.h" 
#include "pml_ob1_comm.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_sendreq.h"

                                                                                                               
static mca_pml_ob1_recv_frag_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, mca_pml_ob1_comm_proc_t* proc);


static int mca_pml_ob1_recv_request_fini(struct ompi_request_t** request)
{
    MCA_PML_OB1_FINI(request);
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_recv_request_free(struct ompi_request_t** request)
{
    MCA_PML_OB1_FREE(request);
    return OMPI_SUCCESS;
} 

static int mca_pml_ob1_recv_request_cancel(struct ompi_request_t* ompi_request, int complete)
{
    mca_pml_ob1_recv_request_t* request = (mca_pml_ob1_recv_request_t*)ompi_request;
    mca_pml_ob1_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;

    if( true == ompi_request->req_complete ) { /* way to late to cancel this one */
       return OMPI_SUCCESS;
    }
    
    /* The rest should be protected behind the match logic lock */
    OMPI_THREAD_LOCK(&comm->matching_lock);
    if( OMPI_ANY_TAG == ompi_request->req_status.MPI_TAG ) { /* the match has not been already done */
       if( request->req_recv.req_base.req_peer == OMPI_ANY_SOURCE ) {
          ompi_list_remove_item( &comm->wild_receives, (ompi_list_item_t*)request );
       } else {
          mca_pml_ob1_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
          ompi_list_remove_item(&proc->specific_receives, (ompi_list_item_t*)request);
       }
    }
    OMPI_THREAD_UNLOCK(&comm->matching_lock);
    
    OMPI_THREAD_LOCK(&ompi_request_lock);
    ompi_request->req_status._cancelled = true;
    ompi_request->req_complete = true;  /* mark it as completed so all the test/wait  functions
                                    * on this particular request will finish */
    /* Now we have a problem if we are in a multi-threaded environment. We shou ld
     * broadcast the condition on the request in order to allow the other threa ds
     * to complete their test/wait functions.
     */
    if(ompi_request_waiting) {
       ompi_condition_broadcast(&ompi_request_cond);
    }
    OMPI_THREAD_UNLOCK(&ompi_request_lock);
    return OMPI_SUCCESS;
}

static void mca_pml_ob1_recv_request_construct(mca_pml_ob1_recv_request_t* request)
{
    request->req_recv.req_base.req_type = MCA_PML_REQUEST_RECV;
    request->req_recv.req_base.req_ompi.req_fini = mca_pml_ob1_recv_request_fini;
    request->req_recv.req_base.req_ompi.req_free = mca_pml_ob1_recv_request_free;
    request->req_recv.req_base.req_ompi.req_cancel = mca_pml_ob1_recv_request_cancel;
}

static void mca_pml_ob1_recv_request_destruct(mca_pml_ob1_recv_request_t* request)
{
}


OBJ_CLASS_INSTANCE(
    mca_pml_ob1_recv_request_t,
    mca_pml_base_recv_request_t,
    mca_pml_ob1_recv_request_construct,
    mca_pml_ob1_recv_request_destruct);


/*
 * Release resources.
 */

static void mca_pml_ob1_send_ctl_complete(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_pml_ob1_endpoint_t* endpoint = (mca_pml_ob1_endpoint_t*)des->des_cbdata;
    MCA_PML_OB1_ENDPOINT_DES_RETURN(endpoint, des);
}


/*
 *
 */

static void mca_pml_ob1_recv_request_ack(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_pml_ob1_rendezvous_hdr_t* hdr)
{
    mca_pml_ob1_proc_t* proc = recvreq->req_proc;
    mca_pml_ob1_endpoint_t* ep = mca_pml_ob1_ep_array_get_next(&proc->btl_eager);
    mca_btl_base_descriptor_t* des;
    mca_pml_ob1_recv_frag_t* frag;
    mca_pml_ob1_ack_hdr_t* ack;
    int rc;

    /* allocate descriptor */
    MCA_PML_OB1_ENDPOINT_DES_ALLOC(ep, des, sizeof(mca_pml_ob1_ack_hdr_t));
    if(NULL == des) {
        goto retry;
    }

    /* fill out header */
    ack = (mca_pml_ob1_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_ACK;
    ack->hdr_common.hdr_flags = 0;
    ack->hdr_src_req = hdr->hdr_src_req;
    ack->hdr_dst_req.pval = recvreq;

    /*
     * lookup request buffer to determine if the memory is already
     * registered. if registered on both sides - do one rdma for
     * the entire message.
     */

    recvreq->req_chunk = mca_mpool_base_find(recvreq->req_recv.req_base.req_addr);
    if( NULL != recvreq->req_chunk &&
        ((hdr->hdr_match.hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_PIN)
         || mca_pml_ob1.leave_pinned)) {  /* BUG here! hdr_flags are 0! */ 
        struct mca_mpool_base_reg_mpool_t *reg = recvreq->req_chunk->mpools;
        while(reg->mpool != NULL) {
            if(NULL != mca_pml_ob1_ep_array_find(&proc->btl_rdma,(mca_btl_base_module_t*) reg->user_data)) {
                recvreq->req_rdma_offset = hdr->hdr_frag_length;
                ack->hdr_rdma_offset = hdr->hdr_frag_length;
                recvreq->req_mpool = reg;
                break;
            }
            reg++;
        }
    } 
    
    /* use the longer rdma protocol for this request if:
     * - size is larger than the rdma threshold
     * - rdma devices are available
    */
    if(NULL == recvreq->req_mpool && !mca_pml_ob1.leave_pinned) {
        if(recvreq->req_recv.req_bytes_packed > mca_pml_ob1.rdma_offset &&
           mca_pml_ob1_ep_array_get_size(&proc->btl_rdma) &&
           ompi_convertor_need_buffers(&recvreq->req_recv.req_convertor) == 0) {
    
            /* use convertor to figure out the rdma offset for this request */
            recvreq->req_rdma_offset = mca_pml_ob1.rdma_offset;
            ompi_convertor_set_position(
                &recvreq->req_recv.req_convertor,
                &recvreq->req_rdma_offset);
            ack->hdr_rdma_offset = recvreq->req_rdma_offset;
        } else {
            recvreq->req_rdma_offset = recvreq->req_recv.req_bytes_packed;
            ack->hdr_rdma_offset = recvreq->req_recv.req_bytes_packed;
        }
    }
    else{ 
        recvreq->req_rdma_offset = hdr->hdr_frag_length;
        ack->hdr_rdma_offset = hdr->hdr_frag_length;
    }

    /* initialize descriptor */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbfunc = mca_pml_ob1_send_ctl_complete;
    des->des_cbdata = ep;

    rc = ep->btl_send(ep->btl, ep->btl_endpoint, des, MCA_BTL_TAG_PML);
    if(rc != OMPI_SUCCESS) {
        ep->btl_free(ep->btl,des);
        goto retry;
    }
    return;

    /* queue request to retry later */
retry:
    MCA_PML_OB1_RECV_FRAG_ALLOC(frag,rc);
    frag->btl = NULL;
    frag->hdr.hdr_rndv = *hdr;
    frag->num_segments = 0;
    frag->request = recvreq;
    ompi_list_append(&mca_pml_ob1.acks_pending, (ompi_list_item_t*)frag);
}


/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_ob1_recv_request_progress(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments)
{
    size_t bytes_received = 0;
    size_t bytes_delivered = 0;
    size_t data_offset = 0;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    bool schedule;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:

            bytes_received = hdr->hdr_match.hdr_msg_length;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_match_hdr_t),
                data_offset,
                bytes_received,
                bytes_delivered);
            break;

        case MCA_PML_OB1_HDR_TYPE_RNDV:

#if MCA_PML_OB1_TIMESTAMPS
            recvreq->ack = get_profiler_timestamp();
#endif
            recvreq->req_send = hdr->hdr_rndv.hdr_src_req;
            mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv);
            bytes_received = hdr->hdr_rndv.hdr_frag_length;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_rendezvous_hdr_t),
                data_offset,
                bytes_received,
                bytes_delivered);
            break;

        case MCA_PML_OB1_HDR_TYPE_FRAG:

            bytes_received = hdr->hdr_frag.hdr_frag_length;
            data_offset = hdr->hdr_frag.hdr_frag_offset;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_frag_hdr_t),
                data_offset,
                bytes_received,
                bytes_delivered);
            break;

        case MCA_PML_OB1_HDR_TYPE_FIN:

            bytes_delivered = bytes_received = hdr->hdr_fin.hdr_rdma_length; 
            OMPI_THREAD_ADD32(&recvreq->req_pipeline_depth,-1);
            break;

        default:
            break;
    }

    /* check completion status */
    OMPI_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_bytes_received += bytes_received;
    recvreq->req_bytes_delivered += bytes_delivered;
    if (recvreq->req_bytes_received >= recvreq->req_recv.req_bytes_packed) {

        /* initialize request status */
        recvreq->req_recv.req_base.req_ompi.req_status._count = recvreq->req_bytes_delivered;
        recvreq->req_recv.req_base.req_pml_complete = true; 
        recvreq->req_recv.req_base.req_ompi.req_complete = true;

#if MCA_PML_OB1_TIMESTAMPS
        if(recvreq->req_bytes_received > 0) {
            int i;
            ompi_output(0, "[%d,%d,%d] dst ack: %llu",
                    ORTE_NAME_ARGS(orte_process_info.my_name), recvreq->ack);
            for(i=0; i<recvreq->pin_index; i++) {
                ompi_output(0, "[%d,%d,%d] dst pin, %llu %llu", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), recvreq->pin1[i], recvreq->pin2[i] - recvreq->pin1[i]);
            }
            for(i=0; i<recvreq->fin_index; i++) {
                ompi_output(0, "[%d,%d,%d] dst fin: %llu %llu",
                    ORTE_NAME_ARGS(orte_process_info.my_name), recvreq->fin1[i], recvreq->fin2[i] - recvreq->fin1[i]);
            }
        }
#endif
        if(ompi_request_waiting) {
            ompi_condition_broadcast(&ompi_request_cond);
        }
        schedule = false;
    } else if (recvreq->req_rdma_offset < recvreq->req_recv.req_bytes_packed) {
        schedule = true;
    }
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

    /* schedule additional rdma operations */
    if(schedule) {
        mca_pml_ob1_recv_request_schedule(recvreq);
    }
}


/*
 * Schedule RDMA protocol.
 *
*/

void mca_pml_ob1_recv_request_schedule(mca_pml_ob1_recv_request_t* recvreq)
{
    if(OMPI_THREAD_ADD32(&recvreq->req_lock,1) == 1) {
        mca_pml_ob1_proc_t* proc = recvreq->req_proc;
        size_t num_btl_avail = mca_pml_ob1_ep_array_get_size(&proc->btl_rdma);
        do {
            size_t bytes_remaining = recvreq->req_recv.req_bytes_packed - recvreq->req_rdma_offset;
            while(bytes_remaining > 0 && recvreq->req_pipeline_depth < mca_pml_ob1.recv_pipeline_depth) {
                mca_pml_ob1_endpoint_t* ep;
                size_t hdr_size;
                size_t size;
                mca_pml_ob1_rdma_hdr_t* hdr;
                mca_btl_base_descriptor_t* dst;
                mca_btl_base_descriptor_t* ctl;
                int rc;

                /*
                 * if the memory is already registed - use the NICs that its
                 * registed with. Otherwise, schedule round-robin across the
                 * available RDMA nics.
                */
                if(recvreq->req_mpool == NULL &&  !mca_pml_ob1.leave_pinned) {
                    ep = mca_pml_ob1_ep_array_get_next(&proc->btl_rdma);

                    /* if there is only one btl available or the size is less than
                     * than the min fragment size, schedule the rest via this btl
                     */
                    if(num_btl_avail == 1 || bytes_remaining < ep->btl_min_rdma_size) {
                        size = bytes_remaining;

                    /* otherwise attempt to give the BTL a percentage of the message
                     * based on a weighting factor. for simplicity calculate this as
                     * a percentage of the overall message length (regardless of amount
                     * previously assigned)
                     */
                    } else {
                        size = (ep->btl_weight * bytes_remaining) / 100;
                    }
    
                    /* makes sure that we don't exceed BTL max rdma size */
                    if (ep->btl_max_rdma_size != 0 && size > ep->btl_max_rdma_size) {
                        size = ep->btl_max_rdma_size;
                    }

                    /* prepare a descriptor for RDMA */
                    ompi_convertor_set_position(&recvreq->req_recv.req_convertor, &recvreq->req_rdma_offset);
#if MCA_PML_OB1_TIMESTAMPS
                    recvreq->pin1[recvreq->pin_index] = get_profiler_timestamp();
#endif
                    dst = ep->btl_prepare_dst(
                        ep->btl,
                        ep->btl_endpoint,
                        NULL,
                        &recvreq->req_recv.req_convertor,
                        0,
                        &size);
#if MCA_PML_OB1_TIMESTAMPS
                    recvreq->pin2[recvreq->pin_index] = get_profiler_timestamp();
#endif
                } else {
                    mca_mpool_base_registration_t * reg; 
                    size = bytes_remaining;

                    /* prepare a descriptor for RDMA */
                    ompi_convertor_set_position(&recvreq->req_recv.req_convertor, &recvreq->req_rdma_offset);

                    if(NULL != recvreq->req_mpool){ 
                        /* find the endpoint corresponding to this btl and schedule the entire message */
                        ep = mca_pml_ob1_ep_array_find(&proc->btl_rdma, (mca_btl_base_module_t*) recvreq->req_mpool->user_data);
                        reg = recvreq->req_mpool->mpool_registration; 
                        

                    }
                    else{ 
                        ep = mca_pml_ob1_ep_array_get_next(&proc->btl_rdma);
                        reg = NULL; 
                    }
                    
#if MCA_PML_OB1_TIMESTAMPS
                    recvreq->pin1[recvreq->pin_index] = get_profiler_timestamp();
#endif
                    dst = ep->btl_prepare_dst(
                                              ep->btl,
                                              ep->btl_endpoint,
                                              reg,
                                              &recvreq->req_recv.req_convertor,
                                              0,
                                              &size);
#if MCA_PML_OB1_TIMESTAMPS
                    recvreq->pin2[recvreq->pin_index] = get_profiler_timestamp();
#endif
                }

                if(dst == NULL) {
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.recv_pending, (ompi_list_item_t*)recvreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                dst->des_cbdata = recvreq;
#if MCA_PML_OB1_TIMESTAMPS
                recvreq->pin_index++;
#endif

                /* prepare a descriptor for rdma control message */
                hdr_size = sizeof(mca_pml_ob1_rdma_hdr_t);
                if(dst->des_dst_cnt > 1) {
                    hdr_size += (sizeof(mca_btl_base_segment_t) * (dst->des_dst_cnt-1));
                }

                MCA_PML_OB1_ENDPOINT_DES_ALLOC(ep, ctl, hdr_size);
                if(ctl == NULL) {
                    ep->btl_free(ep->btl,dst);
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.recv_pending, (ompi_list_item_t*)recvreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                ctl->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
                ctl->des_cbfunc = mca_pml_ob1_send_ctl_complete;
                ctl->des_cbdata = ep;
                
                /* fill in rdma header */
                hdr = (mca_pml_ob1_rdma_hdr_t*)ctl->des_src->seg_addr.pval;
                hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_PUT;
                hdr->hdr_common.hdr_flags = 0;
                hdr->hdr_src = recvreq->req_send;
                hdr->hdr_dst.pval = dst;
                hdr->hdr_rdma_offset = recvreq->req_rdma_offset;
                hdr->hdr_seg_cnt = dst->des_dst_cnt;
                memcpy(hdr->hdr_segs, dst->des_dst, dst->des_dst_cnt * sizeof(mca_btl_base_segment_t));

                /* update request state */
                recvreq->req_rdma_offset += size;
                OMPI_THREAD_ADD32(&recvreq->req_pipeline_depth,1);

                /* send rdma request to peer */
                rc = ep->btl_send(ep->btl, ep->btl_endpoint, ctl, MCA_BTL_TAG_PML);
                if(rc == OMPI_SUCCESS) {
                    bytes_remaining -= size;
                } else {
                    ep->btl_free(ep->btl,ctl);
                    ep->btl_free(ep->btl,dst);
                    recvreq->req_rdma_offset -= size;
                    OMPI_THREAD_ADD32(&recvreq->req_pipeline_depth,-1);
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.recv_pending, (ompi_list_item_t*)recvreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }

                /* run progress as the prepare (pinning) can take some time */
                mca_pml_ob1_progress();
            }
        } while(OMPI_THREAD_ADD32(&recvreq->req_lock,-1) > 0);
    }
}

/*
 * This routine is used to match a posted receive when the source process 
 * is specified.
*/

void mca_pml_ob1_recv_request_match_specific(mca_pml_ob1_recv_request_t* request)
{
    mca_pml_ob1_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_ob1_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
    mca_pml_ob1_recv_frag_t* frag;
   
    /* check for a specific match */
    OMPI_THREAD_LOCK(&comm->matching_lock);

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    if (ompi_list_get_size(&proc->unexpected_frags) > 0 &&
        (frag = mca_pml_ob1_recv_request_match_specific_proc(request, proc)) != NULL) {
        OMPI_THREAD_UNLOCK(&comm->matching_lock);
        
        mca_pml_ob1_recv_request_progress(request,frag->btl,frag->segments,frag->num_segments);
        if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
              (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
            MCA_PML_OB1_RECV_FRAG_RETURN(frag);
        }
        return; /* match found */
    }

    /* We didn't find any matches.  Record this irecv so we can match 
     * it when the message comes in.
    */
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE) { 
        ompi_list_append(&proc->specific_receives, (ompi_list_item_t*)request);
    }
    OMPI_THREAD_UNLOCK(&comm->matching_lock);
}


/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/

void mca_pml_ob1_recv_request_match_wild(mca_pml_ob1_recv_request_t* request)
{
    mca_pml_ob1_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_ob1_comm_proc_t* proc = comm->procs;
    size_t proc_count = comm->num_procs;
    size_t i;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
    */
    OMPI_THREAD_LOCK(&pml_comm->c_matching_lock);

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    for (i = 0; i < proc_count; i++) {
        mca_pml_ob1_recv_frag_t* frag;

        /* continue if no frags to match */
        if (ompi_list_get_size(&proc->unexpected_frags) == 0) {
            proc++;
            continue;
        }

        /* loop over messages from the current proc */
        if ((frag = mca_pml_ob1_recv_request_match_specific_proc(request, proc)) != NULL) {
            OMPI_THREAD_UNLOCK(&comm->matching_lock);

            mca_pml_ob1_recv_request_progress(request,frag->btl,frag->segments,frag->num_segments);
            if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
                  (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
                MCA_PML_OB1_RECV_FRAG_RETURN(frag);
            }
            return; /* match found */
        }
        proc++;
    } 

    /* We didn't find any matches.  Record this irecv so we can match to
     * it when the message comes in.
    */
 
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE)
        ompi_list_append(&comm->wild_receives, (ompi_list_item_t*)request);
    OMPI_THREAD_UNLOCK(&comm->matching_lock);
}


/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. 
*/

static mca_pml_ob1_recv_frag_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, 
    mca_pml_ob1_comm_proc_t* proc)
{
    ompi_list_t* unexpected_frags = &proc->unexpected_frags;
    mca_pml_ob1_recv_frag_t* frag;
    mca_pml_ob1_match_hdr_t* hdr;
    int tag = request->req_recv.req_base.req_tag;

    if( OMPI_ANY_TAG == tag ) {
        for (frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_recv_frag_t*)ompi_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_next(frag)) {
            hdr = &(frag->hdr.hdr_match);
            
            /* check first frag - we assume that process matching has been done already */
            if( hdr->hdr_tag >= 0 ) {
                goto find_fragment;
            } 
        }
    } else {
        for (frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_recv_frag_t*)ompi_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_next(frag)) {
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
    MCA_PML_OB1_RECV_REQUEST_MATCHED(request, hdr);
    if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
          (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
        ompi_list_remove_item(unexpected_frags, (ompi_list_item_t*)frag);
        frag->request = request;
    } 
    return frag;
}


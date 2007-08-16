/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "ompi_config.h"

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h" 
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_ob1_comm.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_rdmafrag.h"
#include "ompi/mca/bml/base/base.h" 
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/datatype/dt_arch.h"


static mca_pml_ob1_recv_frag_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, mca_pml_ob1_comm_proc_t* proc);

void mca_pml_ob1_recv_request_process_pending(void)
{
    mca_pml_ob1_recv_request_t* recvreq;
    int i, s = opal_list_get_size(&mca_pml_ob1.recv_pending);

    for(i = 0; i < s; i++) {
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        recvreq = (mca_pml_ob1_recv_request_t*)
            opal_list_remove_first(&mca_pml_ob1.recv_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        if(NULL == recvreq)
            break;
        recvreq->req_pending = false;
        if(mca_pml_ob1_recv_request_schedule_exclusive(recvreq) == 
                OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }
}

static int mca_pml_ob1_recv_request_free(struct ompi_request_t** request)
{
    mca_pml_ob1_recv_request_t* recvreq = *(mca_pml_ob1_recv_request_t**)request; 

    assert( false == recvreq->req_recv.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_recv.req_base.req_free_called = true;

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_NOTIFY,
                             &(recvreq->req_recv.req_base), PERUSE_RECV );

    if( true == recvreq->req_recv.req_base.req_pml_complete ) {
        MCA_PML_OB1_RECV_REQUEST_RETURN( recvreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
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
    OPAL_THREAD_LOCK(&comm->matching_lock);
    if( OMPI_ANY_TAG == ompi_request->req_status.MPI_TAG ) { /* the match has not been already done */
       if( request->req_recv.req_base.req_peer == OMPI_ANY_SOURCE ) {
          opal_list_remove_item( &comm->wild_receives, (opal_list_item_t*)request );
       } else {
          mca_pml_ob1_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
          opal_list_remove_item(&proc->specific_receives, (opal_list_item_t*)request);
       }
       PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q,
                                &(request->req_recv.req_base), PERUSE_RECV );
       /**
        * As now the PML is done with this request we have to force the lmp_complete
        * to true. Otherwise, the request will never be freed.
        */
       request->req_recv.req_base.req_pml_complete = true;
    }
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request->req_status._cancelled = true;
    /* This macro will set the req_complete to true so the MPI Test/Wait* functions
     * on this request will be able to complete. As the status is marked as
     * cancelled the cancel state will be detected.
     */
    MCA_PML_OB1_RECV_REQUEST_MPI_COMPLETE(request);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    return OMPI_SUCCESS;
}

static void mca_pml_ob1_recv_request_construct(mca_pml_ob1_recv_request_t* request)
{
    request->req_recv.req_base.req_type = MCA_PML_REQUEST_RECV;
    request->req_recv.req_base.req_ompi.req_free = mca_pml_ob1_recv_request_free;
    request->req_recv.req_base.req_ompi.req_cancel = mca_pml_ob1_recv_request_cancel;
    request->req_rdma_cnt = 0;
}

OBJ_CLASS_INSTANCE(
    mca_pml_ob1_recv_request_t,
    mca_pml_base_recv_request_t,
    mca_pml_ob1_recv_request_construct,
    NULL);


/*
 * Release resources.
 */

static void mca_pml_ob1_recv_ctl_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    MCA_BML_BASE_BTL_DES_RETURN(bml_btl, des);

    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/*
 * Put operation has completed remotely - update request status
 */

static void mca_pml_ob1_put_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)des->des_cbdata;
    size_t bytes_received = 0;

    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( des->des_dst, des->des_dst_cnt,
                                        0, bytes_received );
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_pipeline_depth,-1);
    mca_bml_base_free(bml_btl, des);

    /* check completion status */
    if( OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received)
        >= recvreq->req_recv.req_bytes_packed ) {
        MCA_PML_OB1_RECV_REQUEST_PML_COMPLETE( recvreq );
    } else if (recvreq->req_rdma_offset < recvreq->req_recv.req_bytes_packed) {
        /* schedule additional rdma operations */
        mca_pml_ob1_recv_request_schedule(recvreq);
    }
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/*
 *
 */

int mca_pml_ob1_recv_request_ack_send_btl(
        ompi_proc_t* proc, mca_bml_base_btl_t* bml_btl,
        uint64_t hdr_src_req, void *hdr_dst_req, uint64_t hdr_rdma_offset)
{
    mca_btl_base_descriptor_t* des;
    mca_pml_ob1_ack_hdr_t* ack;
    int rc;

    /* allocate descriptor */
    MCA_PML_OB1_DES_ALLOC(bml_btl, des, sizeof(mca_pml_ob1_ack_hdr_t));
    if(NULL == des) {
        return OMPI_ERR_OUT_OF_RESOURCE; 
    }

    /* fill out header */
    ack = (mca_pml_ob1_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_ACK;
    ack->hdr_common.hdr_flags = 0;
    ack->hdr_src_req.lval = hdr_src_req;
    ack->hdr_dst_req.pval = hdr_dst_req;
    ack->hdr_rdma_offset = hdr_rdma_offset;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
    ack->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
    /* if we are little endian and the remote side is big endian,
       we're responsible for making sure the data is in network byte
       order */
    if (proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        ack->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
        MCA_PML_OB1_ACK_HDR_HTON(*ack);
    }
#endif
#endif

    /* initialize descriptor */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbfunc = mca_pml_ob1_recv_ctl_completion;

    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(rc != OMPI_SUCCESS) {
        mca_bml_base_free(bml_btl, des);
        return OMPI_ERR_OUT_OF_RESOURCE; 
    }
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_recv_request_ack(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_pml_ob1_rendezvous_hdr_t* hdr, 
    size_t bytes_received)
{
    ompi_proc_t* proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
    mca_bml_base_endpoint_t* bml_endpoint = NULL; 

    bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml; 

    /* by default copy */
    recvreq->req_rdma_offset = hdr->hdr_msg_length;
    if(hdr->hdr_msg_length > bytes_received) {
        

        /*
         * lookup request buffer to determine if memory is already
         * registered. 
         */

        if(ompi_convertor_need_buffers(&recvreq->req_recv.req_convertor) == 0 &&
           hdr->hdr_match.hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_CONTIG) {
            char *base;
            ptrdiff_t lb;
            ompi_ddt_type_lb(recvreq->req_recv.req_convertor.pDesc, &lb);
            base = recvreq->req_recv.req_convertor.pBaseBuf + lb;
            recvreq->req_rdma_cnt = mca_pml_ob1_rdma_btls(
                                                          bml_endpoint,
                                                          (unsigned char*) base,
                                                          recvreq->req_recv.req_bytes_packed,
                                                          recvreq->req_rdma);
            
            /* memory is already registered on both sides */
            if (hdr->hdr_match.hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_PIN &&
                recvreq->req_rdma_cnt != 0) {

                recvreq->req_rdma_offset = bytes_received;
                
                /* are rdma devices available for long rdma protocol */
            } else if (bml_endpoint->btl_rdma_offset < hdr->hdr_msg_length &&
                       mca_bml_base_btl_array_get_size(&bml_endpoint->btl_rdma)) {
                
                /* use convertor to figure out the rdma offset for this request */
                recvreq->req_rdma_offset = bml_endpoint->btl_rdma_offset;
                if(recvreq->req_rdma_offset < bytes_received) {
                    recvreq->req_rdma_offset = bytes_received;
                }
                ompi_convertor_set_position( &recvreq->req_recv.req_convertor,
                                             &recvreq->req_rdma_offset );
            }
        }
        /* start rdma at current fragment offset - no need to ack */
        if(recvreq->req_rdma_offset == bytes_received)
            return OMPI_SUCCESS;
    }
    /* let know to shedule function there is no need to put ACK flag */
    recvreq->req_ack_sent = true;
    
    return mca_pml_ob1_recv_request_ack_send(proc, hdr->hdr_src_req.lval,
            recvreq, recvreq->req_rdma_offset);
}

                                                                                                            
/**
 * Return resources used by the RDMA
 */

static void mca_pml_ob1_rget_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)frag->rdma_req;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(status);
        orte_errmgr.abort();
    }

    mca_pml_ob1_send_fin(recvreq->req_recv.req_base.req_proc,
            frag->rdma_hdr.hdr_rget.hdr_des.pval, bml_btl); 

    /* is receive request complete */
    if( OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, frag->rdma_length)
        == recvreq->req_recv.req_bytes_packed ) {
        MCA_PML_OB1_RECV_REQUEST_PML_COMPLETE( recvreq );
    }

    MCA_PML_OB1_RDMA_FRAG_RETURN(frag);

    /* return rdma descriptor - do this after queuing the fin message - as 
     * release rdma resources (unpin memory) can take some time.
     */
    mca_bml_base_free(bml_btl, des);

    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}


/*
 *
 */
int mca_pml_ob1_recv_request_get_frag(
        mca_pml_ob1_rdma_frag_t* frag)
{
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)frag->rdma_req;
    mca_bml_base_endpoint_t* bml_endpoint = frag->rdma_ep;
    mca_bml_base_btl_t* bml_btl;
    mca_btl_base_descriptor_t* descriptor;
    size_t save_size = frag->rdma_length;
    int rc;
    
    char *base;
    ptrdiff_t lb;
    ompi_ddt_type_lb(recvreq->req_recv.req_convertor.pDesc, &lb);
    base = recvreq->req_recv.req_convertor.pBaseBuf + lb;
    bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma,
            frag->rdma_btl);
    if(NULL == bml_btl) {
        opal_output(0, "[%s:%d] invalid bml for rdma get", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* prepare descriptor */
    mca_bml_base_prepare_dst(
        bml_btl, 
        NULL,
        &recvreq->req_recv.req_convertor,
        0,
        &frag->rdma_length, 
        &descriptor);
    if(NULL == descriptor) {
        frag->rdma_length = save_size;
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    descriptor->des_src = frag->rdma_segs;
    descriptor->des_src_cnt = frag->rdma_hdr.hdr_rdma.hdr_seg_cnt;
    descriptor->des_cbfunc = mca_pml_ob1_rget_completion;
    descriptor->des_cbdata = frag;

    PERUSE_TRACE_COMM_OMPI_EVENT(PERUSE_COMM_REQ_XFER_CONTINUE,
                                 &(recvreq->req_recv.req_base),
                                 frag->rdma_length, PERUSE_RECV);

    /* queue up get request */
    if(OMPI_SUCCESS != (rc = mca_bml_base_get(bml_btl,descriptor))) {
        if(OMPI_ERR_OUT_OF_RESOURCE == rc) {
            mca_bml_base_free(bml_btl, descriptor);
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending,
                    (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } else {
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
    }

    return OMPI_SUCCESS;
}

static void mca_pml_ob1_recv_request_rget(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_btl_base_module_t* btl,
    mca_pml_ob1_rget_hdr_t* hdr)
{
    mca_bml_base_endpoint_t* bml_endpoint = NULL;
    mca_pml_ob1_rdma_frag_t* frag;
    size_t i, size = 0;
    int rc;

    /* if receive buffer is not contiguous we can't just RDMA read into it, so 
     * fall back to copy in/out protocol. It is a pity because buffer on the 
     * sender side is already registered. We need to ne smarter here, perhaps 
     * do couple of RDMA reads */ 
    if(ompi_convertor_need_buffers(&recvreq->req_recv.req_convertor) == true) { 
        mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv, 0); 
        return; 
    } 

    MCA_PML_OB1_RDMA_FRAG_ALLOC(frag,rc);
    if(NULL == frag) {
        /* GLB - FIX */
         ORTE_ERROR_LOG(rc);
         orte_errmgr.abort();
    }

    /* lookup bml datastructures */
    bml_endpoint = (mca_bml_base_endpoint_t*)recvreq->req_recv.req_base.req_proc->proc_bml; 

    /* allocate/initialize a fragment */
    for(i = 0; i < hdr->hdr_seg_cnt; i++) {
        size += hdr->hdr_segs[i].seg_len;
        frag->rdma_segs[i] = hdr->hdr_segs[i];
    }
    frag->rdma_hdr.hdr_rget = *hdr;
    frag->rdma_req = recvreq;
    frag->rdma_ep = bml_endpoint;
    frag->rdma_btl = btl;
    frag->rdma_length = size;
    frag->rdma_state = MCA_PML_OB1_RDMA_GET;

    mca_pml_ob1_recv_request_get_frag(frag);
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
    
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( segments, num_segments,
                                        0, bytes_received );
    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:

            bytes_received -= sizeof(mca_pml_ob1_match_hdr_t);
            recvreq->req_recv.req_bytes_packed = bytes_received;
            MCA_PML_OB1_RECV_REQUEST_MATCHED(recvreq,&hdr->hdr_match);
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
            bytes_received -= sizeof(mca_pml_ob1_rendezvous_hdr_t);
                   
            recvreq->req_recv.req_bytes_packed = hdr->hdr_rndv.hdr_msg_length;
            recvreq->req_send = hdr->hdr_rndv.hdr_src_req;
            MCA_PML_OB1_RECV_REQUEST_MATCHED(recvreq,&hdr->hdr_match);

            mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv, bytes_received);

            if(recvreq->req_recv.req_base.req_pml_complete) { 
                /* We must check that completion hasn't already occured */
                /*  for the self BTL we may choose the RDMA PUT protocol */
                /*  on the send side, in  this case we send no eager data */
                /*  if, on the receiver side the data is not contiguous we  */
                /*  may choose to use the copy in/out protocol */ 
                /*  if this occurs, the entire request can be completed in a */
                /*  single call to mca_pml_ob1_recv_request_ack */ 
                /*  as soon as the last fragment of the copy in/out protocol */ 
                /*  gets local completion. This doesn't occur in the general */
                /*  case of the copy in/out protocol because when both sender */
                /*  and receiver agree on the copy in/out protoocol we eagerly */
                /*  send data, we don't update the request with this eagerly sent */
                /*  data until the end of this function, so completion could not have */
                /*  yet occurred. */
                return;
            }
                
            /**
             * The PUT protocol do not attach any data to the original request.
             * Therefore, we might want to avoid unpacking if there is nothing to
             * unpack.
             */
            if( 0 < bytes_received ) {
                MCA_PML_OB1_RECV_REQUEST_UNPACK(
                    recvreq,
                    segments,
                    num_segments,
                    sizeof(mca_pml_ob1_rendezvous_hdr_t),
                    data_offset,
                    bytes_received,
                    bytes_delivered);
            }
            break;

        case MCA_PML_OB1_HDR_TYPE_RGET:

            recvreq->req_recv.req_bytes_packed = hdr->hdr_rndv.hdr_msg_length;
            MCA_PML_OB1_RECV_REQUEST_MATCHED(recvreq,&hdr->hdr_match);
            mca_pml_ob1_recv_request_rget(recvreq, btl, &hdr->hdr_rget);
            return;

        case MCA_PML_OB1_HDR_TYPE_FRAG:

            bytes_received -= sizeof(mca_pml_ob1_frag_hdr_t);
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

        default:
            break;
    }

    /* check completion status */
    if( OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received)
        >= recvreq->req_recv.req_bytes_packed ) {
        MCA_PML_OB1_RECV_REQUEST_PML_COMPLETE( recvreq );
    } else if (recvreq->req_rdma_offset < recvreq->req_recv.req_bytes_packed) {
        /* schedule additional rdma operations */
        mca_pml_ob1_recv_request_schedule(recvreq);
    }
}


/**
 * Handle completion of a probe request
 */

void mca_pml_ob1_recv_request_matched_probe(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_btl_base_module_t* btl,
    mca_btl_base_segment_t* segments,
    size_t num_segments)
{
    size_t bytes_packed = 0;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:

            MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( segments, num_segments,
                                                sizeof(mca_pml_ob1_match_hdr_t),
                                                bytes_packed );
            break;

        case MCA_PML_OB1_HDR_TYPE_RNDV:
        case MCA_PML_OB1_HDR_TYPE_RGET:

            bytes_packed = hdr->hdr_rndv.hdr_msg_length;
            break;
    }

    /* set completion status */
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_TAG = hdr->hdr_match.hdr_tag;
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE = hdr->hdr_match.hdr_src;
    recvreq->req_bytes_received = bytes_packed;
    recvreq->req_bytes_delivered = bytes_packed;
    MCA_PML_OB1_RECV_REQUEST_PML_COMPLETE( recvreq );
}


/*
 * Schedule RDMA protocol.
 *
*/

int mca_pml_ob1_recv_request_schedule_exclusive(
        mca_pml_ob1_recv_request_t* recvreq)
{
    ompi_proc_t* proc = recvreq->req_recv.req_base.req_proc;
    mca_bml_base_endpoint_t* bml_endpoint =
        (mca_bml_base_endpoint_t*) proc->proc_bml; 
    mca_bml_base_btl_t* bml_btl; 
    int num_btl_avail =
        mca_bml_base_btl_array_get_size(&bml_endpoint->btl_rdma);
    int num_tries = num_btl_avail;

    if(recvreq->req_rdma_cnt)
        num_tries = recvreq->req_rdma_cnt;

    do {
        size_t bytes_remaining = recvreq->req_recv.req_bytes_packed -
            recvreq->req_rdma_offset;
        size_t prev_bytes_remaining = 0;
        int num_fail = 0;

        while(bytes_remaining > 0 &&
                recvreq->req_pipeline_depth < mca_pml_ob1.recv_pipeline_depth) {
            size_t hdr_size;
            size_t size, i;
            mca_pml_ob1_rdma_hdr_t* hdr;
            mca_btl_base_descriptor_t* dst;
            mca_btl_base_descriptor_t* ctl;
            mca_mpool_base_registration_t * reg = NULL;
            int rc;
               
            if(prev_bytes_remaining == bytes_remaining) {
                if( ++num_fail == num_tries ) {
                    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                    if(false == recvreq->req_pending) {
                        opal_list_append(&mca_pml_ob1.recv_pending,
                                (opal_list_item_t*)recvreq);
                        recvreq->req_pending = true;
                    }
                    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
            } else {
                num_fail = 0;
                prev_bytes_remaining = bytes_remaining;
            }

            ompi_convertor_set_position(&recvreq->req_recv.req_convertor,
                                        &recvreq->req_rdma_offset);

            if(recvreq->req_rdma_cnt) {
                /*
                 * Select the next btl out of the list w/ preregistered
                 * memory.
                 */
                 bml_btl = recvreq->req_rdma[recvreq->req_rdma_idx].bml_btl;
                 num_btl_avail = recvreq->req_rdma_cnt - recvreq->req_rdma_idx;
                 reg = recvreq->req_rdma[recvreq->req_rdma_idx].btl_reg;

                 if(++recvreq->req_rdma_idx >= recvreq->req_rdma_cnt)
                    recvreq->req_rdma_idx = 0;
            } else {
                /*
                 * Otherwise, schedule round-robin across the
                 * available RDMA nics dynamically registering/deregister
                 * as required.
                 */
                bml_btl =
                    mca_bml_base_btl_array_get_next(&bml_endpoint->btl_rdma);
            }

            /*
             * If more than one NIC is available - try to use both for
             * anything larger than the eager limit
             */
            if(num_btl_avail == 1 ||
                    bytes_remaining < bml_btl->btl_eager_limit) {
                size = bytes_remaining;
            } else {
                /* 
                 * otherwise attempt to give the BTL a percentage of
                 * the message based on a weighting factor. for
                 * simplicity calculate this as a percentage of the
                 * overall message length (regardless of amount
                 * previously assigned)
                 */
                size = (size_t)(bml_btl->btl_weight * bytes_remaining);
            }
            /* makes sure that we don't exceed BTL max rdma size
             * if memory is not pinned already */
            if(0 == recvreq->req_rdma_cnt &&
                    bml_btl->btl_max_rdma_size != 0 &&
                    size > bml_btl->btl_max_rdma_size) {
                size = bml_btl->btl_max_rdma_size;
            }

            /* prepare a descriptor for RDMA */
            mca_bml_base_prepare_dst(bml_btl, reg,
                    &recvreq->req_recv.req_convertor, 0, &size, &dst);

            if(dst == NULL) {
                continue;
            }

            dst->des_cbfunc = mca_pml_ob1_put_completion;
            dst->des_cbdata = recvreq;

            /* prepare a descriptor for rdma control message */
            hdr_size = sizeof(mca_pml_ob1_rdma_hdr_t);
            if(dst->des_dst_cnt > 1) {
                hdr_size += (sizeof(mca_btl_base_segment_t) *
                        (dst->des_dst_cnt-1));
            }

            MCA_PML_OB1_DES_ALLOC(bml_btl, ctl, hdr_size);
            if(ctl == NULL) {
                mca_bml_base_free(bml_btl,dst);
                continue;
            }
            ctl->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
            ctl->des_cbfunc = mca_pml_ob1_recv_ctl_completion;
            
            /* fill in rdma header */
            hdr = (mca_pml_ob1_rdma_hdr_t*)ctl->des_src->seg_addr.pval;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_PUT;
            hdr->hdr_common.hdr_flags =
                (!recvreq->req_ack_sent) ? MCA_PML_OB1_HDR_TYPE_ACK : 0;
            hdr->hdr_req = recvreq->req_send;
            hdr->hdr_des.pval = dst;
            hdr->hdr_rdma_offset = recvreq->req_rdma_offset;
            hdr->hdr_seg_cnt = dst->des_dst_cnt;

            for( i = 0; i < dst->des_dst_cnt; i++ ) {
                hdr->hdr_segs[i].seg_addr.lval = ompi_ptr_ptol(dst->des_dst[i].seg_addr.pval);
                hdr->hdr_segs[i].seg_len       = dst->des_dst[i].seg_len;
                hdr->hdr_segs[i].seg_key.key64 = dst->des_dst[i].seg_key.key64;
            }

            if(!recvreq->req_ack_sent)
                recvreq->req_ack_sent = true;
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
            hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
            /* if we are little endian and the remote side is big endian,
               we're responsible for making sure the data is in network byte
               order */
            /* RDMA is currently disabled by bml if arch doesn't
               match, so this shouldn't be needed.  here to make sure
               we remember if we ever change the bml. */
            assert(0 == (recvreq->req_recv.req_base.req_proc->proc_arch & 
                         OMPI_ARCH_ISBIGENDIAN));
#endif
#endif

            PERUSE_TRACE_COMM_OMPI_EVENT( PERUSE_COMM_REQ_XFER_CONTINUE,
                                          &(recvreq->req_recv.req_base), size,
                                          PERUSE_RECV);

            /* send rdma request to peer */
            rc = mca_bml_base_send(bml_btl, ctl, MCA_BTL_TAG_PML);
            if(rc == OMPI_SUCCESS) {
                /* update request state */
                recvreq->req_rdma_offset += size;
                OPAL_THREAD_ADD_SIZE_T(&recvreq->req_pipeline_depth,1);
                bytes_remaining -= size;
            } else {
                mca_bml_base_free(bml_btl,ctl);
                mca_bml_base_free(bml_btl,dst);
                continue;
            }

            /* run progress as the prepare (pinning) can take some time */
            mca_bml.bml_progress();
        }
    } while(OPAL_THREAD_ADD32(&recvreq->req_lock,-1) > 0);

    return OMPI_SUCCESS;
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
    OPAL_THREAD_LOCK(&comm->matching_lock);
    /**
     * The laps of time between the ACTIVATE event and the SEARCH_UNEX one include
     * the cost of the request lock.
     */
    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_SEARCH_UNEX_Q_BEGIN,
                             &(request->req_recv.req_base), PERUSE_RECV );

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    if (opal_list_get_size(&proc->unexpected_frags) > 0 &&
        (frag = mca_pml_ob1_recv_request_match_specific_proc(request, proc)) != NULL) {
        OPAL_THREAD_UNLOCK(&comm->matching_lock);

        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_SEARCH_UNEX_Q_END,
                                 &(request->req_recv.req_base), PERUSE_RECV );

        if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
              (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
            mca_pml_ob1_recv_request_progress(request,frag->btl,frag->segments,frag->num_segments);
            MCA_PML_OB1_RECV_FRAG_RETURN(frag);
        } else {
            mca_pml_ob1_recv_request_matched_probe(request,frag->btl,frag->segments,frag->num_segments);
        }
        return; /* match found */
    }

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_SEARCH_UNEX_Q_END,
                             &(request->req_recv.req_base), PERUSE_RECV );

    /* We didn't find any matches.  Record this irecv so we can match 
     * it when the message comes in.
     */
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE) { 
        opal_list_append(&proc->specific_receives, (opal_list_item_t*)request);
        if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_PROBE) {
            PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_INSERT_IN_POSTED_Q,
                                     &(request->req_recv.req_base), PERUSE_RECV );
        }
    }
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
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
    OPAL_THREAD_LOCK(&comm->matching_lock);
    /**
     * The laps of time between the ACTIVATE event and the SEARCH_UNEX one include
     * the cost of the request lock.
     */
    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_SEARCH_UNEX_Q_BEGIN,
                             &(request->req_recv.req_base), PERUSE_RECV );

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    for (i = 0; i < proc_count; i++) {
        mca_pml_ob1_recv_frag_t* frag;

        /* continue if no frags to match */
        if (opal_list_get_size(&proc->unexpected_frags) == 0) {
            proc++;
            continue;
        }

        /* loop over messages from the current proc */
        if ((frag = mca_pml_ob1_recv_request_match_specific_proc(request, proc)) != NULL) {
            OPAL_THREAD_UNLOCK(&comm->matching_lock);

            PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_SEARCH_UNEX_Q_END,
                                     &(request->req_recv.req_base), PERUSE_RECV );

            if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
                  (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
                mca_pml_ob1_recv_request_progress(request,frag->btl,frag->segments,frag->num_segments);
                MCA_PML_OB1_RECV_FRAG_RETURN(frag);
            } else {
                mca_pml_ob1_recv_request_matched_probe(request,frag->btl,frag->segments,frag->num_segments);
            }
            return; /* match found */
        }
        proc++;
    } 

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_SEARCH_UNEX_Q_END,
                             &(request->req_recv.req_base), PERUSE_RECV );

    /* We didn't find any matches.  Record this irecv so we can match to
     * it when the message comes in.
    */
 
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE) {
        opal_list_append(&comm->wild_receives, (opal_list_item_t*)request);
        /**
         * We don't want to generate this kind of event for MPI_Probe. Hopefully,
         * the compiler will optimize out the empty if loop in the case where PERUSE
         * support is not required by the user.
         */
        if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_PROBE) {
            PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_INSERT_IN_POSTED_Q,
                                     &(request->req_recv.req_base), PERUSE_RECV );
        }
    }
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
}


/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. This
 *  function has to be called with the communicator matching lock held.
*/

static mca_pml_ob1_recv_frag_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, 
    mca_pml_ob1_comm_proc_t* proc)
{
    opal_list_t* unexpected_frags = &proc->unexpected_frags;
    mca_pml_ob1_recv_frag_t* frag;
    mca_pml_ob1_match_hdr_t* hdr;
    int tag = request->req_recv.req_base.req_tag;

    if( OMPI_ANY_TAG == tag ) {
        for (frag =  (mca_pml_ob1_recv_frag_t*)opal_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_recv_frag_t*)opal_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_recv_frag_t*)opal_list_get_next(frag)) {
            hdr = &(frag->hdr.hdr_match);
            
            /* check first frag - we assume that process matching has been done already */
            if( hdr->hdr_tag >= 0 ) {
                goto find_fragment;
            } 
        }
    } else {
        for (frag =  (mca_pml_ob1_recv_frag_t*)opal_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_recv_frag_t*)opal_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_recv_frag_t*)opal_list_get_next(frag)) {
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
    request->req_recv.req_base.req_proc = proc->ompi_proc;
    if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
          (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
        PERUSE_TRACE_MSG_EVENT( PERUSE_COMM_MSG_REMOVE_FROM_UNEX_Q,
                                request->req_recv.req_base.req_comm,
                                hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV );
        opal_list_remove_item(unexpected_frags, (opal_list_item_t*)frag);
        frag->request = request;
    } 
    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_MATCH_UNEX,
                             &(request->req_recv.req_base), PERUSE_RECV );
    return frag;
}


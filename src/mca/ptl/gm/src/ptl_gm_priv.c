/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * $HEADER$
 */
#include "ompi_config.h"

#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "ptl_gm.h"
#include "ptl_gm_req.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_sendfrag.h"
#include "ptl_gm_priv.h"

int mca_ptl_gm_peer_put(mca_ptl_gm_peer_t *ptl_peer,
                        mca_ptl_gm_send_frag_t *fragment, 
                        struct mca_pml_base_send_request_t *sendreq, 
                        size_t offset,
                        size_t *size, 
                        int flags,
                        void * target_buffer,
                        int bytes) 
{
    gm_put( ptl_peer->peer_ptl->gm_port, fragment->registered_buf,
	    (gm_remote_ptr_t)target_buffer, bytes, GM_LOW_PRIORITY,
	    ptl_peer->local_id, ptl_peer->port_number, 
	    put_callback, (void *)fragment );

    fragment->send_frag.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->send_frag.frag_base.frag_peer =
	(struct mca_ptl_base_peer_t*)ptl_peer;
    fragment->send_frag.frag_base.frag_addr =(void *)target_buffer;
    fragment->send_frag.frag_base.frag_size = bytes; 
    return OMPI_SUCCESS;
}

int mca_ptl_gm_peer_send( mca_ptl_gm_peer_t *ptl_peer,
			  mca_ptl_gm_send_frag_t *fragment, 
			  struct mca_pml_base_send_request_t *sendreq, 
			  size_t offset,
			  size_t *size, 
			  int flags ) 
{
    struct iovec iov;
    size_t size_in,size_out;
    int header_length;
    mca_ptl_base_header_t* header;
    ompi_convertor_t *convertor = NULL;
    int rc, freeAfter;
    unsigned int in_size, max_data;

    header = (mca_ptl_base_header_t*)fragment->send_buf;
    header_length = sizeof(mca_ptl_base_match_header_t);
    A_PRINT("peer send (could be ack) : headerlen is %d \n", header_length);
    size_in = *size;
  
    if( (size_in + header_length) <= GM_BUF_SIZE ) 
	iov.iov_len = size_in;
    else
	iov.iov_len = GM_BUF_SIZE - header_length;

    if( size_in > 0 ) {
        /* first fragment (eager send) and first fragment of long protocol
         * can use the convertor initialized on the request. The remaining
         * fragments must copy/reinit the convertor.
         */
        if( offset <= mca_ptl_gm_module.super.ptl_first_frag_size ) {
            convertor = &sendreq->req_convertor;
        } else {
            convertor = &(fragment->send_frag.frag_base.frag_convertor);
            ompi_convertor_copy(&sendreq->req_convertor, convertor);
            ompi_convertor_init_for_send( convertor,
                                          0,
                                          sendreq->req_base.req_datatype,
                                          sendreq->req_base.req_count,
                                          sendreq->req_base.req_addr,
                                          offset, NULL );
        }

        /* if data is contigous convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer 
         * that holds the packed data
         */

        /* copy the data to the registered buffer */
        iov.iov_base = ((char*)fragment->send_buf) + header_length;
	max_data = iov.iov_len;
	in_size = 1;
        if((rc = ompi_convertor_pack(convertor, &(iov), &in_size, &max_data, &freeAfter)) < 0)
            return OMPI_ERROR;
    }

    if( (header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_FRAG) ||
	(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_MATCH) )
	header->hdr_match.hdr_msg_length = iov.iov_len;

    /* adjust size and request offset to reflect actual number of bytes
     * packed by convertor
     */
    size_out = iov.iov_len + header_length;
  
    A_PRINT( "peer_send request is %p\t, frag->req = %p, fragment is %p,size is %d, send_frag is %p\n",
	     sendreq, fragment->req, fragment, size_out,
	     ((mca_ptl_base_header_t *)header)->hdr_ack.hdr_src_ptr);
 
    DO_DEBUG( printf( "send pointer %p SIZE %d length %lu\n",
                      (void*)fragment->send_buf, GM_BUF_SIZE, size_out ) );

    fragment->send_frag.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->send_frag.frag_base.frag_peer = (struct mca_ptl_base_peer_t*)ptl_peer;
    fragment->send_frag.frag_base.frag_addr = ((char*)fragment->send_buf) + header_length; 
    fragment->send_frag.frag_base.frag_size = size_out - header_length;
    fragment->send_frag.frag_request = sendreq;

    fragment->already_send = 0;
    /* initiate the gm send */
    gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, fragment->send_buf, 
                                   GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
                                   send_callback, (void *)fragment );
    if( size_out <= GM_BUF_SIZE ) {
        /* small message. All data went out */
        ptl_peer->peer_ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl_peer->peer_ptl,
                                                     fragment->send_frag.frag_request,
                                                     iov.iov_len );
        /*gm_send_with_callback( ptl_peer->peer_ptl->gm_port, fragment->send_buf, 
          GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
          ptl_peer->port_number, send_callback, (void *)fragment );*/
        *size = iov.iov_len;
    } else if( size_in <= (5 * GM_BUF_SIZE) ) {  /* eager message */
        while( size_out < size_in ) {
            ompi_list_item_t* item;
            mca_ptl_gm_eager_header_t* header;
            
            OMPI_FREE_LIST_WAIT( &(ptl_peer->peer_ptl->gm_send_dma_frags), item, rc );
            iov.iov_base = (char*)header + sizeof(mca_ptl_gm_eager_header_t);
            iov.iov_len = GM_BUF_SIZE - sizeof(mca_ptl_gm_eager_header_t);
            max_data = iov.iov_len;
            in_size = 1;

            if((rc = ompi_convertor_pack(convertor, &(iov), &in_size, &max_data, &freeAfter)) < 0)
                return OMPI_ERROR;

            size_out += iov.iov_len;
            header = (mca_ptl_gm_eager_header_t*)item;
            header->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
            header->hdr_common.hdr_flags = 0;
            header->hdr_src_ptr.pval = fragment;

            gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, header,
                                           GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
                                           send_callback, (void *)header );
        }
        ptl_peer->peer_ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl_peer->peer_ptl,
                                                     fragment->send_frag.frag_request, size_out );
        *size = size_out;
    } else {  /* large message */
        
    }

    A_PRINT("inside peer send : bytes sent is %d\n",*size);
    return OMPI_SUCCESS;
}


void put_callback(struct gm_port *port,void * context, gm_status_t status)
{
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_send_frag_t *putfrag;
    mca_pml_base_send_request_t *send_req;
    mca_ptl_base_header_t* header;
    int bytes2;

    putfrag = (mca_ptl_gm_send_frag_t *)context;
    header = (mca_ptl_base_header_t*)putfrag->send_buf;
    bytes2 = header->hdr_ack.hdr_dst_size;
    ptl = (mca_ptl_gm_module_t *)putfrag->ptl;
    send_req = putfrag->req;

    A_PRINT("ENTERING PUT CALLBACK\n");

    switch  (status) {
    case GM_SUCCESS:
        /* local put completed, mark put as complete */
   
        GM_DBG(PTL_GM_DBG_COMM,"PUTCALLBACK WITH CASE GM_SUCCESS\n");
        ompi_atomic_add( &(ptl->num_send_tokens), 1 );
        putfrag->put_sent = 1;

        /* send the header information through send/receive channel */
#if 0
        rc = mca_ptl_gm_peer_send (putfrag->peer,putfrag,send_req,
                                offset,&size,flags);
        assert(rc == 0);
        GM_DBG(PTL_GM_DBG_COMM,"FINISHED SENDING FIN\n");
        GM_DBG(PTL_GM_DBG_COMM,"after issuing the put completion the request offset = %d\n",send_req->req_offset);
#endif

        /* deregister the user memory */
       status = gm_deregister_memory(ptl->gm_port, (char *)(putfrag->registered_buf), bytes2);

       if(GM_SUCCESS != status) {
    	   ompi_output(0," unpinning memory failed\n");
       } else {
    	   GM_DBG(PTL_GM_DBG_COMM, " unpinning %d bytes of memory  success\n",bytes2);
       }    
       break;

    case GM_SEND_TIMED_OUT:
        /* need to take care of retransmission */
        break;

    case GM_SEND_DROPPED:
        /* need to handle this case */
        break;

    default:
        ompi_output(0, "[%s:%d] error in message completion\n",__FILE__,__LINE__);
        break;
    }
}

void send_callback( struct gm_port *port, void * context, gm_status_t status )
{
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_send_frag_t *frag;
    mca_pml_base_send_request_t *gm_send_req;
    mca_ptl_base_header_t* header;


    frag = (mca_ptl_gm_send_frag_t *)context;
    ptl = (mca_ptl_gm_module_t *)frag->send_frag.frag_base.frag_owner;
    gm_send_req = frag->send_frag.frag_request;   
 
    header = (mca_ptl_base_header_t*)frag->send_buf;

    switch  (status) {
    case GM_SUCCESS:
        DO_DEBUG( printf( "send_callback for data ptr %p\n", (void*)frag->send_buf ) );
	ompi_atomic_add( &(ptl->num_send_tokens), 1 );

        switch( header->hdr_common.hdr_type ) {
        case MCA_PTL_HDR_TYPE_MATCH:
	    /*ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, frag->send_frag.frag_request,
              header->hdr_match.hdr_msg_length);*/
	    
            /* return the DMA memory */
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_dma_frags), ((ompi_list_item_t *)frag->send_buf));
            frag->send_buf = NULL;
	    /* Return sendfrag to free list */
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t *) frag));
            break;
        case MCA_PTL_HDR_TYPE_RNDV:
        case MCA_PTL_HDR_TYPE_FRAG:
            {
                /* Use by eager messages. It contains just enough informations to be able
                 * to retrieve the fragment to whom it belong. I use the mca_ptl_gm_eager_header_t
                 * and I store in hdr_src_ptr the pointer to the fragment that have been
                 * generated the send operation.
                 */
                mca_ptl_gm_eager_header_t* header = (mca_ptl_gm_eager_header_t*)context;
                frag = (mca_ptl_gm_send_frag_t*)(header->hdr_src_ptr.pval);
                ptl = (mca_ptl_gm_module_t *)frag->send_frag.frag_base.frag_owner;
                frag->already_send += (GM_BUF_SIZE - sizeof(mca_ptl_gm_eager_header_t));
                if( frag->already_send > frag->send_frag.frag_base.frag_size ) {
                    frag->send_buf = NULL;
                    /* I will update the status of the request just once when everything is
                     * already done. Dont waste the time :)
                     */
                    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, frag->send_frag.frag_request, 
                                                  frag->send_frag.frag_base.frag_size );
                    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t*)frag));
                }
                OMPI_FREE_LIST_RETURN(&(ptl->gm_send_dma_frags), ((ompi_list_item_t *)header));
            }
            break;
        case MCA_PTL_HDR_TYPE_ACK:
	    A_PRINT("send callback: Completion of send_ack, sent frag is %p\n", frag);
            /* return the DMA memory */
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_dma_frags), ((ompi_list_item_t *)frag->send_buf));
            frag->send_buf = NULL;
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t *) frag));
            break;
        case MCA_PTL_HDR_TYPE_FIN:
	    A_PRINT("send callback : Completion of fin, bytes complete = %d\n",
                    header->hdr_ack.hdr_dst_size);
	    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, frag->send_frag.frag_request, 
					  header->hdr_ack.hdr_dst_size);
	    
            /* return the DMA memory */
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_dma_frags), ((ompi_list_item_t *)frag->send_buf));
            frag->send_buf = NULL;
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t *) frag));
            break;
        default:
	    /* Not going to call progress on this send,
	     * and not free-ing descriptor */
	    frag->send_complete = 1;
	    A_PRINT("send callback : match required but not yet recv ack sendfrag is %p\n",frag);
        }
	break;
	
    case GM_SEND_TIMED_OUT:
        /* need to take care of retransmission */
        break;
	
    case GM_SEND_DROPPED:
        /* need to handle this case */
        break;
	
    default:
        ompi_output(0, "[%s:%d] error in message completion\n",__FILE__,__LINE__);
        break;
    }
    
    A_PRINT("RETURNING FROM SEND_CALLBACK\n");
}

static void ptl_gm_ctrl_frag(struct mca_ptl_gm_module_t *ptl, mca_ptl_base_header_t * header)
{
    mca_ptl_gm_send_frag_t * frag;
    mca_pml_base_send_request_t *req;
    mca_pml_base_recv_request_t *request;
    int bytes;
    char * reg_buf; 
    int status;
  
    if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_ACK) {
	A_PRINT( "ack header is %d, frag_ptr is %p\n", header->hdr_common.hdr_size,
		 header->hdr_ack.hdr_src_ptr.pval);
	frag = (mca_ptl_gm_send_frag_t *)(header->hdr_ack.hdr_src_ptr.pval);
	A_PRINT("inside ACK, corresp frag pointer  %p\n",frag);
	req = (mca_pml_base_send_request_t *) frag->req;
	assert(req != NULL);
	req->req_peer_match.pval = header->hdr_ack.hdr_dst_match.pval;
	req->req_peer_addr.pval = header->hdr_ack.hdr_dst_addr.pval;
	req->req_peer_size = header->hdr_ack.hdr_dst_size;
	frag->wait_for_ack = 0;
	
	bytes = frag->send_frag.frag_base.frag_size; 
	
	if(frag->send_complete == 1) {
	    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
					  frag->send_frag.frag_request,bytes); 
            
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), (ompi_list_item_t *)frag);
	    A_PRINT( "inside ACK,returning frag pointer  %p, request is %p, bytes is %d\n",
		     frag, frag->send_frag.frag_request, header->hdr_frag.hdr_frag_length );
	}
    } else if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_FIN) {
	GM_DBG(PTL_GM_DBG_COMM,"CASE: HDR_TYPE_FIN\n");
	request = (mca_pml_base_recv_request_t*)
	    header->hdr_ack.hdr_dst_match.pval;
	/* call receive progress and indicate the recv has been completed  */
	
	GM_DBG( PTL_GM_DBG_COMM,"Calling recv_progress with bytes = %ld\n",
                (long)header->hdr_ack.hdr_dst_size );
	ptl->super.ptl_recv_progress (
				      (mca_ptl_base_module_t *) ptl,
				      request , 
				      header->hdr_ack.hdr_dst_size,
				      header->hdr_ack.hdr_dst_size);
	/* deregister the memory */
	bytes = header->hdr_ack.hdr_dst_size;
	reg_buf =(char *) header->hdr_ack.hdr_dst_addr.pval; 
	status = gm_deregister_memory(ptl->gm_port, reg_buf, bytes);
	
	if(GM_SUCCESS != status) {
	    ompi_output(0," unpinning memory failed\n");
	} else {
	    GM_DBG(PTL_GM_DBG_COMM, "unpinning memory success,addr:%p,bytes:%d\n",reg_buf,bytes);
	}
    } else {
	OMPI_OUTPUT((0, "Unkonwn header type in ptl_gm_ctrl_frag\n"));
    }
}

static mca_ptl_gm_recv_frag_t*
mca_ptl_gm_recv_frag_match( struct mca_ptl_gm_module_t *ptl,
			    gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t * recv_frag;
    bool matched;
    mca_ptl_base_header_t *header;
    
    header = (mca_ptl_base_header_t *)gm_ntohp(event->recv.buffer);
    
    /* allocate a receive fragment */
    recv_frag = mca_ptl_gm_alloc_recv_frag( (struct mca_ptl_base_module_t*)ptl );
    A_PRINT("the allocate drecv fragment is %p\n", recv_frag);
    
    recv_frag->frag_recv.frag_base.frag_owner = (struct mca_ptl_base_module_t*)ptl;
    recv_frag->frag_recv.frag_base.frag_peer = NULL;
    recv_frag->frag_recv.frag_request = NULL;
    recv_frag->frag_recv.frag_is_buffered = false;
    recv_frag->frag_hdr_cnt = 0;
    recv_frag->frag_msg_cnt = 0;
    recv_frag->frag_ack_pending = false;
    recv_frag->frag_progressed = 0;
    
    recv_frag->frag_recv.frag_base.frag_header = *header;

    recv_frag->frag_recv.frag_base.frag_addr =
	(char *) header + sizeof(mca_ptl_base_match_header_t);
    recv_frag->frag_recv.frag_base.frag_size = header->hdr_match.hdr_msg_length;
    
    recv_frag->matched = false;
    recv_frag->have_allocated_buffer = false;
    recv_frag->ptl = ptl;
    
    matched = ptl->super.ptl_match( &(ptl->super),
                                    &(recv_frag->frag_recv),
                                    &(recv_frag->frag_recv.frag_base.frag_header.hdr_match) );
    if( matched ) {
        return NULL;
    }
    
    GM_DBG(PTL_GM_DBG_COMM,
	   "matching receive not yet posted get tag %d comm %d source %d\n",
	   header->hdr_match.hdr_tag, header->hdr_match.hdr_contextid, header->hdr_match.hdr_src );
    return recv_frag;
}

static mca_ptl_gm_recv_frag_t*
mca_ptl_gm_recv_frag_frag( struct mca_ptl_gm_module_t *ptl,
                           gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t * recv_frag;
    bool matched;
    mca_ptl_base_header_t *header;

    header = (mca_ptl_base_header_t *)gm_ntohp(event->recv.buffer);

    /* allocate a receive fragment */
    recv_frag = mca_ptl_gm_alloc_recv_frag( (struct mca_ptl_base_module_t*)ptl );
    A_PRINT("the allocate drecv fragment is %p\n", recv_frag);

    recv_frag->frag_recv.frag_base.frag_owner = (struct mca_ptl_base_module_t*)ptl;
    recv_frag->frag_recv.frag_base.frag_peer = NULL;
    recv_frag->frag_recv.frag_request = NULL;
    recv_frag->frag_recv.frag_is_buffered = false;
    recv_frag->frag_hdr_cnt = 0;
    recv_frag->frag_msg_cnt = 0;
    recv_frag->frag_ack_pending = false;
    recv_frag->frag_progressed = 0;

    recv_frag->frag_recv.frag_base.frag_header = *header;
    recv_frag->frag_recv.frag_base.frag_addr =
        (char *) header + sizeof(mca_ptl_base_frag_header_t);
    recv_frag->frag_recv.frag_base.frag_size = header->hdr_frag.hdr_frag_length;

    recv_frag->matched = false;
    recv_frag->have_allocated_buffer = false;
    recv_frag->ptl = ptl;

    matched = ptl->super.ptl_match( &(ptl->super),
                                    &(recv_frag->frag_recv),
                                    &(recv_frag->frag_recv.frag_base.frag_header.hdr_match) );
    if( matched ) {
        return NULL;
    }

    GM_DBG(PTL_GM_DBG_COMM,
           "matching receive not yet posted get tag %d comm %d source %d\n",
           header->hdr_match.hdr_tag, header->hdr_match.hdr_contextid, header->hdr_match.hdr_src );
    return recv_frag;
}

void mca_ptl_gm_outstanding_recv( struct mca_ptl_gm_module_t *ptl )
{
    mca_ptl_gm_recv_frag_t * frag = NULL;
    int  size;
    bool matched; 
    
    size = ompi_list_get_size (&ptl->gm_recv_outstanding_queue);
    
    if (size > 0) {
        frag = (mca_ptl_gm_recv_frag_t *)
	    ompi_list_remove_first( (ompi_list_t *)&(ptl->gm_recv_outstanding_queue) );
	
	
        GM_DBG(PTL_GM_DBG_COMM," the frag size to be matched is %ld\n",frag->frag_recv.frag_base.frag_size);
        matched = ptl->super.ptl_match( &(ptl->super),
					&(frag->frag_recv),
					&(frag->frag_recv.frag_base.frag_header.hdr_match) );
	
        GM_DBG(PTL_GM_DBG_COMM,"the value of matched is %d\n", matched);
	
        if(!matched) {
	    ompi_list_append((ompi_list_t *)&(ptl->gm_recv_outstanding_queue),
			     (ompi_list_item_t *) frag);
        } else {
	    /* if allocated buffer, free the buffer */
	    /* return the recv descriptor to the free list */
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_recv_frags_free), (ompi_list_item_t *)frag);   
        }  
    }
} 

static inline
mca_ptl_gm_recv_frag_t* ptl_gm_handle_recv( struct mca_ptl_gm_module_t *ptl, gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t* frag = NULL;
    mca_ptl_base_header_t *header;
    
    header = (mca_ptl_base_header_t *)gm_ntohp(event->recv.buffer);
    
    switch(header->hdr_common.hdr_type) {
    case MCA_PTL_HDR_TYPE_MATCH:
	frag = mca_ptl_gm_recv_frag_match( ptl, event );
	break;
    case MCA_PTL_HDR_TYPE_FRAG:
        frag = mca_ptl_gm_recv_frag_frag( ptl, event );
        break;
	
    case MCA_PTL_HDR_TYPE_ACK:
    case MCA_PTL_HDR_TYPE_NACK:
    case MCA_PTL_HDR_TYPE_FIN:
        ptl_gm_ctrl_frag(ptl,header);
        break;
    default:
        ompi_output(0,"[%s:%d] unexpected frag type %d\n",
                    __FILE__,__LINE__,header->hdr_common.hdr_type);
        break;
    }
    return frag;
}

int mca_ptl_gm_analyze_recv_event( struct mca_ptl_gm_module_t* ptl, gm_recv_event_t* event )
{
    void * mesg;
    mca_ptl_gm_recv_frag_t * frag;
    
    switch (gm_ntohc(event->recv.type)) {
    case GM_RECV_EVENT:
    case GM_HIGH_RECV_EVENT:
    case GM_PEER_RECV_EVENT:
    case GM_HIGH_PEER_RECV_EVENT:
	mesg = gm_ntohp(event->recv.buffer);
	frag = ptl_gm_handle_recv( ptl, event );
	GM_DBG(PTL_GM_DBG_COMM,"FINISHED HANDLING INCOMING EVENT\n");
	
	if( (frag != NULL) && !(frag->matched) ) {
	    /* allocate temporary buffer: temporary until the fragment will be finally matched */
	    char* buffer = malloc( GM_BUF_SIZE );
	    if (NULL == buffer) {
		ompi_output(0, "[%s:%d] error in allocating memory \n", __FILE__, __LINE__);
	    }
	    /* copy the data from the registered buffer to the newly allocated one */
	    memcpy( buffer, mesg, gm_ntohl(event->recv.length) );
	    /* associate the buffer with the unexpected fragment */
	    frag->frag_recv.frag_base.frag_addr = (void *)buffer;
	    /* mark the fragment as having pending buffers */
	    frag->have_allocated_buffer = true;
	}
	gm_provide_receive_buffer( ptl->gm_port, mesg, GM_SIZE, GM_LOW_PRIORITY );
	break;
    case GM_NO_RECV_EVENT:
	break;
	
    default:
	gm_unknown(ptl->gm_port, event);
	
    }
    
    return 0;
}

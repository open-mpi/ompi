/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
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

static void send_continue_callback( struct gm_port *port, void * context, gm_status_t status )
{
    mca_ptl_gm_module_t* gm_ptl;
    mca_ptl_gm_send_frag_t* frag;
    mca_ptl_base_header_t* header;

    header = (mca_ptl_base_header_t*)context;

    frag = header->hdr_frag.hdr_src_ptr.pval;
    gm_ptl = (mca_ptl_gm_module_t *)frag->send_frag.frag_base.frag_owner;

    switch( status ) {
    case GM_SUCCESS:
	/*OMPI_OUTPUT( (0, "[%s:%d] send_continue_callback release header %p from fragment %p (available %d)\n",
	  __FILE__, __LINE__, (void*)header, (void*)frag, gm_ptl->num_send_tokens) );*/
	frag->already_send += header->hdr_frag.hdr_frag_length;
	OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_send_dma_frags), ((ompi_list_item_t*)header) );
	/* release the send token */
	ompi_atomic_add( &(gm_ptl->num_send_tokens), 1 );

	if( frag->already_send >= frag->send_frag.frag_base.frag_size ) {
	    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_send_frags), ((ompi_list_item_t*)frag) );
	}
	break;
    case GM_SEND_TIMED_OUT:
	printf( "send_continue timed out\n" );
	break;
    case GM_SEND_DROPPED:
	printf( "send_continue dropped\n" );
	break;
    default:
	printf( "send_continue other error %d\n", status );
    }
}

int mca_ptl_gm_peer_send_continue( mca_ptl_gm_peer_t *ptl_peer,
				   mca_ptl_gm_send_frag_t *fragment, 
				   struct mca_pml_base_send_request_t *sendreq, 
				   size_t offset,
				   size_t *size, 
				   int flags,
				   void * target_buffer,
				   int bytes )
{
    mca_ptl_base_header_t hdr;
    size_t update_offset = offset, header_length = sizeof(mca_ptl_base_frag_header_t);
    ompi_list_item_t* item;
    int rc = 0;

    /*OMPI_OUTPUT( (0, "[%s:%d]mca_ptl_gm_peer_send_continue peer %p fragment %p send request %p\n\toffset %ld size %ld flags %d target buffer %p bytes %d\n",
		  __FILE__, __LINE__, (void*)ptl_peer, (void*)fragment, (void*)sendreq, offset, *size,
		  flags, target_buffer, bytes) );*/

    hdr.hdr_frag.hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
    hdr.hdr_frag.hdr_common.hdr_flags = flags;
    hdr.hdr_frag.hdr_frag_length = *size;
    hdr.hdr_frag.hdr_frag_offset = update_offset;
    hdr.hdr_frag.hdr_src_ptr.lval = 0L;  /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
    hdr.hdr_frag.hdr_src_ptr.pval = fragment;
    hdr.hdr_frag.hdr_dst_ptr = sendreq->req_peer_match;
    
    fragment->send_frag.frag_base.frag_addr =(void *)target_buffer;
    fragment->send_frag.frag_base.frag_size = bytes; 
    fragment->already_send = 0;
    /* must update the offset after actual fragment size is determined
     * before attempting to send the fragment
     */
    mca_pml_base_send_request_offset( sendreq,
				      fragment->send_frag.frag_base.frag_size );

    /* The first DMA memory buffer has been alocated in same time as the fragment */
    item = (ompi_list_item_t*)fragment->send_buf;

    if( (*size) <= (5 * GM_BUF_SIZE) ) {  /* small protocol */
	size_t max_data;
	int freeAfter;
	unsigned int in_size;
	struct iovec iov;
	ompi_convertor_t *convertor = &(fragment->send_frag.frag_base.frag_convertor);
	
	/* If we have an eager send then we should send the rest of the data. */
        while( 0 == rc ) {
	    if( NULL == item ) {
		OMPI_FREE_LIST_WAIT( &(ptl_peer->peer_ptl->gm_send_dma_frags), item, rc );
		ompi_atomic_sub( &(ptl_peer->peer_ptl->num_send_tokens), 1 );
	    }
            iov.iov_base = (char*)item + header_length;
            iov.iov_len = GM_BUF_SIZE - header_length;
            max_data = iov.iov_len;
            in_size = 1;

            if((rc = ompi_convertor_pack(convertor, &(iov), &in_size, &max_data, &freeAfter)) < 0)
                return OMPI_ERROR;

	    hdr.hdr_frag.hdr_frag_offset = update_offset;
	    hdr.hdr_frag.hdr_frag_length = iov.iov_len;
	    update_offset += iov.iov_len;
	    *(mca_ptl_base_frag_header_t*)item = hdr.hdr_frag;

            gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, item,
                                           GM_SIZE, iov.iov_len + sizeof(mca_ptl_gm_eager_header_t),
					   GM_LOW_PRIORITY, ptl_peer->local_id,
                                           send_continue_callback, (void*)item );
	    item = NULL;  /* force to retrieve a new one on the next loop */
        }
        *size = update_offset - offset;
	if( !(flags & MCA_PTL_FLAGS_ACK) ) {
	    ptl_peer->peer_ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl_peer->peer_ptl,
							 fragment->send_frag.frag_request,
							 (*size) );
	}
    } else {
	/* Large set of data => we have to setup a rendez-vous protocol. Here we can
	 * use the match header already filled in by the upper level and just complete it
	 * with the others informations.
	 */
        /* For large messages I prefer to use the read semantics. It does not work well
         * if the receiver does not post the message shortly after it receive the
         * rdv (as the memory on the sender will be still locked). But the protocol
         * is easier to implement.
         */
        gm_status_t status;
	ompi_ptr_t* local_address;

        status = gm_register_memory( ptl_peer->peer_ptl->gm_port,
                                     target_buffer /*sendreq->req_base.req_addr */,
                                     (*size) );
        if( status != GM_SUCCESS ) {
            printf( "Cannot register memory from %p length %ud bytes\n",
                    (void*)sendreq->req_base.req_addr, (*size) );
            return OMPI_ERROR;
        }
	*(mca_ptl_base_frag_header_t*)item = hdr.hdr_frag;

	local_address = (ompi_ptr_t*)((char*)item + header_length);
	local_address->lval = 0;
	local_address->pval = target_buffer /*(void*)sendreq->req_base.req_addr */;

        gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, item,
                                       GM_SIZE, header_length + sizeof(ompi_ptr_t), GM_LOW_PRIORITY,
                                       ptl_peer->local_id,
                                       send_continue_callback, (void *)item );
    }

    return OMPI_SUCCESS;
}

/* At this point the header is already filled up with informations for matching.
 * Now depending on the quantity of data that have to be transfered and on the flags
 * we will add more informations on the header.
 */
int mca_ptl_gm_peer_send( mca_ptl_gm_peer_t *ptl_peer,
			  mca_ptl_gm_send_frag_t *fragment, 
			  struct mca_pml_base_send_request_t *sendreq, 
			  size_t offset,
			  size_t *size, 
			  int flags ) 
{
    struct iovec iov;
    size_t size_in, size_out;
    int header_length;
    mca_ptl_base_header_t* hdr;
    ompi_convertor_t *convertor = NULL;
    int rc, freeAfter;
    unsigned int in_size, max_data = 0;

    hdr = (mca_ptl_base_header_t*)fragment->send_buf;
    size_in = *size;

    fragment->send_frag.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->send_frag.frag_base.frag_peer = (struct mca_ptl_base_peer_t*)ptl_peer;
    fragment->send_frag.frag_request = sendreq;
    fragment->already_send = 0;

    /* At this point the header is already filled up with informations as a match header */
    if( (flags & MCA_PTL_FLAGS_ACK) || (0 == offset) ) {
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_match.hdr_contextid  = sendreq->req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src        = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst        = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag        = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq    = sendreq->req_base.req_sequence;
	if( flags & MCA_PTL_FLAGS_ACK ) {
	    header_length = sizeof(mca_ptl_base_rendezvous_header_t);
	    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_RNDV;
	    hdr->hdr_rndv.hdr_frag_length = size_in;
	    hdr->hdr_rndv.hdr_src_ptr.lval = 0;
	    hdr->hdr_rndv.hdr_src_ptr.pval = fragment;
	} else {
	    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
	    header_length = sizeof(mca_ptl_base_match_header_t);
	}
    } else {
	header_length = sizeof(mca_ptl_base_frag_header_t);
	hdr->hdr_frag.hdr_common.hdr_type  = MCA_PTL_HDR_TYPE_FRAG;
	hdr->hdr_frag.hdr_common.hdr_flags = flags;
	hdr->hdr_frag.hdr_frag_length      = size_in;
	hdr->hdr_frag.hdr_frag_offset      = offset;
	hdr->hdr_frag.hdr_src_ptr.lval     = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
	hdr->hdr_frag.hdr_src_ptr.pval     = fragment;
	hdr->hdr_frag.hdr_dst_ptr          = sendreq->req_peer_match;
    }

    iov.iov_len = 0;  /* nothing yet */

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

	if( (size_in + header_length) <= GM_BUF_SIZE ) 
	    iov.iov_len = size_in;
	else
	    iov.iov_len = GM_BUF_SIZE - header_length;
	
	/* copy the data to the registered buffer */
	iov.iov_base = ((char*)fragment->send_buf) + header_length;
	max_data = iov.iov_len;
	in_size = 1;
	if((rc = ompi_convertor_pack(convertor, &(iov), &in_size, &max_data, &freeAfter)) < 0)
	    return OMPI_ERROR;

	fragment->send_frag.frag_base.frag_addr = ((char*)fragment->send_buf) + header_length;
	fragment->send_frag.frag_base.frag_size = max_data;
    } else {
	fragment->send_frag.frag_base.frag_addr = NULL;
	fragment->send_frag.frag_base.frag_size = 0;
    }
    
    /* adjust size and request offset to reflect actual number of bytes
     * packed by convertor
     */
    size_out = iov.iov_len + header_length;
    
    DO_DEBUG( printf( "send pointer %p SIZE %d length %lu\n",
                      (void*)fragment->send_buf, GM_BUF_SIZE, size_out ) );

    /* must update the offset after actual fragment size is determined
     * before attempting to send the fragment
     */
    mca_pml_base_send_request_offset( sendreq,
				      fragment->send_frag.frag_base.frag_size );

    /* Send the first fragment */
    gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, fragment->send_buf, 
				   GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
				   send_callback, (void *)fragment );

    if( !(flags & MCA_PTL_FLAGS_ACK) ) {
	ptl_peer->peer_ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl_peer->peer_ptl,
						     fragment->send_frag.frag_request,
						     size_out );
    }
    *size = size_out - header_length;

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

    switch  (status) {
    case GM_SUCCESS:
        /* local put completed, mark put as complete */
        ompi_atomic_add( &(ptl->num_send_tokens), 1 );
        putfrag->put_sent = 1;

        /* deregister the user memory */
       status = gm_deregister_memory(ptl->gm_port, (char *)(putfrag->registered_buf), bytes2);

       if(GM_SUCCESS != status) {
    	   ompi_output(0," unpinning memory failed\n");
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
    mca_ptl_base_header_t* header;
    int hdr_type, hdr_flags;
    size_t hdr_dst_size;

    frag = (mca_ptl_gm_send_frag_t *)context;
    ptl = (mca_ptl_gm_module_t *)frag->send_frag.frag_base.frag_owner;
 
    header = (mca_ptl_base_header_t*)frag->send_buf;
    frag->send_buf = NULL;
    hdr_type = header->hdr_common.hdr_type;
    hdr_flags = header->hdr_common.hdr_flags;
    hdr_dst_size = header->hdr_ack.hdr_dst_size;

    switch  (status) {
    case GM_SUCCESS:
	/* release the send DMA buffer as soon as possible */
	OMPI_FREE_LIST_RETURN(&(ptl->gm_send_dma_frags), ((ompi_list_item_t *)header));
	/* release the send token */
	ompi_atomic_add( &(ptl->num_send_tokens), 1 );

        switch( hdr_type ) {
        case MCA_PTL_HDR_TYPE_FRAG:
        case MCA_PTL_HDR_TYPE_MATCH:
	    if( !(hdr_flags & MCA_PTL_FLAGS_ACK) ) {
		/* Return sendfrag to free list */
		OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t*)frag));
	    }
            break;
        case MCA_PTL_HDR_TYPE_RNDV:
	    /*OMPI_OUTPUT( (0, "[%s:%d] send_callback release header %p from fragment %p (available %d)\n",
	      __FILE__, __LINE__, (void*)header, (void*)frag, ptl->num_send_tokens) );*/
            /* As we actually use the read semantics for long messages, we dont
             * have to do anything special here except to release the DMA memory buffer.
             */
            break;
        case MCA_PTL_HDR_TYPE_ACK:
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t*)frag));
            break;
        case MCA_PTL_HDR_TYPE_FIN:
	    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, frag->send_frag.frag_request, 
					  hdr_dst_size);
	    
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t*)frag));
            break;
        default:
	    /* Not going to call progress on this send,
	     * and not free-ing descriptor */
	    frag->send_complete = 1;
        }
	break;
	
    case GM_SEND_TIMED_OUT:
        /* need to take care of retransmission */
        break;
	
    case GM_SEND_DROPPED:
        /* need to handle this case */
        break;
	
    default:
        ompi_output( 0, "[%s:%d] error in message completion\n", __FILE__, __LINE__ );
        break;
    }
}

static void ptl_gm_ctrl_frag(struct mca_ptl_gm_module_t *ptl, mca_ptl_base_header_t * header)
{
    mca_ptl_gm_send_frag_t * frag;
    mca_pml_base_send_request_t *req;
    mca_pml_base_recv_request_t *request;
    int status;
  
    if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_ACK) {
	frag = (mca_ptl_gm_send_frag_t *)(header->hdr_ack.hdr_src_ptr.pval);
	req = (mca_pml_base_send_request_t *) frag->req;
	assert(req != NULL);
	req->req_peer_match = header->hdr_ack.hdr_dst_match;
	req->req_peer_addr = header->hdr_ack.hdr_dst_addr;
	req->req_peer_size = header->hdr_ack.hdr_dst_size;
	frag->wait_for_ack = 0;
	
	if( (req->req_peer_size != 0) && (req->req_peer_addr.pval == NULL) ) {
	    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
					  frag->send_frag.frag_request,
					  frag->send_frag.frag_base.frag_size ); 
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), (ompi_list_item_t *)frag);
	}
    } else if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_FIN) {
	request = (mca_pml_base_recv_request_t*)header->hdr_ack.hdr_dst_match.pval;
	/* call receive progress and indicate the recv has been completed  */
	ptl->super.ptl_recv_progress( (mca_ptl_base_module_t *) ptl,
                                      request , 
                                      header->hdr_ack.hdr_dst_size,
                                      header->hdr_ack.hdr_dst_size );
	/* deregister the memory */
	status = gm_deregister_memory( ptl->gm_port,
				       header->hdr_ack.hdr_dst_addr.pval,
				       header->hdr_ack.hdr_dst_size );
	
	if(GM_SUCCESS != status) {
	    ompi_output(0," unpinning memory failed\n");
	}
    } else {
	OMPI_OUTPUT((0, "Unkonwn header type in ptl_gm_ctrl_frag\n"));
    }
}

/* We get a RNDV header in two situations:
 * - when the remote node need a ack
 * - when we set a rendez-vous protocol with the remote node.
 * In both cases we have to send an ack back.
 */
static mca_ptl_gm_recv_frag_t*
mca_ptl_gm_recv_frag_match( struct mca_ptl_gm_module_t *ptl,
			    gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t* recv_frag;
    bool matched;
    mca_ptl_base_header_t* hdr;
    
    hdr = (mca_ptl_base_header_t*)gm_ntohp(event->recv.buffer);
    
    /* allocate a receive fragment */
    recv_frag = mca_ptl_gm_alloc_recv_frag( (struct mca_ptl_base_module_t*)ptl );
    
    recv_frag->frag_recv.frag_base.frag_owner = (struct mca_ptl_base_module_t*)ptl;
    recv_frag->frag_recv.frag_base.frag_peer = NULL;
    recv_frag->frag_recv.frag_request = NULL;
    recv_frag->frag_recv.frag_is_buffered = false;
    recv_frag->frag_hdr_cnt = 0;
    recv_frag->frag_msg_cnt = 0;
    recv_frag->frag_ack_pending = false;
    recv_frag->frag_progressed = 0;
    
    recv_frag->frag_recv.frag_base.frag_header.hdr_rndv = hdr->hdr_rndv;

    if( MCA_PTL_HDR_TYPE_MATCH == hdr->hdr_rndv.hdr_match.hdr_common.hdr_type ) {
        recv_frag->frag_recv.frag_base.frag_addr =
            (char *) hdr + sizeof(mca_ptl_base_match_header_t);
	recv_frag->frag_recv.frag_base.frag_size = hdr->hdr_rndv.hdr_match.hdr_msg_length;
    } else {
        assert( MCA_PTL_HDR_TYPE_RNDV == hdr->hdr_rndv.hdr_match.hdr_common.hdr_type );
        recv_frag->frag_recv.frag_base.frag_addr =
            (char *) hdr + sizeof(mca_ptl_base_rendezvous_header_t);
	recv_frag->frag_recv.frag_base.frag_size = hdr->hdr_rndv.hdr_frag_length;
    }
    
    recv_frag->matched = false;
    recv_frag->have_allocated_buffer = false;
    recv_frag->ptl = ptl;
    
    matched = ptl->super.ptl_match( &(ptl->super),
                                    &(recv_frag->frag_recv),
                                    &(recv_frag->frag_recv.frag_base.frag_header.hdr_match) );
    if( matched ) {
        size_t length = recv_frag->frag_recv.frag_base.frag_size;
	/* get some memory and copy the data inside. We can then release the receive buffer */
        char* ptr = (char*)malloc( sizeof(char) * length );
        recv_frag->have_allocated_buffer = true;
        memcpy( ptr, recv_frag->frag_recv.frag_base.frag_addr, length );
        recv_frag->frag_recv.frag_base.frag_addr = ptr;
        return NULL;
    }
    return recv_frag;
}

/* This function get called when the gm_get is finish (i.e. when the read from remote memory
 * is completed. We have to send back the ack. If the original data was too large for just one
 * fragment it will be split in severals. We have to send back for each of these fragments one
 * ack.
 */
static void mca_ptl_gm_get_callback( struct gm_port *port, void * context, gm_status_t status )
{
    mca_ptl_gm_module_t* gm_ptl;
    mca_ptl_gm_recv_frag_t* frag;
    mca_ptl_gm_send_frag_t *ack;
    mca_pml_base_recv_request_t *request;
    mca_ptl_gm_peer_t* peer;
    int rc;

    frag = (mca_ptl_gm_recv_frag_t*)context;
    gm_ptl = (mca_ptl_gm_module_t *)frag->frag_recv.frag_base.frag_owner;
    request = frag->frag_recv.frag_request;
    peer = (mca_ptl_gm_peer_t*)frag->frag_recv.frag_base.frag_peer;
    
    switch( status ) {
    case GM_SUCCESS:
        /*OMPI_OUTPUT( (0, "[%s:%d] mca_ptl_gm_get_callback release header %p from fragment %p (available %\d)\n",
          __FILE__, __LINE__, (void*)header, (void*)frag, gm_ptl->num_send_tokens) );*/
	ack = mca_ptl_gm_alloc_send_frag( gm_ptl, NULL );
	rc = mca_ptl_gm_send_ack_init( ack, gm_ptl, 
				       (mca_ptl_gm_peer_t *)(frag->frag_recv.frag_base.frag_peer),
				       frag, NULL,
				       frag->frag_recv.frag_base.frag_size );
	    
	gm_send_to_peer_with_callback( ((mca_ptl_gm_module_t*)(ack->send_frag.frag_base.frag_owner))->gm_port,
				       ack->send_buf, GM_SIZE, sizeof(mca_ptl_base_ack_header_t),
				       GM_LOW_PRIORITY, peer->local_id, send_callback, (void*)ack );
        break;
    case GM_SEND_TIMED_OUT:
        printf( "mca_ptl_gm_get_callback timed out\n" );
        break;
    case GM_SEND_DROPPED:
        printf( "mca_ptl_gm_get_callback dropped\n" );
        break;
    default:
        printf( "mca_ptl_gm_get_callback other error %d\n", status );
    }
}

static mca_ptl_gm_recv_frag_t*
mca_ptl_gm_recv_frag_frag( struct mca_ptl_gm_module_t *ptl,
                           gm_recv_event_t* event )
{
    mca_pml_base_recv_request_t *request;
    ompi_convertor_t* convertor;
    mca_ptl_base_header_t *hdr;
    struct iovec iov;
    uint32_t iov_count, max_data;
    int32_t freeAfter, rc;
    mca_ptl_gm_recv_frag_t* recv_frag;

    hdr = (mca_ptl_base_header_t *)gm_ntohp(event->recv.buffer);

    recv_frag = (mca_ptl_gm_recv_frag_t*)hdr->hdr_frag.hdr_dst_ptr.pval;
    request = (mca_pml_base_recv_request_t*)recv_frag->frag_recv.frag_request;
    /* here we can have a synchronisation problem if several threads work in same time
     * with the same request. The only question is if it's possible ?
     */
    convertor = &(recv_frag->frag_recv.frag_base.frag_convertor);
    ompi_convertor_init_for_recv( convertor, 0,
				  request->req_base.req_datatype,
				  request->req_base.req_count,
				  request->req_base.req_addr,
				  hdr->hdr_frag.hdr_frag_offset, NULL );

    if( hdr->hdr_frag.hdr_frag_length <= (GM_BUF_SIZE - sizeof(mca_ptl_base_frag_header_t)) ) {
	iov.iov_base = (char*)hdr + sizeof(mca_ptl_base_frag_header_t);
	iov.iov_len = hdr->hdr_frag.hdr_frag_length;
	iov_count = 1;
	max_data = hdr->hdr_frag.hdr_frag_length;
	freeAfter = 0;  /* unused here */
	rc = ompi_convertor_unpack( convertor, &iov, &iov_count, &max_data, &freeAfter );
	assert( 0 == freeAfter );
	if( (hdr->hdr_frag.hdr_frag_offset + hdr->hdr_frag.hdr_frag_length) >=
	    recv_frag->frag_recv.frag_base.frag_size ) {
	    /* update the request status if we are done */
	    ptl->super.ptl_recv_progress( (mca_ptl_base_module_t*)ptl, request, max_data, max_data );
	    OBJ_RELEASE( recv_frag );
 	}
    } else {
	gm_status_t status;
	ompi_ptr_t* remote_memory = (ompi_ptr_t*)((char*)hdr + sizeof(mca_ptl_base_frag_header_t));
	mca_ptl_gm_peer_t* peer;

	status = gm_register_memory( ptl->gm_port,
				     (char*)request->req_base.req_addr + hdr->hdr_frag.hdr_frag_offset,
				     hdr->hdr_frag.hdr_frag_length );
	if( status != GM_SUCCESS ) {
	    printf( "Cannot register memory from %p length %lld bytes\n",
                    (void*)request->req_base.req_addr, hdr->hdr_frag.hdr_frag_length );
            return NULL;
	}

	gm_get( ptl->gm_port, remote_memory->lval,
		recv_frag->frag_recv.frag_base.frag_addr,
		recv_frag->frag_recv.frag_base.frag_size,
		GM_HIGH_PRIORITY, peer->peer_addr->global_id, peer->peer_addr->port_id,
		mca_ptl_gm_get_callback, recv_frag );
    }

    return NULL;
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
	
	
        matched = ptl->super.ptl_match( &(ptl->super),
					&(frag->frag_recv),
					&(frag->frag_recv.frag_base.frag_header.hdr_match) );
	
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
    case MCA_PTL_HDR_TYPE_RNDV:
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
        ompi_output( 0, "[%s:%d] unexpected frag type %d\n",
		     __FILE__, __LINE__, header->hdr_common.hdr_type );
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
    case GM_PEER_RECV_EVENT:
    case GM_HIGH_RECV_EVENT:
    case GM_HIGH_PEER_RECV_EVENT:
	mesg = gm_ntohp(event->recv.buffer);
	frag = ptl_gm_handle_recv( ptl, event );
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

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
#include "mca/pml/teg/src/pml_teg_proc.h"

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
	if( frag->send_frag.frag_base.frag_size <= mca_ptl_gm_component.gm_eager_limit ) {
	    /* small message */
	    frag->frag_bytes_validated += header->hdr_frag.hdr_frag_length;
	}

	OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_send_dma_frags), ((ompi_list_item_t*)header) );
	/* release the send token */
	ompi_atomic_add( &(gm_ptl->num_send_tokens), 1 );

	if( frag->frag_bytes_validated >= frag->send_frag.frag_base.frag_size ) {
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

static void send_continue_short_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_ptl_gm_module_t* gm_ptl;
    mca_ptl_base_frag_t* frag_base;
    mca_ptl_base_header_t* header;

    header = (mca_ptl_base_header_t*)context;

    frag_base = (mca_ptl_base_frag_t*)header->hdr_frag.hdr_src_ptr.pval;
    gm_ptl = (mca_ptl_gm_module_t *)frag_base->frag_owner;

    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_send_dma_frags), ((ompi_list_item_t*)header) );
    /* release the send token */
    ompi_atomic_add( &(gm_ptl->num_send_tokens), 1 );
}

static inline
int mca_ptl_gm_send_next_long_segment( mca_ptl_gm_send_frag_t* frag,
				       int flags )
{
    struct mca_ptl_gm_peer_t* ptl_peer;
    gm_status_t status;
    char* pointer;
    uint64_t length;
    int32_t hdr_flags = 0;
    mca_ptl_gm_frag_header_t* hdr;

    ptl_peer = (struct mca_ptl_gm_peer_t*)frag->send_frag.frag_base.frag_peer;

    length = frag->send_frag.frag_base.frag_size - frag->frag_bytes_processed;
    if( length <= mca_ptl_gm_component.gm_rdma_frag_size ) {
	hdr_flags = PTL_FLAG_GM_LAST_FRAGMENT;
    } else {
	length = mca_ptl_gm_component.gm_rdma_frag_size;
    }
    pointer = (char*)frag->send_frag.frag_base.frag_addr + frag->frag_offset + frag->frag_bytes_processed;

    if( flags & GM_PTL_SEND_MESSAGE ) {
	ompi_list_item_t* item;
	int32_t rc;
	
	OMPI_FREE_LIST_GET( &(ptl_peer->peer_ptl->gm_send_dma_frags), item, rc );
	hdr = (mca_ptl_gm_frag_header_t*)item;

	hdr->hdr_frag.hdr_common.hdr_type  = MCA_PTL_HDR_TYPE_FRAG;
	hdr->hdr_frag.hdr_common.hdr_flags = frag->send_frag.frag_base.frag_header.hdr_common.hdr_flags | hdr_flags;
	hdr->hdr_frag.hdr_src_ptr.lval     = 0L;  /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
	hdr->hdr_frag.hdr_src_ptr.pval     = frag;
	hdr->hdr_frag.hdr_dst_ptr          = frag->send_frag.frag_base.frag_header.hdr_ack.hdr_dst_match;
	hdr->hdr_frag.hdr_frag_offset      = frag->frag_bytes_processed;
	hdr->hdr_frag.hdr_frag_length      = length;
	hdr->registered_memory.lval        = 0L;
	hdr->registered_memory.pval        = pointer;

	gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, hdr,
				       GM_SIZE, sizeof(mca_ptl_gm_frag_header_t),
				       GM_LOW_PRIORITY, ptl_peer->local_id,
				       send_continue_callback, (void*)hdr );
	frag->frag_bytes_processed += length;
	pointer += length;
	length = frag->send_frag.frag_base.frag_size - frag->frag_bytes_processed;
	if( length > mca_ptl_gm_component.gm_rdma_frag_size )
	    length = mca_ptl_gm_component.gm_rdma_frag_size;
    }

    if( (flags & GM_PTL_REGISTER_MEMORY) && (0 != length) ) {
	status = gm_register_memory( ptl_peer->peer_ptl->gm_port, pointer, length );
	if( status != GM_SUCCESS ) {
	    ompi_output( 0, "Cannot register sender memory (%p, %lld) bytes offset %d\n",
			 pointer, length, frag->frag_bytes_processed );
	    return OMPI_ERROR;
	}
    }
    
    return OMPI_SUCCESS;
}

int mca_ptl_gm_peer_send_continue( mca_ptl_gm_peer_t *ptl_peer,
				   mca_ptl_gm_send_frag_t *fragment, 
				   struct mca_pml_base_send_request_t *sendreq, 
				   size_t offset,
				   size_t *size, 
				   int flags )
{
    mca_ptl_gm_frag_header_t* hdr;
    uint64_t remaining_bytes;
    ompi_list_item_t *item;
    int rc = 0;

    fragment->frag_offset = offset;

    /* must update the offset after actual fragment size is determined
     * before attempting to send the fragment
     */
    mca_pml_base_send_request_offset( sendreq,
				      fragment->send_frag.frag_base.frag_size );

    /* The first DMA memory buffer has been alocated in same time as the fragment */
    item = (ompi_list_item_t*)fragment->send_buf;
    hdr = (mca_ptl_gm_frag_header_t*)item;
    remaining_bytes = fragment->send_frag.frag_base.frag_size - fragment->frag_bytes_processed;

    if( remaining_bytes <= mca_ptl_gm_component.gm_eager_limit ) {  /* small protocol */
	int32_t freeAfter;
	uint32_t max_data, in_size;
	struct iovec iov;
	ompi_convertor_t *convertor = &(fragment->send_frag.frag_base.frag_convertor);
	
	/* If we have an eager send then we should send the rest of the data. */
        while( 0 < remaining_bytes ) {
	    if( NULL == item ) {
		OMPI_FREE_LIST_WAIT( &(ptl_peer->peer_ptl->gm_send_dma_frags), item, rc );
		ompi_atomic_sub( &(ptl_peer->peer_ptl->num_send_tokens), 1 );
		hdr = (mca_ptl_gm_frag_header_t*)item;
	    }
            iov.iov_base = (char*)item + sizeof(mca_ptl_base_frag_header_t);
            iov.iov_len = mca_ptl_gm_component.gm_segment_size - sizeof(mca_ptl_base_frag_header_t);
	    if( iov.iov_len >= remaining_bytes ) 
		iov.iov_len = remaining_bytes;
            max_data = iov.iov_len;
            in_size = 1;

            if( ompi_convertor_pack(convertor, &(iov), &in_size, &max_data, &freeAfter) < 0)
                return OMPI_ERROR;

	    hdr->hdr_frag.hdr_common.hdr_type  = MCA_PTL_HDR_TYPE_FRAG;
	    hdr->hdr_frag.hdr_common.hdr_flags = flags;
	    hdr->hdr_frag.hdr_src_ptr.lval     = 0L;  /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
	    hdr->hdr_frag.hdr_src_ptr.pval     = fragment;
	    hdr->hdr_frag.hdr_dst_ptr          = sendreq->req_peer_match;
	    hdr->hdr_frag.hdr_frag_offset      = fragment->frag_offset + fragment->frag_bytes_processed;
	    hdr->hdr_frag.hdr_frag_length      = iov.iov_len;

	    fragment->frag_bytes_processed += iov.iov_len;
	    remaining_bytes -= iov.iov_len;
	    if( remaining_bytes == 0 )
		hdr->hdr_frag.hdr_common.hdr_flags |= PTL_FLAG_GM_LAST_FRAGMENT;

	    /* for the last piece set the header type to FIN */
            gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, hdr,
                                           GM_SIZE, iov.iov_len + sizeof(mca_ptl_base_frag_header_t),
					   GM_LOW_PRIORITY, ptl_peer->local_id,
                                           send_continue_callback, (void*)hdr );
	    item = NULL;  /* force to retrieve a new one on the next loop */
        }
        *size = fragment->frag_bytes_processed;
	if( !(flags & MCA_PTL_FLAGS_ACK) ) {
	    ptl_peer->peer_ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl_peer->peer_ptl,
							 fragment->send_frag.frag_request,
							 (*size) );
	}
	return OMPI_SUCCESS;
    }
    /* Large set of data => we have to setup a rendez-vous protocol. Here we can
     * use the match header already filled in by the upper level and just complete it
     * with the others informations. When we reach this point the rendez-vous protocol
     * has already been realized so we know that the receiver expect our message.
     */
    hdr->hdr_frag.hdr_common.hdr_type  = MCA_PTL_HDR_TYPE_FRAG;
    hdr->hdr_frag.hdr_common.hdr_flags = flags;
    hdr->hdr_frag.hdr_src_ptr.lval     = 0L;  /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
    hdr->hdr_frag.hdr_src_ptr.pval     = fragment;
    hdr->hdr_frag.hdr_dst_ptr          = sendreq->req_peer_match;
    hdr->hdr_frag.hdr_frag_offset      = fragment->frag_offset;
    hdr->hdr_frag.hdr_frag_length      = *size;
    hdr->registered_memory.lval        = 0L;
    hdr->registered_memory.pval        = NULL; 
    
    gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, hdr,
				   GM_SIZE, sizeof(mca_ptl_base_frag_header_t) + sizeof(ompi_ptr_t),
                                   GM_LOW_PRIORITY, ptl_peer->local_id,
				   send_continue_short_callback, (void *)hdr );
    
    /* Now we are waiting for the ack message. Meanwhile we can register the sender first piece
     * of data. In this way we have a recovery between the expensive registration on both sides.
     */
    return mca_ptl_gm_send_next_long_segment( fragment, GM_PTL_REGISTER_MEMORY );
}

static void send_match_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_ptl_gm_module_t* gm_ptl;
    mca_ptl_base_header_t* header = (mca_ptl_base_header_t*)context;

    gm_ptl = (mca_ptl_gm_module_t*)((long)header->hdr_rndv.hdr_frag_length);

    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_send_dma_frags), ((ompi_list_item_t*)header) );
    /* release the send token */
    ompi_atomic_add( &(gm_ptl->num_send_tokens), 1 );
}

/* This function is used for the initial send. For small size messages the data will be attached
 * to the header, when for long size messages we will setup a rendez-vous protocol. We dont need
 * to fill a fragment description here as all that we need is the request pointer. In same time
 * even if we fill a fragment it will be lost as soon as we get the answer from the remote node
 * and we will be unable to reuse any informations stored inside (like the convertor).
 */
int mca_ptl_gm_peer_send( struct mca_ptl_base_module_t* ptl,
                          struct mca_ptl_base_peer_t* ptl_base_peer,
			  struct mca_pml_base_send_request_t *sendreq, 
			  size_t offset,
			  size_t size,
			  int flags )
{
    struct iovec iov;
    size_t size_in, size_out;
    int header_length;
    mca_ptl_base_header_t* hdr;
    ompi_convertor_t *convertor = NULL;
    int rc, freeAfter;
    unsigned int in_size, max_data = 0;
    mca_ptl_gm_peer_t* ptl_peer = (mca_ptl_gm_peer_t*)ptl_base_peer;
    ompi_list_item_t *item;
    char* sendbuf;

    OMPI_FREE_LIST_WAIT( &(((mca_ptl_gm_module_t*)ptl)->gm_send_dma_frags), item, rc );
    ompi_atomic_sub( &(((mca_ptl_gm_module_t*)ptl)->num_send_tokens), 1 );
    sendbuf = (char*)item;

    hdr = (mca_ptl_base_header_t*)item;
    size_in = size;

    /* Populate the header with the match informations */
    (void)mca_ptl_gm_init_header_match( hdr, sendreq, flags );
    header_length = sizeof(mca_ptl_base_rendezvous_header_t);
    hdr->hdr_rndv.hdr_frag_length = (uint64_t)((long)ptl);
    hdr->hdr_rndv.hdr_src_ptr.lval = 0L;
    hdr->hdr_rndv.hdr_src_ptr.pval = sendreq;
    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_RNDV;

    if( size_in > 0 ) {
	convertor = &sendreq->req_convertor;

	if( (size_in + header_length) <= mca_ptl_gm_component.gm_segment_size ) 
	    iov.iov_len = size_in;
	else
	    iov.iov_len = mca_ptl_gm_component.gm_segment_size - header_length;
	
	/* copy the data to the registered buffer */
	iov.iov_base = ((char*)hdr) + header_length;
	max_data = iov.iov_len;
	in_size = 1;
	if((rc = ompi_convertor_pack(convertor, &(iov), &in_size, &max_data, &freeAfter)) < 0)
	    return OMPI_ERROR;


	/* must update the offset after actual fragment size is determined
	 * before attempting to send the fragment
	 */
	mca_pml_base_send_request_offset( sendreq, max_data );
    } else {
	iov.iov_len = 0;  /* no data will be transmitted */
    }
    
    /* adjust size and request offset to reflect actual number of bytes
     * packed by convertor
     */
    size_out = iov.iov_len + header_length;
    
    /* Send the first fragment */
    gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, hdr,
				   GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
				   send_match_callback, (void *)hdr );

    if( !(flags & MCA_PTL_FLAGS_ACK) ) {
	ptl_peer->peer_ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl_peer->peer_ptl,
						     sendreq,
						     max_data );
    }

    return OMPI_SUCCESS;
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
        case MCA_PTL_HDR_TYPE_FIN:
	    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t*)frag));
            break;
        default:
	    /* Not going to call progress on this send, and not free-ing descriptor */
	    printf( "Called with a strange headertype ...\n" );
	    break;
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

static mca_ptl_gm_recv_frag_t*
mca_ptl_gm_ctrl_frag( struct mca_ptl_gm_module_t *ptl,
		      mca_ptl_base_header_t * header )
{
    mca_pml_base_send_request_t *req;
  
    if( MCA_PTL_HDR_TYPE_ACK == header->hdr_common.hdr_type ) {
	if( header->hdr_common.hdr_flags & PTL_FLAG_GM_HAS_FRAGMENT ) {
	    mca_ptl_gm_send_frag_t* frag = (mca_ptl_gm_send_frag_t*)(header->hdr_ack.hdr_src_ptr.pval);
	    /* update the fragment header with the most up2date informations */
	    frag->send_frag.frag_base.frag_header.hdr_ack.hdr_dst_match = header->hdr_ack.hdr_dst_match;
	    req = frag->send_frag.frag_request;
	    assert(req != NULL);
	    req->req_peer_match = header->hdr_ack.hdr_dst_match;
	    req->req_peer_addr  = header->hdr_ack.hdr_dst_addr;
	    req->req_peer_size  = header->hdr_ack.hdr_dst_size;
	    
	    if( (req->req_peer_size != 0) && (req->req_peer_addr.pval == NULL) ) {
		ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
					      req, frag->send_frag.frag_base.frag_size ); 
		OMPI_FREE_LIST_RETURN( &(ptl->gm_send_frags), (ompi_list_item_t *)frag );
	    } else {
		if( header->hdr_common.hdr_flags & PTL_FLAG_GM_HAS_FRAGMENT ) {
		    frag->send_frag.frag_base.frag_header.hdr_common.hdr_flags |= PTL_FLAG_GM_HAS_FRAGMENT;
		}
	    }
	} else {  /* initial reply to a rendez-vous request */
	    req = (mca_pml_base_send_request_t*)(header->hdr_ack.hdr_src_ptr.pval);
	    req->req_peer_match = header->hdr_ack.hdr_dst_match;
            req->req_peer_addr  = header->hdr_ack.hdr_dst_addr;
            req->req_peer_size  = header->hdr_ack.hdr_dst_size;
	    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
					  req, req->req_offset );
	}
    } else if( MCA_PTL_HDR_TYPE_NACK == header->hdr_common.hdr_type ) {
    } else {
	OMPI_OUTPUT((0, "Unkonwn header type in ptl_gm_ctrl_frag\n"));
    }
    return NULL;
}

/* We get a RNDV header in two situations:
 * - when the remote node need a ack
 * - when we set a rendez-vous protocol with the remote node.
 * In both cases we have to send an ack back.
 */
static mca_ptl_gm_recv_frag_t*
mca_ptl_gm_recv_frag_match( struct mca_ptl_gm_module_t *ptl,
			    mca_ptl_base_header_t* hdr )
{
    mca_ptl_gm_recv_frag_t* recv_frag;
    bool matched;
    uint64_t length;

    /* allocate a receive fragment */
    recv_frag = mca_ptl_gm_alloc_recv_frag( (struct mca_ptl_base_module_t*)ptl );

    if( MCA_PTL_HDR_TYPE_MATCH == hdr->hdr_rndv.hdr_match.hdr_common.hdr_type ) {
        recv_frag->frag_recv.frag_base.frag_addr =
            (char*)hdr + sizeof(mca_ptl_base_match_header_t);
    } else {
        assert( MCA_PTL_HDR_TYPE_RNDV == hdr->hdr_rndv.hdr_match.hdr_common.hdr_type );
        recv_frag->frag_recv.frag_base.frag_addr =
            (char*)hdr + sizeof(mca_ptl_base_rendezvous_header_t);
    }
    recv_frag->frag_recv.frag_base.frag_size = hdr->hdr_rndv.hdr_match.hdr_msg_length;
    recv_frag->frag_recv.frag_is_buffered = false;
    recv_frag->have_allocated_buffer = false;

    recv_frag->frag_recv.frag_base.frag_header.hdr_rndv = hdr->hdr_rndv;
    matched = ptl->super.ptl_match( &(ptl->super),
                                    &(recv_frag->frag_recv),
                                    &(recv_frag->frag_recv.frag_base.frag_header.hdr_match) );
    if( true == matched ) return NULL;  /* done and fragment already removed */

    length = mca_ptl_gm_component.gm_segment_size - sizeof(mca_ptl_base_rendezvous_header_t);
    if( recv_frag->frag_recv.frag_base.frag_size < length ) {
	length = recv_frag->frag_recv.frag_base.frag_size;
    }
    /* get some memory and copy the data inside. We can then release the receive buffer */
    if( 0 != length ) {
        char* ptr = (char*)gm_get_local_buffer();
        if (NULL == ptr) {
            ompi_output(0, "[%s:%d] error in allocating memory \n", __FILE__, __LINE__);
        }
        recv_frag->have_allocated_buffer = true;
        memcpy( ptr, recv_frag->frag_recv.frag_base.frag_addr, length );
        recv_frag->frag_recv.frag_base.frag_addr = ptr;
    } else {
        recv_frag->frag_recv.frag_base.frag_addr = NULL;
    }
    recv_frag->matched = false;
    
    return recv_frag;
}

static void recv_short_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_ptl_gm_module_t* gm_ptl;
    mca_ptl_base_frag_t* frag_base;
    mca_ptl_base_ack_header_t* header;

    header = (mca_ptl_base_ack_header_t*)context;

    frag_base = (mca_ptl_base_frag_t*)header->hdr_dst_match.pval;
    gm_ptl = (mca_ptl_gm_module_t *)frag_base->frag_owner;

    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_send_dma_frags), ((ompi_list_item_t*)header) );
    /* release the send token */
    ompi_atomic_add( &(gm_ptl->num_send_tokens), 1 );
}

static int mca_ptl_gm_send_quick_fin_message( struct mca_ptl_gm_peer_t* ptl_peer,
					      struct mca_ptl_gm_recv_frag_t* frag )
{
    ompi_list_item_t *item;
    mca_ptl_base_header_t *hdr;
    int rc;

    OMPI_FREE_LIST_WAIT( &(ptl_peer->peer_ptl->gm_send_dma_frags), item, rc );
    ompi_atomic_sub( &(ptl_peer->peer_ptl->num_send_tokens), 1 );
    hdr = (mca_ptl_base_header_t*)item;

    hdr->hdr_common.hdr_type        = MCA_PTL_HDR_TYPE_FIN;
    hdr->hdr_common.hdr_flags       = PTL_FLAG_GM_HAS_FRAGMENT;
    hdr->hdr_ack.hdr_src_ptr.pval   = frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_src_ptr.pval;
    hdr->hdr_ack.hdr_dst_match.lval = 0;
    hdr->hdr_ack.hdr_dst_match.pval = frag;
    hdr->hdr_ack.hdr_dst_addr.lval  = 0; /*we are filling both p and val of dest address */
    hdr->hdr_ack.hdr_dst_addr.pval  = NULL;
    hdr->hdr_ack.hdr_dst_size       = frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length;

    gm_send_to_peer_with_callback( ptl_peer->peer_ptl->gm_port, hdr, GM_SIZE, sizeof(mca_ptl_base_ack_header_t),
				   GM_LOW_PRIORITY, ptl_peer->local_id,
				   recv_short_callback, (void*)hdr );
    return OMPI_SUCCESS;
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
    mca_pml_base_recv_request_t *request;
    mca_ptl_gm_peer_t* peer;
    uint64_t length;
    void* pointer;

    frag = (mca_ptl_gm_recv_frag_t*)context;
    gm_ptl = (mca_ptl_gm_module_t *)frag->frag_recv.frag_base.frag_owner;
    request = frag->frag_recv.frag_request;
    peer = (mca_ptl_gm_peer_t*)frag->frag_recv.frag_base.frag_peer;
    
    switch( status ) {
    case GM_SUCCESS:
	pointer = (char*)request->req_base.req_addr + frag->frag_offset + frag->frag_bytes_validated;
	length = frag->frag_recv.frag_base.frag_size - frag->frag_bytes_validated;
	if( length > mca_ptl_gm_component.gm_rdma_frag_size )
	    length = mca_ptl_gm_component.gm_rdma_frag_size;

	/* send an ack message to the sender */
	mca_ptl_gm_send_quick_fin_message( peer, frag );

	frag->frag_bytes_validated += length;

	if( frag->frag_recv.frag_base.frag_size <= frag->frag_bytes_validated ) {
	    gm_ptl->super.ptl_recv_progress( (mca_ptl_base_module_t*)gm_ptl, request,
					     frag->frag_recv.frag_base.frag_size,
					     frag->frag_recv.frag_base.frag_size );
	    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_recv_frags_free), (ompi_list_item_t*)frag );
	}
	status = gm_deregister_memory( peer->peer_ptl->gm_port, pointer, length );
	if( GM_SUCCESS != status ) {
	    OMPI_OUTPUT( (0, "unpinning receiver memory from get (%p, %u) failed \n", pointer, length) );
	}
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
mca_ptl_gm_recv_frag_frag( struct mca_ptl_gm_module_t* ptl,
			   mca_ptl_gm_frag_header_t* hdr )
{
    mca_pml_base_recv_request_t *request;
    ompi_convertor_t local_convertor, *convertor;
    struct iovec iov;
    uint32_t iov_count, max_data;
    int32_t freeAfter, rc;
    mca_ptl_gm_recv_frag_t* recv_frag;

    if( hdr->hdr_frag.hdr_common.hdr_flags & PTL_FLAG_GM_HAS_FRAGMENT ) {
	recv_frag = (mca_ptl_gm_recv_frag_t*)hdr->hdr_frag.hdr_dst_ptr.pval;
	request = (mca_pml_base_recv_request_t*)recv_frag->frag_recv.frag_request;
	/* here we can have a synchronisation problem if several threads work in same time
	 * with the same request. The only question is if it's possible ?
	 */
	convertor = &(recv_frag->frag_recv.frag_base.frag_convertor);
    } else {
	request = (mca_pml_base_recv_request_t*)hdr->hdr_frag.hdr_dst_ptr.pval;

	if( hdr->hdr_frag.hdr_frag_length <= (mca_ptl_gm_component.gm_segment_size - sizeof(mca_ptl_base_frag_header_t)) ) {
	    ompi_proc_t* proc = ompi_comm_peer_lookup( request->req_base.req_comm,
						       request->req_base.req_ompi.req_status.MPI_SOURCE ); 
	    convertor = &local_convertor;
	    convertor->stack_size = 0;  /* dont let the convertor free the stack */
            ompi_convertor_copy( proc->proc_convertor, convertor );
	    recv_frag = NULL;
	} else {  /* large message => we have to create a receive fragment */
	    recv_frag = mca_ptl_gm_alloc_recv_frag( (struct mca_ptl_base_module_t*)ptl );
	    recv_frag->frag_recv.frag_request = request;
	    recv_frag->frag_offset = hdr->hdr_frag.hdr_frag_offset;
	    recv_frag->matched = true;
	    recv_frag->frag_recv.frag_base.frag_size = hdr->hdr_frag.hdr_frag_length;
	    recv_frag->frag_recv.frag_base.frag_peer = (struct mca_ptl_base_peer_t*)
		mca_pml_teg_proc_lookup_remote_peer( request->req_base.req_comm,
						     request->req_base.req_ompi.req_status.MPI_SOURCE,
						     (struct mca_ptl_base_module_t*)ptl );
	    convertor = &(recv_frag->frag_recv.frag_base.frag_convertor);
	}
	ompi_convertor_init_for_recv( convertor, 0,
				      request->req_base.req_datatype,
				      request->req_base.req_count,
				      request->req_base.req_addr,
				      hdr->hdr_frag.hdr_frag_offset, NULL );
    }

    if( NULL == recv_frag ) {
	iov.iov_base = (char*)hdr + sizeof(mca_ptl_base_frag_header_t);
	iov.iov_len = hdr->hdr_frag.hdr_frag_length;
	iov_count = 1;
	max_data = hdr->hdr_frag.hdr_frag_length;
	freeAfter = 0;  /* unused here */
	rc = ompi_convertor_unpack( convertor, &iov, &iov_count, &max_data, &freeAfter );
	assert( 0 == freeAfter );
	ptl->super.ptl_recv_progress( (mca_ptl_base_module_t*)ptl, request, max_data, max_data );
    } else {
	gm_status_t status;
	mca_ptl_gm_peer_t* peer = (mca_ptl_gm_peer_t*)recv_frag->frag_recv.frag_base.frag_peer;
	char* pointer = (char*)request->req_base.req_addr + recv_frag->frag_offset;
	uint64_t length = mca_ptl_gm_component.gm_rdma_frag_size;

	recv_frag->frag_recv.frag_base.frag_header.hdr_frag = hdr->hdr_frag;
	if( NULL == hdr->registered_memory.pval ) {  /* first round of the local rendez-vous protocol */
	    /* send an ack message to the sender ... quick hack (TODO) */
	    recv_frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length = 0;
	    mca_ptl_gm_send_quick_fin_message( (mca_ptl_gm_peer_t*)recv_frag->frag_recv.frag_base.frag_peer,
					       recv_frag );
	    recv_frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length = hdr->hdr_frag.hdr_frag_length;
	    if( length >= hdr->hdr_frag.hdr_frag_length )
		length = hdr->hdr_frag.hdr_frag_length;
	} else {
	    /* There is a kind of rendez-vous protocol used internally by the GM driver. If the amount of data
	     * to transfert is large enough, then the sender will start sending a frag message with the 
	     * remote_memory set to NULL (but with the length set to the length of the first fragment).
	     * It will allow the receiver to start to register it's own memory. Later when the receiver
	     * get a fragment with the remote_memory field not NULL it can start getting the data.
	     */
	    pointer += hdr->hdr_frag.hdr_frag_offset;
	    gm_get( ptl->gm_port, hdr->registered_memory.lval, pointer, hdr->hdr_frag.hdr_frag_length,
		    GM_LOW_PRIORITY, peer->local_id, peer->port_number, mca_ptl_gm_get_callback, recv_frag );
	    pointer += hdr->hdr_frag.hdr_frag_length;
	    length = recv_frag->frag_recv.frag_base.frag_size - recv_frag->frag_bytes_processed;
	    if( length > mca_ptl_gm_component.gm_rdma_frag_size )
		length = mca_ptl_gm_component.gm_rdma_frag_size;
	}

	if( 0 != length ) {
	    status = gm_register_memory( ptl->gm_port, pointer, length );
	    if( status != GM_SUCCESS ) {
		ompi_output( 0, "Cannot register receiver memory (%p, %ld) bytes offset %ld\n",
			     pointer, length, hdr->hdr_frag.hdr_frag_offset );
		return NULL;
	    }
	    recv_frag->frag_bytes_processed += length;
	}
    }

    return NULL;
}

static mca_ptl_gm_recv_frag_t*
mca_ptl_gm_recv_frag_fin( struct mca_ptl_gm_module_t* ptl,
			  mca_ptl_base_header_t* hdr )
{
    mca_ptl_gm_send_frag_t* frag;
    gm_status_t status;
    char* memory_to_deregister;
    uint64_t length;

    frag = (mca_ptl_gm_send_frag_t*)hdr->hdr_ack.hdr_src_ptr.pval;

    frag->send_frag.frag_base.frag_header.hdr_common.hdr_flags = hdr->hdr_common.hdr_flags;
    frag->send_frag.frag_base.frag_header.hdr_ack.hdr_dst_match = hdr->hdr_ack.hdr_dst_match;

    memory_to_deregister = (char*)frag->send_frag.frag_base.frag_addr
	+ frag->frag_offset + frag->frag_bytes_validated;
    length = frag->send_frag.frag_base.frag_size - frag->frag_bytes_validated;

    if( 0 == frag->frag_bytes_processed ) {
	/* I just receive the ack for the first fragment => setup the pipeline */
	mca_ptl_gm_send_next_long_segment( frag, GM_PTL_SEND_MESSAGE | GM_PTL_REGISTER_MEMORY );
    }
    /* continue the pipeline ... send the next segment */
    if( frag->frag_bytes_processed != frag->send_frag.frag_base.frag_size ) {
	/* If there is still something pending ... */
	mca_ptl_gm_send_next_long_segment( frag, GM_PTL_SEND_MESSAGE | GM_PTL_REGISTER_MEMORY );
    }

    if( 0 != hdr->hdr_ack.hdr_dst_size ) {
	if( length > mca_ptl_gm_component.gm_rdma_frag_size )
	    length = mca_ptl_gm_component.gm_rdma_frag_size;
	frag->frag_bytes_validated += length;
	if( frag->send_frag.frag_base.frag_size == frag->frag_bytes_validated ) {
	    /* mark the request as done before deregistering the memory */
	    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
					  frag->send_frag.frag_request,
					  frag->frag_bytes_validated );
	    OMPI_FREE_LIST_RETURN( &(ptl->gm_send_frags), (ompi_list_item_t*)frag );
	}
	status = gm_deregister_memory( ptl->gm_port, memory_to_deregister, length );
	if( GM_SUCCESS != status ) {
	    ompi_output( 0, "Unable to deregister sender GM memory at %p length %d bytes\n",
			 memory_to_deregister, length );
	}
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

typedef mca_ptl_gm_recv_frag_t* (frag_management_fct_t)( struct mca_ptl_gm_module_t *ptl,
							 mca_ptl_base_header_t *hdr );
frag_management_fct_t* frag_management_fct[MCA_PTL_HDR_TYPE_MAX] = {
    NULL,
    mca_ptl_gm_recv_frag_match,
    mca_ptl_gm_recv_frag_match,
    (frag_management_fct_t*)mca_ptl_gm_recv_frag_frag,  /* force the conversion to remove a warning */
    mca_ptl_gm_ctrl_frag,
    mca_ptl_gm_ctrl_frag,
    NULL,
    mca_ptl_gm_recv_frag_fin,
    NULL };

int mca_ptl_gm_analyze_recv_event( struct mca_ptl_gm_module_t* ptl, gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t * frag;
    mca_ptl_base_header_t *header = NULL, *release_buf;
    frag_management_fct_t* function;

    release_buf = (mca_ptl_base_header_t*)gm_ntohp(event->recv.buffer);

    switch (gm_ntohc(event->recv.type)) {
    case GM_FAST_RECV_EVENT:
    case GM_FAST_PEER_RECV_EVENT:
    case GM_FAST_HIGH_RECV_EVENT:
    case GM_FAST_HIGH_PEER_RECV_EVENT:
	header = (mca_ptl_base_header_t *)gm_ntohp(event->recv.message);
	goto have_event;
    case GM_RECV_EVENT:
    case GM_PEER_RECV_EVENT:
    case GM_HIGH_RECV_EVENT:
    case GM_HIGH_PEER_RECV_EVENT:
	header = release_buf;
	goto have_event;
    case GM_NO_RECV_EVENT:
	break;
	
    default:
	gm_unknown(ptl->gm_port, event);
    }
    return 0;

 have_event:
    if( header->hdr_common.hdr_type >= MCA_PTL_HDR_TYPE_MAX ) {
	ompi_output( 0, "[%s:%d] unexpected frag type %d\n",
		     __FILE__, __LINE__, header->hdr_common.hdr_type );
    } else {
	function = frag_management_fct[header->hdr_common.hdr_type];
	if( NULL == function ) {
	    ompi_output( 0, "[%s:%d] NOT yet implemented function for the header type %d\n",
			 __FILE__, __LINE__, header->hdr_common.hdr_type );
	} else {
	    frag = function( ptl, header );
	}
    }
    gm_provide_receive_buffer( ptl->gm_port, release_buf, GM_SIZE, GM_LOW_PRIORITY );

    return 0;
}

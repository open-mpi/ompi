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
#include <string.h>
#include "class/ompi_bitmap.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "mca/ns/ns.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "ptl_gm.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_priv.h"
#include "ptl_gm_sendfrag.h"

mca_ptl_gm_module_t mca_ptl_gm_module = {
    {
	&mca_ptl_gm_component.super,
	1,  /* max size of request cache */
	sizeof(mca_ptl_gm_send_frag_t), /* bytes required by ptl for a request */
	0,  /* max size of first fragment */
	0,  /* min fragment size */
	0,  /* max fragment size */
	0,  /* exclusivity */
	50, /* latency */
	0,  /* bandwidth */
	MCA_PTL_PUT,  /* ptl flags */
	/* collection of interfaces */
	mca_ptl_gm_add_procs,
	mca_ptl_gm_del_procs,
	mca_ptl_gm_finalize,
	mca_ptl_gm_peer_send,
	mca_ptl_gm_put,
	mca_ptl_gm_get,
	mca_ptl_gm_matched,
	mca_ptl_gm_request_init, 
	mca_ptl_gm_request_fini,
	NULL,
	NULL,
	NULL
    }
};

OBJ_CLASS_INSTANCE (mca_ptl_gm_send_request_t,
                    mca_pml_base_send_request_t, NULL, NULL);
OBJ_CLASS_INSTANCE (mca_ptl_gm_peer_t, ompi_list_item_t, NULL, NULL);

int
mca_ptl_gm_add_procs (struct mca_ptl_base_module_t *ptl,
                      size_t nprocs,
                      struct ompi_proc_t **orte_procs,
                      struct mca_ptl_base_peer_t **peers,
                      ompi_bitmap_t * reachable)
{
    uint32_t i, j, num_peer_ptls = 1;
    struct ompi_proc_t *orte_proc;
    mca_ptl_gm_proc_t *ptl_proc;
    mca_ptl_gm_peer_t *ptl_peer;
    unsigned int lid;
    ompi_proc_t* local_proc = ompi_proc_local();

    for (i = 0; i < nprocs; i++) {
        orte_proc = orte_procs[i];
        if( orte_proc == local_proc ) continue;
        ptl_proc = mca_ptl_gm_proc_create ((mca_ptl_gm_module_t *) ptl, orte_proc);
        if (NULL == ptl_proc) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        OMPI_THREAD_LOCK (&ptl_proc->proc_lock);
        if (ptl_proc->proc_addr_count == ptl_proc->proc_peer_count) {
            OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
            return OMPI_ERR_UNREACH;
        }

        /* TODO: make this extensible to multiple nics */
        for( j = 0; j < num_peer_ptls; j++ ) {
            ptl_peer = OBJ_NEW (mca_ptl_gm_peer_t);
            if (NULL == ptl_peer) {
                OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            ptl_peer->peer_ptl = (mca_ptl_gm_module_t *) ptl;
            ptl_peer->peer_proc = ptl_proc;
            ptl_peer->global_id = ptl_proc->proc_addrs->global_id;
            ptl_peer->port_number = ptl_proc->proc_addrs->port_id;
            if (GM_SUCCESS !=
                gm_global_id_to_node_id (((mca_ptl_gm_module_t *) ptl)->gm_port,
                                         ptl_proc->proc_addrs[j].global_id,
                                         &lid)) {
                ompi_output( 0, "[%s:%d] error in converting global to local id \n", 
			     __FILE__, __LINE__ );
                return OMPI_ERR_BAD_PARAM;
            }
            ptl_peer->local_id = lid;
            ptl_proc->peer_arr[ptl_proc->proc_peer_count] = ptl_peer;
            ptl_proc->proc_peer_count++;
            ptl_peer->peer_addr = ptl_proc->proc_addrs + i;
        }
        ompi_bitmap_set_bit (reachable, i);
        OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
        peers[i] = (struct mca_ptl_base_peer_t*)ptl_peer;
    }

    return OMPI_SUCCESS;
}

/*
 *
 */
int
mca_ptl_gm_del_procs (struct mca_ptl_base_module_t *ptl,
                      size_t nprocs,
                      struct ompi_proc_t **procs,
                      struct mca_ptl_base_peer_t **peers)
{
    size_t      i;
    for (i = 0; i < nprocs; i++) {
        OBJ_RELEASE (peers[i]);
    }
    return OMPI_SUCCESS;
}

/*
 *
 */
int
mca_ptl_gm_finalize (struct mca_ptl_base_module_t *base_ptl)
{
    uint32_t index;
    mca_ptl_gm_module_t* ptl = (mca_ptl_gm_module_t*)base_ptl;

    for( index = 0; index < mca_ptl_gm_component.gm_num_ptl_modules; index++ ) {
	if( mca_ptl_gm_component.gm_ptl_modules[index] == ptl ) {
	    mca_ptl_gm_component.gm_ptl_modules[index] = NULL;
            break;
	}
    }

    if( index == mca_ptl_gm_component.gm_num_ptl_modules ) {
        ompi_output( 0, "%p is not a GM PTL !!!\n", (void*)base_ptl );
        return OMPI_ERROR;
    }

    /* we should do the same things as in the init step in reverse order.
     * First we shutdown all threads if there are any.
     */
#if OMPI_HAVE_POSIX_THREADS
    if( 0 != ptl->thread.t_handle ) {
        void* thread_return;

	pthread_cancel( ptl->thread.t_handle );
	ompi_thread_join( &(ptl->thread), &thread_return );
    }
#endif  /* OMPI_HAVE_POSIX_THREADS */

    /* Closing each port require several steps. As there is no way to cancel all 
     * already posted messages we start by unregistering all memory and then close
     * the port. After we can release all internal data.
     */
    if( ptl->gm_send_dma_memory != NULL ) {
        gm_dma_free( ptl->gm_port, ptl->gm_send_dma_memory );
        ptl->gm_send_dma_memory = NULL;
    }

    if( ptl->gm_recv_dma_memory != NULL ) {
        gm_dma_free( ptl->gm_port, ptl->gm_recv_dma_memory );
        ptl->gm_recv_dma_memory = NULL;
    }

    /* Now close the port if one is open */
    if( ptl->gm_port != NULL ) {
        gm_close( ptl->gm_port );
        ptl->gm_port = NULL;
    }

    /* And now release all internal ressources. */
    OBJ_DESTRUCT( &(ptl->gm_send_frags) );
    if( ptl->gm_send_fragments != NULL ) {
        free( ptl->gm_send_fragments );
        ptl->gm_send_fragments = NULL;
    }

    OBJ_DESTRUCT( &(ptl->gm_recv_frags_free) );
    if( ptl->gm_recv_fragments != NULL ) {
        free( ptl->gm_recv_fragments );
        ptl->gm_recv_fragments = NULL;
    }

    /* These are supposed to be empty by now */
    OBJ_DESTRUCT( &(ptl->gm_send_frags_queue) );
    OBJ_DESTRUCT( &(ptl->gm_pending_acks) );
    OBJ_DESTRUCT( &(ptl->gm_recv_outstanding_queue) );

    /* And finally release the PTL itself */
    free( ptl );

    return OMPI_SUCCESS;
}

int
mca_ptl_gm_request_init( struct mca_ptl_base_module_t *ptl,
			 struct mca_pml_base_send_request_t *request )
{

#if 0
    mca_ptl_gm_send_frag_t *frag;
    struct mca_ptl_gm_send_request_t *req;
    frag = mca_ptl_gm_alloc_send_frag(ptl, request);
   
    if (NULL == frag) {
        ompi_output(0,"[%s:%d] Unable to allocate a gm send fragment\n");
        return OMPI_ERR_OUT_OF_RESOURCE;   
    } else  {
	req = (mca_ptl_gm_send_request_t *)request;
        req->req_frag = frag;
        frag->status = 0; /*MCA_PTL_GM_FRAG_CACHED;*/
    }
    return OMPI_SUCCESS;
#endif

    return OMPI_ERROR;
}

/*
 *
 */
void
mca_ptl_gm_request_fini (struct mca_ptl_base_module_t *ptl,
                         struct mca_pml_base_send_request_t *request)
{

#if 0
    mca_ptl_gm_send_frag_t *frag;
    frag = ((mca_ptl_gm_send_request_t *)request)->req_frag;
    OMPI_FREE_LIST_RETURN(&(((mca_ptl_gm_module_t *)ptl)->gm_send_frags),
                                            (ompi_list_item_t *)frag);
    frag->status = 0;
#endif

    OBJ_DESTRUCT(request+1);
}

/*
 *  Initiate a put
 */

int
mca_ptl_gm_put (struct mca_ptl_base_module_t *ptl,
                struct mca_ptl_base_peer_t *ptl_peer,
                struct mca_pml_base_send_request_t *sendreq,
                size_t offset, size_t size, int flags)
{
   int rc;
   mca_ptl_gm_send_frag_t *putfrag;

   rc = mca_ptl_gm_put_frag_init( &putfrag,
				  (mca_ptl_gm_peer_t*)ptl_peer, (mca_ptl_gm_module_t*)ptl,
				  sendreq, offset, &size, flags );

   rc = mca_ptl_gm_peer_send_continue( (mca_ptl_gm_peer_t *)ptl_peer, putfrag,
                                       sendreq, offset, &size, flags );
   return OMPI_SUCCESS;
}

/*
 *   initiate a get.
 */

int
mca_ptl_gm_get (struct mca_ptl_base_module_t *ptl,
                struct mca_ptl_base_peer_t *ptl_base_peer,
                struct mca_pml_base_recv_request_t *request,
                size_t offset, size_t size, int flags)
{
    return OMPI_SUCCESS;
}

static void mca_ptl_gm_basic_ack_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_ptl_gm_module_t* gm_ptl;
    mca_ptl_base_frag_t* frag_base;
    mca_ptl_base_header_t* header;

    header = (mca_ptl_base_header_t*)context;

    frag_base = (mca_ptl_base_frag_t*)header->hdr_ack.hdr_dst_addr.pval;
    gm_ptl = (mca_ptl_gm_module_t *)frag_base->frag_owner;

    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_send_dma_frags), ((ompi_list_item_t*)header) );
    /* release the send token */
    ompi_atomic_add( &(gm_ptl->num_send_tokens), 1 );
}

/*  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */
void
mca_ptl_gm_matched( mca_ptl_base_module_t * ptl,
                    mca_ptl_base_recv_frag_t * frag )
{
    mca_pml_base_recv_request_t *request;
    mca_ptl_base_header_t *hdr;
    int32_t rc;
    mca_ptl_gm_module_t *gm_ptl;
    mca_ptl_gm_recv_frag_t *recv_frag;
    mca_ptl_gm_peer_t* peer;
    struct iovec iov = { NULL, 0};
	
    gm_ptl = (mca_ptl_gm_module_t *)ptl;
    request = frag->frag_request;
    recv_frag = (mca_ptl_gm_recv_frag_t *)frag;
    peer = (mca_ptl_gm_peer_t*)recv_frag->frag_recv.frag_base.frag_peer;

    if( frag->frag_base.frag_header.hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK ) {  /* need to send an ack back */
	ompi_list_item_t *item;

	OMPI_FREE_LIST_TRY_GET( &(gm_ptl->gm_send_dma_frags), item );
        
        if( NULL == item ) {
            ompi_output(0,"[%s:%d] unable to alloc a gm fragment\n", __FILE__,__LINE__);
            OMPI_THREAD_LOCK (&mca_ptl_gm_component.gm_lock);
            ompi_list_append (&mca_ptl_gm_module.gm_pending_acks, (ompi_list_item_t *)frag);
            OMPI_THREAD_UNLOCK (&mca_ptl_gm_component.gm_lock);
        } else {
	    ompi_atomic_sub( &(gm_ptl->num_send_tokens), 1 );
	    assert( gm_ptl->num_send_tokens >= 0 );
	    hdr = (mca_ptl_base_header_t*)item;

	    hdr->hdr_ack.hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
	    hdr->hdr_ack.hdr_common.hdr_flags = 0;
	    hdr->hdr_ack.hdr_src_ptr = frag->frag_base.frag_header.hdr_rndv.hdr_src_ptr;
	    hdr->hdr_ack.hdr_dst_match.lval = 0L;
	    hdr->hdr_ack.hdr_dst_match.pval = request;
	    hdr->hdr_ack.hdr_dst_addr.lval = 0L;
	    hdr->hdr_ack.hdr_dst_addr.pval = frag;
	    hdr->hdr_ack.hdr_dst_size = request->req_bytes_packed;

	    gm_send_with_callback( ((mca_ptl_gm_module_t*)ptl)->gm_port, hdr,
                                   GM_SIZE, sizeof(mca_ptl_base_ack_header_t),
                                   GM_LOW_PRIORITY,
                                   peer->local_id,
                                   peer->port_number,
                                   mca_ptl_gm_basic_ack_callback,
                                   (void *)hdr );
        }
    }
    
    if( frag->frag_base.frag_size > 0 ) {
        unsigned int max_data, out_size;
        int freeAfter;

	iov.iov_len = mca_ptl_gm_component.gm_segment_size - sizeof(mca_ptl_base_rendezvous_header_t);
	if( frag->frag_base.frag_size < iov.iov_len ) {
	    iov.iov_len = frag->frag_base.frag_size;
	}
	/* Here we expect that frag_addr is the begin of the buffer header included */
	iov.iov_base = frag->frag_base.frag_addr;
    
        ompi_convertor_copy( peer->peer_proc->proc_ompi->proc_convertor,
                             &frag->frag_base.frag_convertor );
        ompi_convertor_init_for_recv( &frag->frag_base.frag_convertor, 0,
                                      request->req_base.req_datatype,
                                      request->req_base.req_count,
                                      request->req_base.req_addr,
                                      0, NULL );
        out_size = 1;
        max_data = iov.iov_len;
        rc = ompi_convertor_unpack( &frag->frag_base.frag_convertor, &(iov),
                                    &out_size, &max_data, &freeAfter );
        assert( rc >= 0 );
        recv_frag->frag_bytes_processed += max_data;
    }
    
    /* update progress*/
    ptl->ptl_recv_progress( ptl, request, iov.iov_len, iov.iov_len );
    
    /* Now update the status of the fragment */
    if( ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer == true ) {
        gm_release_local_buffer( ((mca_ptl_gm_recv_frag_t*)frag)->frag_recv.frag_base.frag_addr );
        ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer = false;
    }
    
    /* I'm done with this fragment. Return it to the free list */
    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_recv_frags_free), (ompi_list_item_t*)frag );
}


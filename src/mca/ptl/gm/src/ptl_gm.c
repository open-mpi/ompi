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
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_gm.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_req.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_priv.h"

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
	mca_ptl_gm_send, 
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
                      struct ompi_proc_t **ompi_procs,
                      struct mca_ptl_base_peer_t **peers,
                      ompi_bitmap_t * reachable)
{
    uint32_t i, j, num_peer_ptls = 1;
    struct ompi_proc_t *ompi_proc;
    mca_ptl_gm_proc_t *ptl_proc;
    mca_ptl_gm_peer_t *ptl_peer;
    unsigned int lid;
    ompi_proc_t* local_proc = ompi_proc_local();

    for (i = 0; i < nprocs; i++) {
        ompi_proc = ompi_procs[i];
        if( ompi_proc == local_proc ) continue;
        ptl_proc = mca_ptl_gm_proc_create ((mca_ptl_gm_module_t *) ptl, ompi_proc);
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
        frag->ptl = (mca_ptl_gm_module_t*)ptl;
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

int
mca_ptl_gm_send (struct mca_ptl_base_module_t *ptl,
                 struct mca_ptl_base_peer_t *ptl_peer,
                 struct mca_pml_base_send_request_t *sendreq,
                 size_t offset, size_t size, int flags)
{
    mca_ptl_gm_send_frag_t *sendfrag;
    mca_ptl_gm_peer_t *gm_ptl_peer;
    mca_ptl_gm_module_t * gm_ptl;
    int rc;

    gm_ptl = (mca_ptl_gm_module_t *)ptl;
    sendfrag = mca_ptl_gm_alloc_send_frag( gm_ptl, sendreq );
    if (NULL == sendfrag) {
	ompi_output( 0,"[%s:%d] Unable to allocate a gm send frag\n",
                     __FILE__, __LINE__ );
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ((struct mca_ptl_gm_send_request_t *)sendreq)->req_frag = sendfrag;
    ((struct mca_ptl_gm_send_request_t *)sendreq)->need_ack = flags;
    
    /* initiate the send */
    gm_ptl_peer = (mca_ptl_gm_peer_t *)ptl_peer;
    rc = mca_ptl_gm_init_header_match( sendfrag, sendreq, flags );
    rc = mca_ptl_gm_peer_send( gm_ptl_peer, sendfrag, sendreq, offset, &size, flags );

    sendreq->req_offset += size;
    return OMPI_SUCCESS;
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
   mca_ptl_gm_module_t * gm_ptl;
   void* destination_buffer;
   char * buffer_ptr;
   int status, bytes_reg;

   gm_ptl= (mca_ptl_gm_module_t *)ptl;
   buffer_ptr = ((char *) (sendreq->req_base.req_addr)) + offset ;
   bytes_reg = size;
   destination_buffer =(void *)( (sendreq->req_peer_addr).pval);

   /* register the user buffer */
   if (offset > 0) {
       status = gm_register_memory(gm_ptl->gm_port, buffer_ptr, bytes_reg);
       if(GM_SUCCESS != status) {
          ompi_output(0,"[%s:%d] Unable to register memory\n",__FILE__,__LINE__);
       } 
   }

   putfrag = mca_ptl_gm_alloc_send_frag( gm_ptl, sendreq ); /*alloc_put_frag */
    
   putfrag->registered_buf = (void *)buffer_ptr;
   putfrag->peer = (mca_ptl_gm_peer_t *)ptl_peer;

   ((struct mca_ptl_gm_send_request_t *)sendreq)->req_frag =putfrag;
   ((struct mca_ptl_gm_send_request_t *)sendreq)->need_ack = flags;
   ((struct mca_ptl_gm_send_request_t *)sendreq)->need_ack = 0;
   
   rc = mca_ptl_gm_put_frag_init( putfrag ,
				  (mca_ptl_gm_peer_t*)ptl_peer,gm_ptl, 
				  sendreq, offset, &size, flags );
   
   rc = mca_ptl_gm_peer_put( (mca_ptl_gm_peer_t *)ptl_peer, putfrag,
			     sendreq, offset, &size, flags,
			     destination_buffer, bytes_reg );
   gm_ptl->num_send_tokens--;
   sendreq->req_offset += size; 
   
   rc = mca_ptl_gm_peer_send (putfrag->peer,putfrag,sendreq,
			      offset,&size,flags);
   assert(rc == 0);
   
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

/* This function get called when the gm_get is finish (i.e. when the read from remote memory
 * is completed. We have to send back the ack.
 */
static void mca_ptl_gm_get_callback( struct gm_port *port, void * context, gm_status_t status )
{
    int rc;
    mca_ptl_gm_recv_frag_t* recv_frag = (mca_ptl_gm_recv_frag_t*)context;
    mca_ptl_gm_send_frag_t *ack;
    mca_ptl_gm_module_t *gm_ptl;
    mca_pml_base_recv_request_t *request;
    size_t size = 0;

    gm_ptl = (mca_ptl_gm_module_t*)recv_frag->frag_recv.frag_base.frag_owner;
    /* send the registered memory information, send recv request * ptr */
    request = recv_frag->frag_recv.frag_request;
    rc = mca_ptl_gm_send_ack_init( ack, gm_ptl, 
                                   (mca_ptl_gm_peer_t *)(recv_frag->frag_recv.frag_base.frag_peer),
                                   recv_frag, request->req_base.req_addr, recv_frag->frag_recv.frag_base.frag_size );
	    
    mca_ptl_gm_peer_send( (mca_ptl_gm_peer_t *) (ack->send_frag.frag_base.frag_peer),
                          ack, NULL, 0, &size, 0 );
    gm_ptl->num_send_tokens--;
}

/*  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */
void
mca_ptl_gm_matched( mca_ptl_base_module_t * ptl,
                    mca_ptl_base_recv_frag_t * frag )
{
    mca_pml_base_recv_request_t *request;
    mca_ptl_base_header_t *header;
    int bytes_recv, rc, total_bytes, bytes_reg;
    mca_ptl_gm_module_t *gm_ptl;
    struct iovec iov;
    mca_ptl_gm_send_frag_t *ack;
    mca_ptl_gm_recv_frag_t *recv_frag;
    char *buffer_ptr;
    gm_status_t status;
    mca_ptl_gm_peer_t* peer;

    header = &frag->frag_base.frag_header;
    request = frag->frag_request;
    gm_ptl = (mca_ptl_gm_module_t *)ptl;
    recv_frag = (mca_ptl_gm_recv_frag_t *)frag;
    peer = (mca_ptl_gm_peer_t*)recv_frag->frag_recv.frag_base.frag_peer;

    if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK) {
        /* need to send an ack back */

        ack = mca_ptl_gm_alloc_send_frag( gm_ptl, NULL );
        if( NULL == ack ) {
            ompi_output(0,"[%s:%d] unable to alloc a gm fragment\n", __FILE__,__LINE__);
            OMPI_THREAD_LOCK (&mca_ptl_gm_component.gm_lock);
            recv_frag->frag_ack_pending = true;
            ompi_list_append (&mca_ptl_gm_module.gm_pending_acks, (ompi_list_item_t *) frag);
            OMPI_THREAD_UNLOCK (&mca_ptl_gm_component.gm_lock);
        } else {
            buffer_ptr = (char*)(request->req_base.req_addr);
            total_bytes = request->req_bytes_packed;
            bytes_recv = frag->frag_base.frag_size; 
            bytes_reg = total_bytes - bytes_recv;
            buffer_ptr += bytes_recv;
            /* Register the local operation before */
            status = gm_register_memory(gm_ptl->gm_port, buffer_ptr, bytes_reg);
            recv_frag->registered_buf = buffer_ptr;
            if(GM_SUCCESS != status) {
                ompi_output(0,"[%s:%d] Unable to register memory\n",__FILE__,__LINE__);
            }
            
            /* And now read from the remote memory in the local memory. The amount used for this
             * operation should be the minimum one between the server and the client.
             */
            gm_get( gm_ptl->gm_port, recv_frag->remote_registered_memory.lval,
                    buffer_ptr, bytes_reg, GM_HIGH_PRIORITY, peer->peer_addr->global_id,
                    peer->peer_addr->port_id, mca_ptl_gm_get_callback, frag );
        }
    }
    
    /* Here we expect that frag_addr is the begin of the buffer header  included */
    iov.iov_base = ((char*)frag->frag_base.frag_addr); 
    bytes_recv = frag->frag_base.frag_size; 
    iov.iov_len = bytes_recv;
    
    if( header->hdr_match.hdr_msg_length > 0 ) {
        unsigned int max_data, out_size;
        int freeAfter;
	
        /*peer = ompi_comm_peer_lookup( request->req_base.req_comm,
          header->hdr_match.hdr_src );*/
        ompi_convertor_copy( peer->peer_proc->proc_ompi->proc_convertor,
                             &frag->frag_base.frag_convertor);
        ompi_convertor_init_for_recv( &frag->frag_base.frag_convertor,
                                      0,
                                      request->req_base.req_datatype,
                                      request->req_base.req_count,
                                      request->req_base.req_addr,
                                      0  /* TO DO which offset ? */, NULL );
        out_size = 1;
        max_data = iov.iov_len;
        rc = ompi_convertor_unpack( &frag->frag_base.frag_convertor, &(iov),
                                    &out_size, &max_data, &freeAfter );
        assert( rc >= 0 );
    }
    
    /* update progress*/   
    ptl->ptl_recv_progress( ptl, request, bytes_recv, bytes_recv ); 
    
    /* Now update the status of the fragment */
    if( ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer == true ) {
        free( ((mca_ptl_gm_recv_frag_t*)frag)->frag_recv.frag_base.frag_addr);
        ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer = false;
    }
    
    /* I'm done with this fragment. Return it to the free list */
    OMPI_FREE_LIST_RETURN( &(gm_ptl->gm_recv_frags_free), (ompi_list_item_t*)frag );
}


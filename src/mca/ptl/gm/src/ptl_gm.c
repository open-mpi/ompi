/*
 * $HEADER$
 */

#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_gm.h"
#include "ptl_gm_addr.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_req.h"
#include "ptl_gm_req.c"
#include "ptl_gm_peer.h"
#include "ptl_gm_priv.h"

mca_ptl_gm_module_t mca_ptl_gm_module = {
    {
	&mca_ptl_gm_component.super,
	1, /* max size of request cache */
	sizeof(mca_ptl_gm_send_frag_t), /* bytes required by ptl for a request */
	0, /* max size of first fragment */
	0, /* min fragment size */
	0, /* max fragment size */
	0, /* exclusivity */
	0, /* latency */
	0, /* bandwidth */
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
    int         i,j;
    int num_peer_ptls = 1;
    struct ompi_proc_t *ompi_proc;
    mca_ptl_gm_proc_t *ptl_proc;
    mca_ptl_gm_peer_t *ptl_peer;
    unsigned int lid;
    ompi_proc_t* local_proc = ompi_proc_local();

    for (i = 0; i < nprocs; i++) {
        ompi_proc = ompi_procs[i];
        if( ompi_proc == local_proc ) continue;
        ptl_proc =
            mca_ptl_gm_proc_create ((mca_ptl_gm_module_t *) ptl,
                                    ompi_proc);

        if (NULL == ptl_proc) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        OMPI_THREAD_LOCK (&ptl_proc->proc_lock);
        if (ptl_proc->proc_addr_count == ptl_proc->proc_peer_count) {
            OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
            return OMPI_ERR_UNREACH;
        }

        /* TODO: make this extensible to multiple nics */
        /* XXX: */
        /* FIXME: */

        for (j=0; j < num_peer_ptls; j++) {
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
                gm_global_id_to_node_id (((mca_ptl_gm_module_t *) ptl)->my_port,
                                         ptl_proc->proc_addrs[j].global_id,
                                         &lid)) {
                ompi_output (0,
                             "[%s:%d] error in converting global to local id \n", 
			     __FILE__, __LINE__);

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
mca_ptl_gm_finalize (struct mca_ptl_base_module_t *ptl)
{
    free (ptl);
    return OMPI_SUCCESS;
}

int
mca_ptl_gm_request_init(struct mca_ptl_base_module_t *ptl,
                    struct mca_pml_base_send_request_t *request)
{

   mca_ptl_gm_send_frag_t *frag;
   struct mca_ptl_gm_send_request_t *req;
   GM_DBG(PTL_GM_DBG_COMM,
	   "INSIDE REQUEST INIT: the request is %p\n",request);

   frag = mca_ptl_gm_alloc_send_frag(ptl, request);
   
    if (NULL == frag)
    {
        ompi_output(0,"[%s:%d] Unable to allocate a gm send fragment\n");
        return OMPI_ERR_OUT_OF_RESOURCE;   
    }
    else 
    {
       req = (mca_ptl_gm_send_request_t *)request;
        req->req_frag = frag;
        frag->status = 0; /*MCA_PTL_GM_FRAG_CACHED;*/
        frag->ptl = (mca_ptl_gm_module_t*)ptl;
    }
 
    return OMPI_SUCCESS;
}

/*
 *
 */
void
mca_ptl_gm_request_fini (struct mca_ptl_base_module_t *ptl,
                         struct mca_pml_base_send_request_t *request)
{
    mca_ptl_gm_send_frag_t *frag;
     
    frag = ((mca_ptl_gm_send_request_t *)request)->req_frag;
    OMPI_FREE_LIST_RETURN(&(((mca_ptl_gm_module_t *)ptl)->gm_send_frags),
                                            (ompi_list_item_t *)frag);
    frag->status = 0;/*XXX: MCA_PTL_GM_FRAG_LOCAL; */
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

    GM_DBG(PTL_GM_DBG_COMM,"INSIDE PTL GM SEND\n");

    gm_ptl = (mca_ptl_gm_module_t *)ptl;
    if (offset == 0) {
        GM_DBG(PTL_GM_DBG_COMM,"OFFSET = 0\n");
        sendfrag = ((mca_ptl_gm_send_request_t *)sendreq)->req_frag;
        sendfrag->req = sendreq;
        assert(sendreq != NULL);
    } else {
        sendfrag = mca_ptl_gm_alloc_send_frag (ptl,sendreq);
        if (NULL == sendfrag) {
           ompi_output(0,"[%s:%d] Unable to allocate a gm send frag\n",
                        __FILE__, __LINE__);
        return 0; /*XXX: return error */
        }
    }
    
    ((struct mca_ptl_gm_send_request_t *)sendreq)->req_frag =sendfrag;
    ((struct mca_ptl_gm_send_request_t *)sendreq)->need_ack = flags;
    rc = mca_ptl_gm_send_frag_init (sendfrag, 
	    (mca_ptl_gm_peer_t*)ptl_peer, 
	    sendreq, offset, &size, flags);

    /*initiate the send */
    gm_ptl_peer = (mca_ptl_gm_peer_t *)ptl_peer;
    rc = mca_ptl_gm_peer_send (gm_ptl_peer,sendfrag,sendreq,
                                offset,&size,flags);

    gm_ptl->num_send_tokens--;
    /*Update offset */
    sendreq->req_offset += size; /* XXX: should be what convertor packs */
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
       status = gm_register_memory(gm_ptl->my_port, buffer_ptr, bytes_reg);
       if(GM_SUCCESS != status) {
          ompi_output(0,"[%s:%d] Unable to register memory\n",__FILE__,__LINE__);
       } 
   }

   putfrag = mca_ptl_gm_alloc_send_frag (ptl,sendreq); /*alloc_put_frag */
   putfrag->registered_buf = (void *)buffer_ptr;
   putfrag->peer = (mca_ptl_gm_peer_t *)ptl_peer;

    ((struct mca_ptl_gm_send_request_t *)sendreq)->req_frag =putfrag;
    ((struct mca_ptl_gm_send_request_t *)sendreq)->need_ack = flags;
   
   rc = mca_ptl_gm_put_frag_init(putfrag ,
                            (mca_ptl_gm_peer_t*)ptl_peer,gm_ptl, 
                                    sendreq, offset, &size, flags);

   /* check that we have a send token available */
   rc = mca_ptl_gm_peer_put((mca_ptl_gm_peer_t *)ptl_peer, putfrag,
                                sendreq, offset, &size, flags,
                                destination_buffer, bytes_reg);
   gm_ptl->num_send_tokens--;
   sendreq->req_offset += size; /* should be what is returned by put */

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

/*  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */
void
mca_ptl_gm_matched( mca_ptl_base_module_t * ptl,
                    mca_ptl_base_recv_frag_t * frag )
{
    mca_pml_base_recv_request_t *request;
    mca_pml_base_send_request_t *srequest;
    mca_ptl_base_header_t *header;
    int bytes_recv, rc,rc1, total_bytes, bytes_reg;
    mca_ptl_gm_module_t *gm_ptl;
    struct iovec iov;
    mca_ptl_gm_send_frag_t *ack;
    mca_ptl_gm_recv_frag_t *recv_frag;
    char *buffer_ptr;
    gm_status_t status;
    size_t size = 0;

    header = &frag->frag_base.frag_header;
    request = frag->frag_request;
    gm_ptl = (mca_ptl_gm_module_t *)ptl;



    if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) {
        /* might need to send an ack back */
        recv_frag = (mca_ptl_gm_recv_frag_t *) frag;
        ack = mca_ptl_gm_alloc_send_frag(ptl,NULL);
        /*associate null request for ack*/

        if (NULL == ack) {
            ompi_output(0,"[%s:%d] unable to alloc a gm fragment\n",
                        __FILE__,__LINE__);
            OMPI_THREAD_LOCK (&mca_ptl_gm_component.gm_lock);
            recv_frag->frag_ack_pending = true;
            ompi_list_append (&mca_ptl_gm_module.gm_pending_acks,
                              (ompi_list_item_t *) frag);
            OMPI_THREAD_UNLOCK (&mca_ptl_gm_component.gm_lock);
        }
         else 
        {

             buffer_ptr = (char *)( request->req_base.req_addr );

             total_bytes = request->req_bytes_packed;
             /*total_bytes = (request->req_base.req_datatype->size) **/
                                        /*(request->req_base.req_count);*/
             bytes_recv = frag->frag_base.frag_size - header->hdr_common.hdr_size;
             bytes_reg = total_bytes - bytes_recv;
             buffer_ptr += bytes_recv;
             status = gm_register_memory(gm_ptl->my_port, buffer_ptr, bytes_reg);
             recv_frag->registered_buf = buffer_ptr;
             GM_DBG(PTL_GM_DBG_COMM,"Receiver: register addr: %p, bytes: %d\n",buffer_ptr,bytes_reg);

             if(GM_SUCCESS != status) {
                 ompi_output(0,"[%s:%d] Unable to register memory\n",__FILE__,__LINE__);
             }

             /* send the registered memory information, send recv request * ptr */
            rc1 = mca_ptl_gm_send_ack_init (ack, gm_ptl,
                  (mca_ptl_gm_peer_t *)(recv_frag->frag_recv.frag_base.frag_peer)
                    , recv_frag, buffer_ptr, bytes_reg);

            /*XXX : put the registered memory in pin-down cache */
            mca_ptl_gm_peer_send (
		    (mca_ptl_gm_peer_t *) (ack->send_frag.frag_base.frag_peer),
		    ack,srequest,0,&size,0 );
            GM_DBG(PTL_GM_DBG_COMM,"RECEIVER FINISHED SENDING ACK\n");   
            gm_ptl->num_send_tokens--;
        }
    }

    /* Here we expect that frag_addr is the begin of the buffer header  included */
    iov.iov_base = ((char*)frag->frag_base.frag_addr) + header->hdr_common.hdr_size;
    bytes_recv = frag->frag_base.frag_size - header->hdr_common.hdr_size;
    iov.iov_len = bytes_recv;


    if (header->hdr_frag.hdr_frag_length > 0) {
        ompi_proc_t *proc;

        proc = ompi_comm_peer_lookup(request->req_base.req_comm,
                                     request->req_base.req_peer);

        ompi_convertor_copy(proc->proc_convertor,
                            &frag->frag_base.frag_convertor);
        ompi_convertor_init_for_recv(
                                     &frag->frag_base.frag_convertor,
                                     0,
                                     request->req_base.req_datatype,
                                     request->req_base.req_count,
                                     request->req_base.req_addr,
                                     header->hdr_frag.hdr_frag_offset);
        rc = ompi_convertor_unpack(&frag->frag_base.frag_convertor, &(iov), 1);
        assert( rc >= 0 );
        GM_DBG(PTL_GM_DBG_COMM,"in matched: bytes received is %d\n", bytes_recv);
        /*memcpy(request->req_base.req_addr,iov.iov_base,bytes_recv);*/
    }

    /*update progress*/   
    ptl->ptl_recv_progress( ptl, request, bytes_recv, iov.iov_len );

    /* Now update the status of the fragment */
    ((mca_ptl_gm_recv_frag_t*)frag)->matched = true;
    /*if( ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer == true
 * ){*/
        /*free( frag->frag_base.frag_addr );*/
        ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer = false;
    /*}*/
    /*return to free list   */
    gm_ptl = (mca_ptl_gm_module_t *)ptl;
    OMPI_FREE_LIST_RETURN(&(gm_ptl->gm_recv_frags_free),
                    (ompi_list_item_t*)((mca_ptl_gm_recv_frag_t*)frag));
}




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
	mca_ptl_gm_send, /* JMS: Need send here */
	mca_ptl_gm_put,
	mca_ptl_gm_get,
	mca_ptl_gm_matched,
	mca_ptl_gm_request_init, /* JMS need request init here */
	mca_ptl_gm_request_fini, /* JMS need request fini here */
	NULL, /* JMS need match here */
	NULL, /* JMS need send_progress here */
	NULL /* JMS need recv_progress here */
    }
};



/*OBJ_CLASS_INSTANCE (mca_ptl_gm_recv_frag_t,*/
                    /*mca_ptl_base_recv_frag_t, NULL, NULL);*/

OBJ_CLASS_INSTANCE (mca_ptl_gm_send_request_t,
                    mca_pml_base_send_request_t, NULL, NULL);


OBJ_CLASS_INSTANCE (mca_ptl_gm_peer_t, ompi_list_item_t, NULL, NULL);




/*
 *
 */
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

    for (i = 0; i < nprocs; i++) {
        ompi_proc = ompi_procs[i];
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

       for (j=0; j < num_peer_ptls; j++)
       {
         /*XXX: check for self */

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
    "[%s:%d] error in converting global to local id \n", __FILE__, __LINE__);

          }
        ptl_peer->local_id = lid;

        ptl_proc->peer_arr[0] = ptl_peer;
        ptl_proc->proc_peer_count++;
        ptl_peer->peer_addr = ptl_proc->proc_addrs + i;
       }
        ompi_bitmap_set_bit (reachable, i);
        OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);

        peers[i] = (struct mca_ptl_base_peer_t*)ptl_peer;
      
        /*printf ("Global_id\t local_id\t port_number\t process name \n");*/
        /*fflush (stdout);*/
        /*printf ("%u %d %d %d\n", ptl_proc->peer_arr[0]->global_id,*/
                /*ptl_proc->peer_arr[0]->local_id,*/
                /*ptl_proc->peer_arr[0]->port_number,
 * ptl_proc->proc_guid);*/
        /*fflush (stdout);*/

    }

    printf ("returning with success from gm_add_procs\n");
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
   frag = mca_ptl_gm_alloc_send_frag(ptl, request);
   
    if (NULL == frag)
    {
        ompi_output(0,"[%s:%d] Unable to allocate a gm send fragment\n");
        return OMPI_ERR_OUT_OF_RESOURCE;   
    }
    else 
    {
       req = (mca_ptl_gm_send_request_t *)request;
       /*((mca_ptl_gm_send_request_t *)request)->req_frag = frag;*/
        req->req_frag = frag;
        frag->status = 0; /*MCA_PTL_GM_FRAG_CACHED;*/
        frag->ptl = (mca_ptl_gm_module_t*)ptl;
        /*frag->peer = request->req_peer;*/
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

    gm_ptl = (mca_ptl_gm_module_t *)ptl;
    if (offset == 0) {
        sendfrag = ((mca_ptl_gm_send_request_t *)sendreq)->req_frag;
    } else {
        sendfrag = mca_ptl_gm_alloc_send_frag (ptl,sendreq);
        if (NULL == sendfrag) {
           ompi_output(0,"[%s:%d] Unable to allocate a gm send frag\n",
                        __FILE__, __LINE__);
        return 0; /*XXX: return error */
        }
    }
    
    ((struct mca_ptl_gm_send_request_t *)sendreq)->req_frag =sendfrag;
    rc = mca_ptl_gm_send_frag_init (sendfrag, (mca_ptl_gm_peer_t*)ptl_peer, sendreq, offset,
                            &size, flags);

    /*initiate the send */
    gm_ptl_peer = (mca_ptl_gm_peer_t *)ptl_peer;
    rc = mca_ptl_gm_peer_send (gm_ptl_peer,sendfrag,sendreq,
                                offset,&size,flags);

    gm_ptl->num_send_tokens--;
    /*Update offset */
    sendreq->req_offset += size; /* XXX: should be what convertor packs */
    
    /*append to the send_fragments_queue. */
    ompi_list_append (&(gm_ptl->gm_send_frags_queue),
                      (ompi_list_item_t *) sendfrag);
    return rc;
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
    /*mca_ptl_base_recv_request_t *request;*/
    mca_ptl_base_header_t *header;
    int bytes_recv, rc;
    mca_ptl_gm_module_t *gm_ptl;   
    struct iovec iov[1];

    header = &frag->frag_base.frag_header;
    request = frag->frag_request;
    if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) {
#if 0
        /* might need to send an ack back */
        int         rc;
        mca_ptl_gm_send_frag_t *ack;
        recv_frag = (mca_ptl_gm_recv_frag_t *) frag;
        ack = mca_ptl_gm_alloc_send_frag(ptl,NULL);

        if (NULL == ack) {
            ompi_output(0,"[%s:%d] unable to alloc a gm fragment\n",
                        __FILE__,___LINE__);
            OMPI_THREAD_LOCK (&mca_ptl_gm_module.gm_lock);
            recv_frag->frag_ack_pending = true;
            ompi_list_append (&mca_ptl_gm_module.gm_pending_acks,
                              (ompi_list_item_t *) frag);
            OMPI_THREAD_UNLOCK (&mca_ptl_gm_module.gm_lock);
        } else {
            mca_ptl_gm_send_frag_init_ack (ack, ptl,
                                           recv_frag->super.super.
                                           frag_peer, recv_frag);
            /*XXX: check this*/
            mca_ptl_gm_peer_send (ack->super.super.frag_peer, ack,0,0,0 );
        }
#endif
    }

    /* Here we expect that frag_addr is the beging of the buffer header included */
    iov[0].iov_base = ((char*)frag->frag_base.frag_addr) + header->hdr_common.hdr_size;
    bytes_recv = frag->frag_base.frag_size - header->hdr_common.hdr_size;
    iov[0].iov_len = bytes_recv;

    /*process fragment if complete */
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
        rc = ompi_convertor_unpack(&frag->frag_base.frag_convertor, &(iov[0]), 1);
        assert( rc == 1 );
    } 

    /*update progress*/   /* XXX : check this */
    ptl->ptl_recv_progress( ptl, request, bytes_recv, iov[0].iov_len );

    /* Now update the status of the fragment */
    ((mca_ptl_gm_recv_frag_t*)frag)->matched = true;
    if( ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer == true ) {
        free( frag->frag_base.frag_addr );
        ((mca_ptl_gm_recv_frag_t*)frag)->have_allocated_buffer = false;
    }
    /*return to free list   */
    gm_ptl = (mca_ptl_gm_module_t *)ptl;
    OMPI_FREE_LIST_RETURN(&(gm_ptl->gm_recv_frags_free), (ompi_list_item_t*)frag); 
}

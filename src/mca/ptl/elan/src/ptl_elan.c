/*
 * $HEADER$
 */

#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"
#include "ptl_elan_priv.h"

/* XXX: There must be multiple PTL's. This could be the template */
mca_ptl_elan_module_t mca_ptl_elan_module = {
    {
        &mca_ptl_elan_component.super,
	4,
	sizeof(mca_ptl_elan_send_frag_t),
        0,                         /* ptl_exclusivity */
        0,                         /* ptl_latency */
        0,                         /* ptl_bandwidth */
        0,                         /* ptl_frag_first_size */
        0,                         /* ptl_frag_min_size */
        0,                         /* ptl_frag_max_size */
        MCA_PTL_PUT,               /* ptl flags */
        
        /* collection of interfaces */
        mca_ptl_elan_add_procs,
        mca_ptl_elan_del_procs,
        mca_ptl_elan_finalize,
        mca_ptl_elan_isend,
        mca_ptl_elan_put,
        mca_ptl_elan_get,
        mca_ptl_elan_matched,
        mca_ptl_elan_req_init,
        mca_ptl_elan_req_fini
    }
};

int
mca_ptl_elan_add_procs (struct mca_ptl_base_module_t *ptl,
                        size_t nprocs,
                        struct ompi_proc_t **procs,
                        struct mca_ptl_base_peer_t **peers,
                        ompi_bitmap_t * reachable)
{
    struct ompi_proc_t *ompi_proc;
    mca_ptl_elan_proc_t *ptl_proc;
    mca_ptl_elan_peer_t *ptl_peer;

    int         i;

    /* Here nprocs is the number of peer processes */
    for (i = 0; i < nprocs; i++) {

        ompi_proc = procs[i];
        ptl_proc = mca_ptl_elan_proc_create (ompi_proc);

        if (NULL == ptl_proc) {
            ompi_output (0,
                         "[%s:%d] could not create a ptl proc\n",
                         __FILE__, __LINE__);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* Check to make sure that the peer has at least as many 
         * interface addresses exported as we are trying to use. 
         * If not, then don't bind this PTL instance to the proc.
         */
        OMPI_THREAD_LOCK (&ptl_proc->proc_lock);

        if (ptl_proc->proc_addr_count == ptl_proc->proc_peer_count) {
            OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
            ompi_output (0, "all peers are taken already\n");
            return OMPI_ERR_UNREACH;
        }

        /* The ptl_proc datastructure is shared by all PTL 
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the ptl_proc.
         */
	ptl_peer = OBJ_NEW (mca_ptl_elan_peer_t);
	if (NULL == ptl_peer) {
	    OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
	    ompi_output (0, "[%s:%d] unabled to allocate ptl_peer \n",
		    __FILE__, __LINE__);
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}

	/* TODO: Make the add_procs function cleaner and simpler
	 * 1) Since elan_proc_t will only have one vp to address the
	 *    remote processes, there will be only one ptl_peer_t per proc.
	 *    The information to be stored there should be just 
	 *    the addressing information, which is the elan_vp.
	 *    If needed, the information can be expanded with memory-handle,
	 *    queue-handle, put/get-handle, memory-statics.
	 * 2) XXX: Consider matching elan_vp and ompi_proc_name_t.
	 */
        ptl_peer->peer_ptl  = (mca_ptl_elan_module_t *) ptl;
	ptl_peer->peer_proc = ptl_proc;

	/* There is only one peer per elan_peer_proc_t. */
	ptl_proc->proc_peers[0] = ptl_peer;
	if (ptl_proc == mca_ptl_elan_component.elan_local) {
	    ptl_peer->peer_vp = ((mca_ptl_elan_module_t *)ptl)->elan_vp;
	} else {
	    ptl_peer->peer_vp = ptl_proc->proc_addrs->elan_vp;
	    ptl_proc->proc_addrs->inuse = 1;
	}
	ptl_peer->peer_rails = ((mca_ptl_elan_module_t *)ptl)->ptl_ni_total;

	/* There is only one peer per elan_peer_proc_t */
	ptl_proc->proc_peer_count = 1;
        ompi_bitmap_set_bit (reachable, i);
        OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
        peers[i] = (struct mca_ptl_base_peer_t *) ptl_peer;
    }
    return OMPI_SUCCESS;
}

int
mca_ptl_elan_del_procs (struct mca_ptl_base_module_t *ptl,
                        size_t nprocs,
                        struct ompi_proc_t **procs,
                        struct mca_ptl_base_peer_t **peers)
{
    int         i;
    for (i = 0; i < nprocs; i++) {
        OBJ_RELEASE (peers[i]);
    }
    return OMPI_SUCCESS;
}

int
mca_ptl_elan_finalize (struct mca_ptl_base_module_t *ptl)
{
    int         rail_index;
    struct mca_ptl_elan_module_t *elan_ptl;

    elan_ptl = (struct mca_ptl_elan_module_t *) ptl;

    /* XXX: Free all the lists, etc, hanged over PTL 
     * before freeing the PTLs */
    rail_index = elan_ptl->ptl_ni_local;
    free (elan_ptl);

    /* Record the missing of this entry */
    mca_ptl_elan_component.elan_ptl_modules[rail_index] = NULL;
    mca_ptl_elan_component.elan_num_ptl_modules--;

    return OMPI_SUCCESS;
}

int
mca_ptl_elan_req_init (struct mca_ptl_base_module_t *ptl,
                       struct mca_pml_base_send_request_t *request)
{
    mca_ptl_elan_send_frag_t *desc;

    START_FUNC();

    desc = mca_ptl_elan_alloc_send_desc(ptl, request);
    if (NULL == desc) {
        ompi_output(0,
                "[%s:%d] Unable to allocate an elan send descriptors \n", 
                __FILE__, __LINE__);
	return OMPI_ERR_OUT_OF_RESOURCE; 
    } else {
	/* XXX: Hope PML never writes into the fragment */
	((mca_ptl_elan_send_request_t *)request)->req_frag = desc;
    }
    desc->desc->desc_status = MCA_PTL_ELAN_DESC_CACHED;

    END_FUNC();
    return OMPI_SUCCESS;
}

void
mca_ptl_elan_req_fini (struct mca_ptl_base_module_t *ptl,
                       struct mca_pml_base_send_request_t *request)
{
    /* XXX: Lock to be added */
    ompi_ptl_elan_queue_ctrl_t *queue;
    mca_ptl_elan_send_frag_t    *desc;

    queue = ((struct mca_ptl_elan_module_t * )ptl)->queue;

    /* return the fragment and update the status */
    desc = ((mca_ptl_elan_send_request_t *) request)->req_frag;
    OMPI_FREE_LIST_RETURN (&queue->tx_desc_free, (ompi_list_item_t *) desc);
    desc->desc->desc_status = MCA_PTL_ELAN_DESC_LOCAL;
    return;
}


void
mca_ptl_elan_recv_frag_return (struct mca_ptl_base_module_t *ptl,
                               struct mca_ptl_elan_recv_frag_t *frag)
{
    OMPI_FREE_LIST_RETURN(&mca_ptl_elan_component.elan_recv_frags_free, 
            (ompi_list_item_t*)frag);
    return;
}


void
mca_ptl_elan_send_frag_return (struct mca_ptl_base_module_t *ptl,
                               struct mca_ptl_elan_send_frag_t *frag)
{
    return;
}

/*
 *  Initiate an isend operation 
 */

int
mca_ptl_elan_isend (struct mca_ptl_base_module_t *ptl,
                    struct mca_ptl_base_peer_t *ptl_peer,
                    struct mca_pml_base_send_request_t *sendreq,
                    size_t offset,
                    size_t size,
                    int flags)
{
    int rc = OMPI_SUCCESS;
    mca_ptl_elan_send_frag_t *desc;

    /* XXX: 
     *   PML extract an request from PTL component and then use this
     *   a request to ask for a fragment
     *   Is it too deep across stacks to get a request and 
     *   correspondingly multiple LOCKS to go through*/

    START_FUNC();

    if (offset == 0) { /* The first fragment uses a cached desc */
        desc = ((mca_ptl_elan_send_request_t*)sendreq)->req_frag;
    } else {

	desc = mca_ptl_elan_alloc_send_desc(ptl, sendreq);
	if (NULL == desc) {
	    ompi_output(0,
		    "[%s:%d] Unable to allocate an elan send descriptors \n", 
		    __FILE__, __LINE__);
	}
    }

    ((struct mca_ptl_elan_send_request_t *)sendreq)->req_frag = desc;

    rc = mca_ptl_elan_start_desc(desc, 
	    (struct mca_ptl_elan_peer_t *)ptl_peer,
	    sendreq, offset, &size, flags);

    /* Update offset */
    sendreq->req_offset += size;

    END_FUNC();
    return rc;
}

/*
 *  Initiate a put operation. 
 */

int
mca_ptl_elan_put (struct mca_ptl_base_module_t *ptl,
                  struct mca_ptl_base_peer_t *ptl_peer,
                  struct mca_pml_base_send_request_t *sendreq,
                  size_t offset,
                  size_t size,
                  int flags)
{
    int rc = OMPI_SUCCESS;
    return rc;
}

/*
 *  Get routine. We need an interface that provides one more argument,
 *  describing the source of the data.
 */

int
mca_ptl_elan_get (struct mca_ptl_base_module_t *ptl,
                  struct mca_ptl_base_peer_t *ptl_base_peer,
                  struct mca_pml_base_recv_request_t *request,
                  size_t offset,
                  size_t size,
                  int flags)
{
    return OMPI_SUCCESS;
}

/*
 *  A posted receive has been matched 
 *  + Copy the data into user buffer
 *  + Return an ack if need to 
 */

void
mca_ptl_elan_matched (mca_ptl_base_module_t * ptl,
                      mca_ptl_base_recv_frag_t * frag)
{
    mca_pml_base_recv_request_t *request; 
    mca_ptl_base_header_t *header;
    int     set = 0;

    header  = &frag->frag_base.frag_header;
    request = frag->frag_request;

    if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) 
#if 1
    {
	mca_ptl_elan_send_frag_t *desc;

	/* Get a frag desc and allocate a send desc */
	desc = mca_ptl_elan_alloc_send_desc(ptl, NULL);

	if (NULL == desc) {
	    ompi_output(0,
		    "[%s:%d] Unable to allocate an elan send descriptors \n", 
		    __FILE__, __LINE__);
            OMPI_THREAD_LOCK(&mca_ptl_elan_component.elan_lock);
	    ((mca_ptl_elan_recv_frag_t *)frag)->frag_ack_pending = true;
            ompi_list_append(&mca_ptl_elan_component.elan_pending_acks, 
		    (ompi_list_item_t*)frag);
            OMPI_THREAD_UNLOCK(&mca_ptl_elan_component.elan_lock);
	} else {
#if 0
	    mca_ptl_elan_start_desc(desc, 
		    (struct mca_ptl_elan_peer_t *)ptl_peer,
		    NULL, offset, &size, flags);
#endif
        }
    }
#else
    {
	/* TODO: Optimized processing fragments 
	 * Pseudocode, for additional processing of fragments 
	 * a) (ACK:no, Get:No) 
	 *    Remove the frag. no need for further processing
	 * b) (ACK:yes, Get:No) 
	 *    Send an ACK only
	 * c) (ACK:yes, Get:yes) 
	 *     Get a message, update the fragment descriptor and 
	 *     then send an ACK, 
	 * d) Consider moving time-consuming tasks to some BH-like 
	 *    mechanisms.
	 */

    }
#endif

    /* Process the fragment */
    set = fetchNset (&((mca_ptl_elan_recv_frag_t *)frag)->frag_progressed, 1);

    if (!set) {

	/* IN TCP case, IO_VEC is first allocated.
	 * then recv the data, and copy if needed,
	 *
	 * But in ELAN cases, we save the data into an unex buffer
	 * if the recv descriptor is not posted (for too long) (TODO).
	 * We then need to copy from unex_buffer to application buffer */
	if(header->hdr_frag.hdr_frag_length > 0) {

	    struct iovec iov; 
	    ompi_proc_t *proc;

	    /* XXX: if (frag->frag_is_buffered) */
	    iov.iov_base = frag->frag_base.frag_addr;
	    iov.iov_len  = frag->frag_base.frag_size;

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
	    ompi_convertor_unpack(&frag->frag_base.frag_convertor, &iov, 1); 
	} 

#if 0
	 if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) {
	    /* FIXME: Pseudocode, for additional processing of fragments 
	     * a) (ACK:no, Get:No) 
	     *    Remove the frag. no need for further processing
	     * b) (ACK:yes, Get:No) 
	     *    Send an ACK only
	     * c) (ACK:yes, Get:yes) 
	     *     Get a message, update the fragment descriptor and 
	     *     then send an ACK, 
	     * d) Consider moving time-consuming tasks to some BH-like 
	     *    mechanisms.
	     */
	 }

	frag->frag_base.frag_owner->ptl_recv_progress (
                frag->frag_base.frag_owner, 
                request, 
                frag->frag_base.frag_size, 
                frag->frag_base.frag_size);

	/* FIXME: 
	 * To support the required ACK, do not return
	 * until the ack is out */
	if (((mca_ptl_elan_recv_frag_t *) frag)->frag_ack_pending == false)
	    mca_ptl_elan_recv_frag_return (frag->frag_base.frag_owner,
		    (mca_ptl_elan_recv_frag_t *) frag);
#else
	/* XXX: progress the request based on the status of this recv frag
	 * It is possible to employ a scheduling logic here.
	 * Then Done with this fragment, i.e., data */
	mca_ptl_elan_recv_frag_done (header, frag, request);
#endif
 
    }
}


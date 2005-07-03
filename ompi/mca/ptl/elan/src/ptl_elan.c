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
#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/pml/teg/src/pml_teg_proc.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/pml/base/pml_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"
#include "ptl_elan_priv.h"

/* XXX: There must be multiple PTL's. This could be the template */
mca_ptl_elan_module_t mca_ptl_elan_module = {
    {
        &mca_ptl_elan_component.super,
	1,
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
        mca_ptl_elan_req_fini,
        NULL,
        NULL,
        NULL
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

    /* TODO: move from mca_ptl_elan_component_close()
       all the ptl_elan_finalize related code here */
#if 0  
    elan_ptl = (struct mca_ptl_elan_module_t *) ptl;

    /* XXX: Free all the lists, etc, hanged over PTL 
     * before freeing the PTLs */
    rail_index = elan_ptl->ptl_ni_local;
    free (elan_ptl);

    /* Record the missing of this entry */
    mca_ptl_elan_component.modules[rail_index] = NULL;
    mca_ptl_elan_component.num_modules--;
#endif

    return OMPI_SUCCESS;
}

int
mca_ptl_elan_req_init (struct mca_ptl_base_module_t *ptl,
                       struct mca_pml_base_send_request_t *request)
{
    mca_ptl_elan_send_frag_t *desc;

    desc = mca_ptl_elan_alloc_desc(ptl, 
	    (struct mca_pml_base_request_t *) request, 
	    MCA_PTL_ELAN_DESC_QDMA);
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

    return OMPI_SUCCESS;
}

void
mca_ptl_elan_req_fini (struct mca_ptl_base_module_t *ptl,
                       struct mca_pml_base_send_request_t *request)
{
    /* XXX: Lock to be added */
    ompi_ptl_elan_queue_ctrl_t *queue;
    mca_ptl_elan_send_frag_t    *desc;

    /* return the fragment and update the status */
    queue = ((struct mca_ptl_elan_module_t * )ptl)->queue;
    desc = ((mca_ptl_elan_send_request_t *) request)->req_frag;
    OMPI_FREE_LIST_RETURN (&queue->tx_desc_free, (opal_list_item_t *) desc);
    desc->desc->desc_status = MCA_PTL_ELAN_DESC_LOCAL;
    return;
}


void
mca_ptl_elan_recv_frag_return (struct mca_ptl_base_module_t *ptl,
                               struct mca_ptl_elan_recv_frag_t *frag)
{
    OMPI_FREE_LIST_RETURN(&mca_ptl_elan_component.elan_recv_frags_free, 
            (opal_list_item_t*)frag);
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
     *   correspondingly multiple LOCKS to go through
     */

    if (offset == 0 && sendreq->req_cached) { 
	/* The first fragment uses a cached desc */
        desc = ((mca_ptl_elan_send_request_t*)sendreq)->req_frag;
    } else {
       	desc = mca_ptl_elan_alloc_desc(ptl, 
		(struct mca_pml_base_request_t *) sendreq, 
		MCA_PTL_ELAN_DESC_QDMA);
	if (NULL == desc) {
	    ompi_output(0,
		    "[%s:%d] Unable to allocate an elan send descriptors \n", 
		    __FILE__, __LINE__);
	}
    }

#if OMPI_PTL_ELAN_ZERO_FFRAG
    if (sendreq->req_bytes_packed > 
	    (OMPI_PTL_ELAN_MAX_QSIZE - sizeof(mca_ptl_base_header_t)))
	size = 0;
#endif

    ((struct mca_ptl_elan_send_request_t *)sendreq)->req_frag = desc;
    rc = mca_ptl_elan_start_desc(desc, 
	    (struct mca_ptl_elan_peer_t *)ptl_peer,
	    sendreq, offset, &size, flags);

    /* Update offset */
    sendreq->req_offset += size;
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
    mca_ptl_elan_send_frag_t *desc;

    /* PML still utilize this interface the same as a send option.
     * So we need to generate a QDMA to the remote side for completion
     * notification */

    /* XXX: 
     *    Since the address passed down from PML does not provide 
     *    elan information, so there needs to be a change 
     */

    desc = mca_ptl_elan_alloc_desc(ptl, 
	    (struct mca_pml_base_request_t *) sendreq, 
	    MCA_PTL_ELAN_DESC_PUT);
    if (NULL == desc) {
	ompi_output(0,
		"[%s:%d] Unable to allocate an elan send descriptors \n", 
		__FILE__, __LINE__);
    }

    rc = mca_ptl_elan_start_desc(desc, 
	    (struct mca_ptl_elan_peer_t *)ptl_peer,
	    sendreq, offset, &size, flags);

    /* XXX: It is very important to update offset,
     *      otherwise, you stuck */
    sendreq->req_offset += size;

    /* Update all the sends until the put is done */
    return rc;
}

/*
 *  Get routine. We need an interface that provides one more argument,
 *  describing the source of the data.
 */

int
mca_ptl_elan_get (struct mca_ptl_base_module_t *ptl,
                  struct mca_ptl_base_peer_t *ptl_peer,
                  struct mca_pml_base_recv_request_t *req,
                  size_t offset,
                  size_t size,
                  int flags)
{
    int rc = OMPI_SUCCESS;

#if OMPI_PTL_ELAN_ENABLE_GET && defined (HAVE_GET_INTERFACE)
    mca_ptl_elan_send_frag_t *desc;

    /* TODO: 
     *   There is no interface support in PML.
     *   So a walkround is to twist the way ack works.
     *   Step 1: Have elan_isend send E4_addr over
     *   Step 2: Have elan_matched trigger elan_get, 
     *           with an ack chained, which generate events to both sides.
     *   Step 3: Done at both the send side and the recv side.
     */

    /* XXX: 
     *    Since the address passed down from PML does not provide 
     *    elan information, so there needs to be a change 
     */

    desc = mca_ptl_elan_alloc_desc(ptl, 
	    (struct mca_pml_base_request_t *) req, 
	    MCA_PTL_ELAN_DESC_GET);
    if (NULL == desc) {
	ompi_output(0,
		"[%s:%d] Unable to allocate an elan send descriptors \n", 
		__FILE__, __LINE__);
    }

    rc = mca_ptl_elan_start_get(desc, (struct mca_ptl_elan_peer_t *)ptl_peer,
	    req, offset, &size, flags);

    /* XXX: It is very important to update offset,
     *      otherwise, you stuck */
    /*req->req_offset += size;*/

    /* Update all the sends until the put is done */
#endif
    return rc;
}

/* A posted receive has been matched 
 *  + Copy the data into user buffer
 *  + Return an ack if need to 
 */

void
mca_ptl_elan_matched (mca_ptl_base_module_t * ptl,
                      mca_ptl_base_recv_frag_t * frag)
{
    mca_pml_base_recv_request_t *request; 
    mca_ptl_base_header_t *header;
    mca_ptl_elan_recv_frag_t * recv_frag;

    int     set = 0;

    header  = &frag->frag_base.frag_header;
    request = frag->frag_request;
    recv_frag = (mca_ptl_elan_recv_frag_t * ) frag;

    if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK) {
	int  desc_type ;
	/* Basic ACK scheme following TCP cases */
	mca_ptl_elan_send_frag_t *desc;

#if OMPI_PTL_ELAN_ENABLE_GET
	desc_type = MCA_PTL_ELAN_DESC_GET;
#else
	desc_type = MCA_PTL_ELAN_DESC_QDMA;
#endif
	/* Get a frag desc and allocate a send desc */
	desc = mca_ptl_elan_alloc_desc(ptl, NULL, desc_type);
	if (NULL == desc) {
	    ompi_output(0,
		    "[%s:%d] Unable to allocate an elan send descriptors \n", 
		    __FILE__, __LINE__);
            OMPI_THREAD_LOCK(&mca_ptl_elan_component.elan_lock);
	    recv_frag->frag_ack_pending = true;
            opal_list_append(&((mca_ptl_elan_module_t * )ptl)->pending_acks, 
		    (opal_list_item_t*)frag);
            OMPI_THREAD_UNLOCK(&mca_ptl_elan_component.elan_lock);
	} else {
	    /* XXX: recv_frag is released a few lines below,
	     *      pay more attention to timing of the release */ 
#if OMPI_PTL_ELAN_ENABLE_GET
	    mca_ptl_elan_get_with_ack (ptl, desc, recv_frag);
	    LOG_PRINT(PTL_ELAN_DEBUG_GET, "Get desc %p type %d\n", 
		    desc, desc->desc->desc_type);
#else
	    mca_ptl_elan_start_ack (ptl, desc, recv_frag);
#endif
        }
    }

    set = ompi_atomic_fetch_and_set_int (
	    &((mca_ptl_elan_recv_frag_t *)frag)->frag_progressed, 1);
    if (!set) {
	/* IN TCP case, IO_VEC is first allocated.
	 * then recv the data, and copy if needed,
	 *
	 * But in ELAN cases, we save the data into an unex buffer
	 * if the recv descriptor is not posted (for too long) (TODO).
	 * We then need to copy from unex_buffer to application buffer */
	if (header->hdr_frag.hdr_frag_length > 0) {
#if !OMPI_PTL_ELAN_USE_DTP
	    memcpy(request->req_base.req_addr,
		    frag->frag_base.frag_addr, frag->frag_base.frag_size);
#else
	    int iov_count = 1, max_data, freeAfter;
	    struct iovec iov; 
	    ompi_proc_t *proc;

	    iov.iov_base = frag->frag_base.frag_addr;
	    iov.iov_len  = frag->frag_base.frag_size;
	    max_data = iov.iov_len;

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
		    header->hdr_frag.hdr_frag_offset,
		    NULL);
	    ompi_convertor_unpack(&frag->frag_base.frag_convertor, 
		    &iov, &iov_count, &max_data, &freeAfter); 
#endif
	}

	/* XXX: progress the request based on the status of this recv frag
	 * It is possible to employ a scheduling logic here.
	 * Then Done with this fragment, i.e., data */
	mca_ptl_elan_recv_frag_done (header, 
		(mca_ptl_elan_recv_frag_t *)frag, request);
    }
}

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
#include "ptl_ib.h"
#include "ptl_ib_sendfrag.h"

mca_ptl_ib_module_t mca_ptl_ib_module = {
    {
        &mca_ptl_ib_component.super,
        1, /* max size of request cache */
        sizeof(mca_ptl_ib_send_frag_t), /* bytes required by ptl for a request */
        0, /* max size of first fragment */
        0, /* min fragment size */
        0, /* max fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_PTL_PUT,  /* ptl flags */
        mca_ptl_ib_add_procs,
        mca_ptl_ib_del_procs,
        mca_ptl_ib_finalize,
        mca_ptl_ib_send,
        NULL,
#if 0
        mca_ptl_ib_matched,
        mca_ptl_ib_request_alloc,
        mca_ptl_ib_request_return
#else
        NULL, /* Sayantan: need to update matched */
        NULL, /* Sayantan: need request_init */
        NULL, /* Sayantan: need request_fini */
#endif
    }
};


int mca_ptl_ib_add_procs(struct mca_ptl_base_module_t* base_module, 
        size_t nprocs, struct ompi_proc_t **ompi_procs, 
        struct mca_ptl_base_peer_t** peers, ompi_bitmap_t* reachable)
{
    int i, rc;
    struct ompi_proc_t* ompi_proc;

    mca_ptl_ib_proc_t* module_proc;

    mca_ptl_base_peer_t* module_peer;

    D_PRINT("Adding %d procs\n", nprocs);

    for(i = 0; i < nprocs; i++) {

        ompi_proc = ompi_procs[i];
        module_proc = mca_ptl_ib_proc_create(ompi_proc);

        if(NULL == module_proc) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OMPI_THREAD_LOCK(&module_proc->proc_lock);
        if(module_proc->proc_addr_count == module_proc->proc_peer_count) {
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            return OMPI_ERR_UNREACH;
        }

        /* The ptl_proc datastructure is shared by all IB PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the ptl_proc.
         */
        module_peer = OBJ_NEW(mca_ptl_ib_peer_t);

        if(NULL == module_peer) {
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        module_peer->peer_module = (mca_ptl_ib_module_t*)base_module;

        rc = mca_ptl_ib_proc_insert(module_proc, module_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(module_peer);
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            return rc;
        }

        ompi_bitmap_set_bit(reachable, i);
        OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
        peers[i] = module_peer;
    }
    return OMPI_SUCCESS;
}

int mca_ptl_ib_del_procs(struct mca_ptl_base_module_t* ptl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_ptl_base_peer_t ** peers)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}

int mca_ptl_ib_finalize(struct mca_ptl_base_module_t* ptl)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}

int mca_ptl_ib_request_alloc(struct mca_ptl_base_module_t* ptl, 
        struct mca_pml_base_send_request_t** request)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}


void mca_ptl_ib_request_return(struct mca_ptl_base_module_t* ptl, 
        struct mca_pml_base_send_request_t* request)
{
    /* Stub */
    D_PRINT("Stub\n");
}

/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_ptl_ib_send( struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    int rc;
    mca_ptl_ib_send_frag_t* sendfrag;
    ompi_list_item_t* item;

    if (0 == offset) {
        sendfrag = &((mca_ptl_ib_send_request_t*)sendreq)->req_frag;
    } else {
        OMPI_FREE_LIST_GET(&mca_ptl_ib_component.ib_send_frags, item, rc);
        if(NULL == (sendfrag = (mca_ptl_ib_send_frag_t*)item)) {
            return rc;
        }
    }

    rc = mca_ptl_ib_send_frag_init(sendfrag, ptl_peer, sendreq, offset, &size, flags);

    if(rc != OMPI_SUCCESS) {
        return rc;
    }
    /* must update the offset after actual fragment
     * size is determined -- and very important --
     * before attempting to send the fragment
     */
    sendreq->req_offset += size;

    rc = mca_ptl_ib_peer_send(ptl_peer, sendfrag);

    return rc;
}


/*
 *  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */

void mca_ptl_ib_matched(
    mca_ptl_base_module_t* ptl,
    mca_ptl_base_recv_frag_t* frag)
{
    /* Stub */
    D_PRINT("Stub\n");
}

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

mca_ptl_ib_t mca_ptl_ib = {
    {
        &mca_ptl_ib_module.super,
        0, /* ptl_exclusivity */
        0, /* ptl_latency */
        0, /* ptl_andwidth */
        0, /* ptl_frag_first_size */
        0, /* ptl_frag_min_size */
        0, /* ptl_frag_max_size */
        MCA_PTL_PUT,  /* ptl flags */
        mca_ptl_ib_add_procs,
        mca_ptl_ib_del_procs,
        mca_ptl_ib_finalize,
        mca_ptl_ib_send,
        NULL,
        mca_ptl_ib_matched,
        mca_ptl_ib_request_alloc,
        mca_ptl_ib_request_return
    }
};

OBJ_CLASS_INSTANCE(mca_ptl_ib_recv_frag_t, 
        mca_ptl_base_recv_frag_t,
        NULL, NULL);

OBJ_CLASS_INSTANCE(mca_ptl_ib_send_request_t, 
        mca_pml_base_send_request_t,
        NULL, NULL);

OBJ_CLASS_INSTANCE(mca_ptl_ib_peer_t, 
        ompi_list_item_t,
        NULL, NULL);

OBJ_CLASS_INSTANCE(mca_ptl_ib_proc_t, 
        ompi_list_item_t,
        NULL, NULL);

int mca_ptl_ib_add_procs(
    struct mca_ptl_t* ptl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_ptl_base_peer_t** peers, 
    ompi_bitmap_t* reachable)
{
    int i, rc;
    fprintf(stderr,"[%s:%d] %s\n",
            __FILE__, __LINE__, __func__);

    for(i = 0; i < nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_ptl_ib_proc_t* ptl_proc = mca_ptl_ib_proc_create(ompi_proc);
        mca_ptl_base_peer_t* ptl_peer;

        if(NULL == ptl_proc) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OMPI_THREAD_LOCK(&ptl_proc->proc_lock);
        if(ptl_proc->proc_addr_count == ptl_proc->proc_peer_count) {
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return OMPI_ERR_UNREACH;
        }

        /* The ptl_proc datastructure is shared by all TCP PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the ptl_proc.
         */
        ptl_peer = OBJ_NEW(mca_ptl_ib_peer_t);

        if(NULL == ptl_peer) {
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ptl_peer->peer_ptl = (mca_ptl_ib_t*)ptl;

        /*
        rc = mca_ptl_ib_proc_insert(ptl_proc, ptl_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ptl_peer);
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return rc;
        }
        */
        ompi_bitmap_set_bit(reachable, i);
        OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
        peers[i] = ptl_peer;
    }
    return OMPI_SUCCESS;
}

int mca_ptl_ib_del_procs(struct mca_ptl_t* ptl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_ptl_base_peer_t ** peers)
{
    /* Stub */
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    return OMPI_SUCCESS;
}

int mca_ptl_ib_finalize(struct mca_ptl_t* ptl)
{
    /* Stub */
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    return OMPI_SUCCESS;
}

int mca_ptl_ib_request_alloc(struct mca_ptl_t* ptl, 
        struct mca_pml_base_send_request_t** request)
{
    /* Stub */
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    return OMPI_SUCCESS;
}


void mca_ptl_ib_request_return(struct mca_ptl_t* ptl, 
        struct mca_pml_base_send_request_t* request)
{
    /* Stub */
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
}

/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_ptl_ib_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    /* Stub */
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    return OMPI_SUCCESS;
}


/*
 *  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */

void mca_ptl_ib_matched(
    mca_ptl_t* ptl,
    mca_ptl_base_recv_frag_t* frag)
{
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    /* Stub */
}

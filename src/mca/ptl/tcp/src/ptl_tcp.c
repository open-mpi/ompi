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
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_proc.h"
#include "ptl_tcp_sendreq.h"
#include "ptl_tcp_recvfrag.h"


mca_ptl_tcp_t mca_ptl_tcp = {
    {
    &mca_ptl_tcp_module.super,
    0, /* ptl_exclusivity */
    0, /* ptl_latency */
    0, /* ptl_andwidth */
    0, /* ptl_frag_first_size */
    0, /* ptl_frag_min_size */
    0, /* ptl_frag_max_size */
    MCA_PTL_PUT,  /* ptl flags */
    mca_ptl_tcp_add_procs,
    mca_ptl_tcp_del_procs,
    mca_ptl_tcp_finalize,
    mca_ptl_tcp_send,
    NULL,
    mca_ptl_tcp_matched,
    mca_ptl_tcp_request_alloc,
    mca_ptl_tcp_request_return
    }
};


int mca_ptl_tcp_add_procs(
    struct mca_ptl_t* ptl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_ptl_base_peer_t** peers, 
    ompi_bitmap_t* reachable)
{
    size_t i;
    for(i=0; i<nprocs; i++) {
        struct ompi_proc_t *ompi_proc = ompi_procs[i];
        mca_ptl_tcp_proc_t* ptl_proc = mca_ptl_tcp_proc_create(ompi_proc);
        mca_ptl_base_peer_t* ptl_peer;
        int rc;

        if(NULL == ptl_proc)
            return OMPI_ERR_OUT_OF_RESOURCE;

        /* 
         * Check to make sure that the peer has at least as many interface addresses
         * exported as we are trying to use. If not, then don't bind this PTL instance
         * to the proc.
        */
        OMPI_THREAD_LOCK(&ptl_proc->proc_lock);
        if(ptl_proc->proc_addr_count == ptl_proc->proc_peer_count) {
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return OMPI_ERR_UNREACH;
        }

        /* The ptl_proc datastructure is shared by all TCP PTL instances that are trying 
         * to reach this destination. Cache the peer instance on the ptl_proc.
         */
        ptl_peer = OBJ_NEW(mca_ptl_tcp_peer_t);
        if(NULL == ptl_peer) {
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        ptl_peer->peer_ptl = (mca_ptl_tcp_t*)ptl;
        rc = mca_ptl_tcp_proc_insert(ptl_proc, ptl_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ptl_peer);
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return rc;
        }
        ompi_bitmap_set_bit(reachable, i);
        OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
        peers[i] = ptl_peer;
    }
    return OMPI_SUCCESS;
}

int mca_ptl_tcp_del_procs(struct mca_ptl_t* ptl, size_t nprocs, struct ompi_proc_t **procs, struct mca_ptl_base_peer_t ** peers)
{
    size_t i;
    for(i=0; i<nprocs; i++) {
        OBJ_RELEASE(peers[i]);
    }
    return OMPI_SUCCESS;
}

int mca_ptl_tcp_finalize(struct mca_ptl_t* ptl)
{
    free(ptl);
    return OMPI_SUCCESS;
}

int mca_ptl_tcp_request_alloc(struct mca_ptl_t* ptl, struct mca_pml_base_send_request_t** request)
{
    int rc;
    mca_pml_base_send_request_t* sendreq;
    ompi_list_item_t* item;
    OMPI_FREE_LIST_GET(&mca_ptl_tcp_module.tcp_send_requests, item, rc);
    if(NULL != (sendreq = (mca_pml_base_send_request_t*)item))
        sendreq->req_owner = ptl;
    *request = sendreq;
    return rc;
}


void mca_ptl_tcp_request_return(struct mca_ptl_t* ptl, struct mca_pml_base_send_request_t* request)
{
    /* OBJ_DESTRUCT(&request->req_convertor); */
    OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_send_requests, (ompi_list_item_t*)request);
}


void mca_ptl_tcp_recv_frag_return(struct mca_ptl_t* ptl, struct mca_ptl_tcp_recv_frag_t* frag)
{
    if(frag->super.frag_is_buffered) 
        free(frag->super.super.frag_addr);
    /* OBJ_DESTRUCT(&frag->super.super.frag_convertor); */
    OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_recv_frags, (ompi_list_item_t*)frag);
}


void mca_ptl_tcp_send_frag_return(struct mca_ptl_t* ptl, struct mca_ptl_tcp_send_frag_t* frag)
{
    if(ompi_list_get_size(&mca_ptl_tcp_module.tcp_pending_acks)) {
        mca_ptl_tcp_recv_frag_t* pending;
        OMPI_THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
        pending = (mca_ptl_tcp_recv_frag_t*)ompi_list_remove_first(&mca_ptl_tcp_module.tcp_pending_acks);
        if(NULL == pending) {
            OMPI_THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
            OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_send_frags, (ompi_list_item_t*)frag);
            return;
        }
        OMPI_THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
        mca_ptl_tcp_send_frag_init_ack(frag, ptl, pending->super.super.frag_peer, pending);
        mca_ptl_tcp_peer_send(pending->super.super.frag_peer, frag);
        mca_ptl_tcp_recv_frag_return(ptl, pending);
    } else {
        OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_send_frags, (ompi_list_item_t*)frag);
    }
}

/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_ptl_tcp_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    mca_ptl_tcp_send_frag_t* sendfrag;
    int rc;
    if (offset == 0) {
        sendfrag = &((mca_ptl_tcp_send_request_t*)sendreq)->req_frag;
    } else {
        ompi_list_item_t* item;
        OMPI_FREE_LIST_GET(&mca_ptl_tcp_module.tcp_send_frags, item, rc);
        if(NULL == (sendfrag = (mca_ptl_tcp_send_frag_t*)item))
            return rc;
    }
    rc = mca_ptl_tcp_send_frag_init(sendfrag, ptl_peer, sendreq, offset, &size, flags);
    if(rc != OMPI_SUCCESS)
        return rc;
    /* must update the offset after actual fragment size is determined -- and very important --
     * before attempting to send the fragment 
     */
    sendreq->req_offset += size;
    return mca_ptl_tcp_peer_send(ptl_peer, sendfrag);
}


/*
 *  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */

void mca_ptl_tcp_matched(
    mca_ptl_t* ptl,
    mca_ptl_base_recv_frag_t* frag)
{
    /* send ack back to peer? */
    mca_ptl_base_header_t* header = &frag->super.frag_header;
    if(header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) {
        int rc;
        mca_ptl_tcp_send_frag_t* ack;
        mca_ptl_tcp_recv_frag_t* recv_frag = (mca_ptl_tcp_recv_frag_t*)frag;
        ompi_list_item_t* item;
        MCA_PTL_TCP_SEND_FRAG_ALLOC(item, rc);
        ack = (mca_ptl_tcp_send_frag_t*)item;

        if(NULL == ack) {
            OMPI_THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
            recv_frag->frag_ack_pending = true;
            ompi_list_append(&mca_ptl_tcp_module.tcp_pending_acks, (ompi_list_item_t*)frag);
            OMPI_THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
        } else {
            mca_ptl_tcp_send_frag_init_ack(ack, ptl, recv_frag->super.super.frag_peer, recv_frag);
            mca_ptl_tcp_peer_send(ack->super.super.frag_peer, ack);
        }
    }

    /* process fragment if complete */
    mca_ptl_tcp_recv_frag_progress((mca_ptl_tcp_recv_frag_t*)frag);
}



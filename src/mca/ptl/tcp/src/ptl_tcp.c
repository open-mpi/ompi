/*
 * $HEADER$
 */

#include "lam/util/output.h"
#include "lam/util/if.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_header.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_sendfrag.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"
#include "mca/lam/base/mca_base_module_exchange.h"
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
    mca_ptl_tcp_add_proc,
    mca_ptl_tcp_del_proc,
    mca_ptl_tcp_finalize,
    mca_ptl_tcp_send,
    mca_ptl_tcp_recv,
    mca_ptl_tcp_request_alloc,
    mca_ptl_tcp_request_return,
    (mca_ptl_base_frag_return_fn_t)mca_ptl_tcp_recv_frag_return
    }
};


int mca_ptl_tcp_create(int if_index)
{
    mca_ptl_tcp_t* ptl = malloc(sizeof(mca_ptl_tcp_t));
    if(NULL == ptl)
        return LAM_ERR_OUT_OF_RESOURCE;
    memcpy(ptl, &mca_ptl_tcp, sizeof(mca_ptl_tcp));
    mca_ptl_tcp_module.tcp_ptls[mca_ptl_tcp_module.tcp_num_ptls++] = ptl;

    /* initialize the ptl */
    ptl->ptl_ifindex = if_index;
    lam_ifindextoaddr(if_index, (struct sockaddr*)&ptl->ptl_ifaddr, sizeof(ptl->ptl_ifaddr));
    lam_ifindextomask(if_index, (struct sockaddr*)&ptl->ptl_ifmask, sizeof(ptl->ptl_ifmask));
    return LAM_SUCCESS;
}


int mca_ptl_tcp_add_proc(struct mca_ptl_t* ptl, struct lam_proc_t *lam_proc, struct mca_ptl_base_peer_t** peer_ret)
{
    mca_ptl_tcp_proc_t* ptl_proc = mca_ptl_tcp_proc_create(lam_proc);
    mca_ptl_base_peer_t* ptl_peer;
    int rc;

    if(NULL == ptl_proc)
        return LAM_ERR_OUT_OF_RESOURCE;

    /* 
     * Check to make sure that the peer has at least as many interface addresses
     * exported as we are trying to use. If not, then don't bind this PTL instance
     * to the proc.
    */
    THREAD_LOCK(&ptl_proc->proc_lock);
    if(ptl_proc->proc_addr_count == ptl_proc->proc_peer_count) {
        THREAD_UNLOCK(&ptl_proc->proc_lock);
        return LAM_ERR_UNREACH;
    }

    /* The ptl_proc datastructure is shared by all TCP PTL instances that are trying 
     * to reach this destination. Cache the peer instance on the ptl_proc.
     */
    ptl_peer = OBJ_NEW(mca_ptl_tcp_peer_t);
    if(NULL == ptl_peer) {
        THREAD_UNLOCK(&ptl_proc->proc_lock);
        return LAM_ERR_OUT_OF_RESOURCE;
    }
    ptl_peer->peer_ptl = (mca_ptl_tcp_t*)ptl;
    rc = mca_ptl_tcp_proc_insert(ptl_proc, ptl_peer);
    if(rc != LAM_SUCCESS) {
        OBJ_RELEASE(ptl_peer);
        THREAD_UNLOCK(&ptl_proc->proc_lock);
        return rc;
    }
    THREAD_UNLOCK(&ptl_proc->proc_lock);
    *peer_ret = ptl_peer;
    return LAM_SUCCESS;
}


int mca_ptl_tcp_del_proc(struct mca_ptl_t* ptl, struct lam_proc_t *proc, struct mca_ptl_base_peer_t* ptl_peer)
{
    OBJ_RELEASE(ptl_peer);
    return LAM_SUCCESS;
}

int mca_ptl_tcp_finalize(struct mca_ptl_t* ptl)
{
    free(ptl);
    return LAM_SUCCESS;
}

int mca_ptl_tcp_request_alloc(struct mca_ptl_t* ptl, struct mca_ptl_base_send_request_t** request)
{
    int rc;
    mca_ptl_base_send_request_t* sendreq =
        (mca_ptl_base_send_request_t*)lam_free_list_get(&mca_ptl_tcp_module.tcp_send_requests, &rc);
    if(NULL != sendreq)
        sendreq->req_owner = ptl;
    *request = sendreq;
    return rc;
}


void mca_ptl_tcp_request_return(struct mca_ptl_t* ptl, struct mca_ptl_base_send_request_t* request)
{
    lam_free_list_return(&mca_ptl_tcp_module.tcp_send_requests, (lam_list_item_t*)request);
}


void mca_ptl_tcp_recv_frag_return(struct mca_ptl_t* ptl, struct mca_ptl_tcp_recv_frag_t* frag)
{
    if(frag->frag_buff != NULL && frag->frag_buff != frag->super.super.frag_addr)
        free(frag->frag_buff);
    lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
}


void mca_ptl_tcp_send_frag_return(struct mca_ptl_t* ptl, struct mca_ptl_tcp_send_frag_t* frag)
{
    if(lam_list_get_size(&mca_ptl_tcp_module.tcp_pending_acks)) {
        THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
        mca_ptl_tcp_recv_frag_t* pending = (mca_ptl_tcp_recv_frag_t*)
            lam_list_remove_first(&mca_ptl_tcp_module.tcp_pending_acks);
        if(NULL == pending) {
            THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
            lam_free_list_return(&mca_ptl_tcp_module.tcp_send_frags, (lam_list_item_t*)frag);
            return;
        }
        mca_ptl_tcp_send_frag_init_ack(frag, ptl, pending->super.super.frag_peer, pending);
        mca_ptl_tcp_peer_send(pending->super.super.frag_peer, frag);
        THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
        mca_ptl_tcp_recv_frag_return(ptl, pending);
    } else {
        lam_free_list_return(&mca_ptl_tcp_module.tcp_send_frags, (lam_list_item_t*)frag);
    }
}


int mca_ptl_tcp_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_ptl_base_send_request_t* sendreq,
    size_t size,
    int flags)
{
    mca_ptl_tcp_send_frag_t* sendfrag;
    if (sendreq->req_frags == 0) {
        sendfrag = &((mca_ptl_tcp_send_request_t*)sendreq)->req_frag;
    } else {
        int rc;
        sendfrag = (mca_ptl_tcp_send_frag_t*)lam_free_list_get(&mca_ptl_tcp_module.tcp_send_frags, &rc);
        if(NULL == sendfrag)
            return rc;
    }
    mca_ptl_tcp_send_frag_init(sendfrag, ptl_peer, sendreq, size, flags);
    return mca_ptl_tcp_peer_send(ptl_peer, sendfrag);
}


void mca_ptl_tcp_recv(
    mca_ptl_t* ptl,
    mca_ptl_base_recv_frag_t* frag,
    lam_status_public_t* status)
{
    mca_ptl_base_header_t* header = &frag->super.frag_header;

    /* fill in match */
    status->MPI_SOURCE = header->hdr_match.hdr_src;
    status->MPI_TAG = header->hdr_match.hdr_tag;
    status->MPI_ERROR = LAM_SUCCESS;
    status->_count = header->hdr_match.hdr_msg_length;

    /* send ack back to peer */
    if(header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) {
        int rc;
        mca_ptl_tcp_send_frag_t* ack = mca_ptl_tcp_send_frag_alloc(&rc);
        mca_ptl_tcp_recv_frag_t* recv_frag = (mca_ptl_tcp_recv_frag_t*)frag;
        if(NULL == ack) {
            THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
            recv_frag->frag_ack_pending = true;
            lam_list_append(&mca_ptl_tcp_module.tcp_pending_acks, (lam_list_item_t*)frag);
            THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
        } else {
            mca_ptl_tcp_send_frag_init_ack(ack, ptl, recv_frag->super.super.frag_peer, recv_frag);
            mca_ptl_tcp_peer_send(ack->super.super.frag_peer, ack);
        }
    }

    /* process fragment if complete */
    mca_ptl_tcp_recv_frag_progress((mca_ptl_tcp_recv_frag_t*)frag);
}


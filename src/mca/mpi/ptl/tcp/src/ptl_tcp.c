/*
 * $HEADER$
 */

#include "lam/mem/malloc.h"
#include "lam/util/output.h"
#include "lam/util/if.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/lam/base/mca_base_module_exchange.h"
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_proc.h"


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
    mca_ptl_tcp_fini,
    mca_ptl_tcp_send,
    mca_ptl_tcp_request_alloc,
    mca_ptl_tcp_request_return
    }
};


int mca_ptl_tcp_create(int if_index)
{
    mca_ptl_tcp_t* ptl = (mca_ptl_tcp_t*)LAM_MALLOC(sizeof(mca_ptl_tcp_t));
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


int mca_ptl_tcp_add_proc(struct mca_ptl_t* ptl, struct lam_proc_t *lam_proc, struct mca_ptl_peer_t** peer_ret)
{
    mca_ptl_tcp_proc_t* ptl_proc = mca_ptl_tcp_proc_create(lam_proc);
    mca_ptl_peer_t* ptl_peer;
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
    ptl_peer = OBJ_CREATE(mca_ptl_peer_t, &mca_ptl_tcp_peer_cls);
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


int mca_ptl_tcp_del_proc(struct mca_ptl_t* ptl, struct lam_proc_t *proc, struct mca_ptl_peer_t* ptl_peer)
{
    OBJ_RELEASE(ptl_peer);
    return LAM_SUCCESS;
}

int mca_ptl_tcp_fini(struct mca_ptl_t* ptl)
{
    
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


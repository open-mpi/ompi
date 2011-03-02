/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2007-2009 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_IB_ENDPOINT_H
#define MCA_BTL_IB_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "ompi/mca/btl/btl.h"
#include "btl_wv.h"
#include "btl_wv_frag.h"
#include "btl_wv_eager_rdma.h" 
#include <errno.h>
#include <string.h>
#include "ompi/mca/btl/base/btl_base_error.h"
#include "connect/base.h"

BEGIN_C_DECLS

struct mca_btl_wv_frag_t;
struct mca_btl_wv_proc_modex_t;

/**
 * State of IB endpoint connection.
 */

typedef enum {
    /* Defines the state in which this BTL instance
     * has started the process of connection */
    MCA_BTL_IB_CONNECTING,

    /* Waiting for ack from endpoint */
    MCA_BTL_IB_CONNECT_ACK,

    /*Waiting for final connection ACK from endpoint */
    MCA_BTL_IB_WAITING_ACK,

    /* Connected ... both sender & receiver have
     * buffers associated with this connection */
    MCA_BTL_IB_CONNECTED,

    /* Connection is closed, there are no resources
     * associated with this */
    MCA_BTL_IB_CLOSED,

    /* Maximum number of retries have been used.
     * Report failure on send to upper layer */
    MCA_BTL_IB_FAILED
} mca_btl_wv_endpoint_state_t;

typedef struct mca_btl_wv_rem_qp_info_t {
    uint32_t                    rem_qp_num;
    /* Remote QP number */
    uint32_t                    rem_psn;
    /* Remote processes port sequence number */
} mca_btl_wv_rem_qp_info_t;

typedef struct mca_btl_wv_rem_srq_info_t {
    /* Remote SRQ number */
    uint32_t                    rem_srq_num;
} mca_btl_wv_rem_srq_info_t;

typedef struct mca_btl_wv_rem_info_t {
    /* Local identifier of the remote process */
    uint16_t                    rem_lid;
    /* subnet id of remote process */
    uint64_t                    rem_subnet_id;
    /* MTU of remote process */
    uint32_t                    rem_mtu;
    /* index of remote endpoint in endpoint array */
    uint32_t                    rem_index;
    /* Remote QPs */
    mca_btl_wv_rem_qp_info_t *rem_qps;
    /* Remote xrc_srq info, used only with XRC connections */
    mca_btl_wv_rem_srq_info_t *rem_srqs;
    /* Vendor id of remote HCA */
    uint32_t rem_vendor_id;
    /* Vendor part id of remote HCA */
    uint32_t rem_vendor_part_id;
    /* Transport type of remote port */
    mca_btl_wv_transport_type_t rem_transport_type;
} mca_btl_wv_rem_info_t;


/**
 *  Agggregates all per peer qp info for an endpoint
 */
typedef struct mca_btl_wv_endpoint_pp_qp_t {
    int32_t sd_credits;  /**< this rank's view of the credits
                          *  available for sending:
                          *  this is the credits granted by the
                          *  remote peer which has some relation to the
                          *  number of receive buffers posted remotely
                          */
    int32_t  rd_posted;   /**< number of descriptors posted to the nic*/
    int32_t  rd_credits;  /**< number of credits to return to peer */
    int32_t  cm_received; /**< Credit messages received */
    int32_t  cm_return;   /**< how may credits to return */
    int32_t  cm_sent;     /**< Outstanding number of credit messages */
} mca_btl_wv_endpoint_pp_qp_t;


/**
 *  Aggregates all srq qp info for an endpoint
 */
typedef struct mca_btl_wv_endpoint_srq_qp_t {
    int32_t dummy;
} mca_btl_wv_endpoint_srq_qp_t;

typedef struct mca_btl_wv_qp_t {
    struct wv_qp *lcl_qp;
    uint32_t lcl_psn;
    int32_t  sd_wqe;      /**< number of available send wqe entries */
    int users;
    opal_mutex_t lock;
} mca_btl_wv_qp_t;

typedef struct mca_btl_wv_endpoint_qp_t {
    mca_btl_wv_qp_t *qp;
    opal_list_t no_credits_pending_frags[2]; /**< put fragment here if there is no credits
                                     available */
    opal_list_t no_wqe_pending_frags[2]; /**< put fragments here if there is no wqe
                                    available  */
    int32_t  rd_credit_send_lock;  /**< Lock credit send fragment */
    mca_btl_wv_send_control_frag_t *credit_frag;
    size_t ib_inline_max;          /**< max size of inline send*/
    union {
        mca_btl_wv_endpoint_srq_qp_t srq_qp;
        mca_btl_wv_endpoint_pp_qp_t pp_qp;
    } u;
} mca_btl_wv_endpoint_qp_t;

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    /** BTL module that created this connection */
    struct mca_btl_wv_module_t* endpoint_btl;

    /** proc structure corresponding to endpoint */
    struct mca_btl_wv_proc_t*   endpoint_proc;

    /** local CPC to connect to this endpoint */
    ompi_btl_wv_connect_base_module_t *endpoint_local_cpc;

    /** hook for local CPC to hang endpoint-specific data */
    void *endpoint_local_cpc_data;

    /** If endpoint_local_cpc->cbm_uses_cts is true and this endpoint
        is iWARP, then endpoint_initiator must be true on the side
        that actually initiates the QP, false on the other side.  This
        bool is used to know which way to send the first CTS
        message. */
    bool endpoint_initiator;

    /** pointer to remote proc's CPC data (essentially its CPC modex
        message) */
    ompi_btl_wv_connect_base_module_data_t *endpoint_remote_cpc_data;

    /** current state of the connection */
    mca_btl_wv_endpoint_state_t     endpoint_state;

    /** number of connection retries attempted */
    size_t                      endpoint_retries;

    /** timestamp of when the first connection was attempted */
    double                      endpoint_tstamp;

    /** lock for concurrent access to endpoint state */
    opal_mutex_t                endpoint_lock;

    /** list of pending frags due to lazy connection establishment
        for this endpotint */
    opal_list_t                 pending_lazy_frags;

    mca_btl_wv_endpoint_qp_t *qps;

    /** list of pending rget ops */
    opal_list_t                 pending_get_frags;
    /** list of pending rput ops */
    opal_list_t                 pending_put_frags; 

    /** number of available get tokens */
    int32_t                     get_tokens;

    /** subnet id of this endpoint*/
    uint64_t subnet_id;

    /** number of eager received */
    int32_t eager_recv_count;
    /** info about remote RDMA buffer */
    mca_btl_wv_eager_rdma_remote_t eager_rdma_remote;
    /** info about local RDMA buffer */
    mca_btl_wv_eager_rdma_local_t eager_rdma_local;
    /** index of the endpoint in endpoints array */
    int32_t index;

    /** does the endpoint require network byte ordering? */
    bool nbo;
    /** use eager rdma for this peer? */
    bool use_eager_rdma;

    /** information about the remote port */
    mca_btl_wv_rem_info_t rem_info;

    /** Frag for initial wireup CTS protocol; will be NULL if CPC
        indicates that it does not want to use CTS */
    mca_btl_wv_recv_frag_t endpoint_cts_frag;
    /** Memory registration info for the CTS frag */
    struct wv_mr *endpoint_cts_mr;

    /** Whether we've posted receives on this EP or not (only used in
        CTS protocol) */
    bool endpoint_posted_recvs;

    /** Whether we've received the CTS from the peer or not (only used
        in CTS protocol) */
    bool endpoint_cts_received;

    /** Whether we've send out CTS to the peer or not (only used in
        CTS protocol) */
    bool endpoint_cts_sent;
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_wv_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_wv_endpoint_t);

static inline int32_t qp_get_wqe(mca_btl_wv_endpoint_t *ep, const int qp)
{
    return OPAL_THREAD_ADD32(&ep->qps[qp].qp->sd_wqe, -1);
}

static inline int32_t qp_put_wqe(mca_btl_wv_endpoint_t *ep, const int qp)
{
    return OPAL_THREAD_ADD32(&ep->qps[qp].qp->sd_wqe, 1);
}

int mca_btl_wv_endpoint_send(mca_btl_base_endpoint_t*,
        mca_btl_wv_send_frag_t*);
int mca_btl_wv_endpoint_post_send(mca_btl_wv_endpoint_t*,
        mca_btl_wv_send_frag_t*);
void mca_btl_wv_endpoint_send_credits(mca_btl_base_endpoint_t*, const int);
void mca_btl_wv_endpoint_connect_eager_rdma(mca_btl_wv_endpoint_t*);
int mca_btl_wv_endpoint_post_recvs(mca_btl_wv_endpoint_t*);
void mca_btl_wv_endpoint_send_cts(mca_btl_wv_endpoint_t *endpoint);
void mca_btl_wv_endpoint_cpc_complete(mca_btl_wv_endpoint_t*);
void mca_btl_wv_endpoint_connected(mca_btl_wv_endpoint_t*);
void mca_btl_wv_endpoint_init(mca_btl_wv_module_t*,
                                  mca_btl_base_endpoint_t*,
                                  ompi_btl_wv_connect_base_module_t *local_cpc,
                                  struct mca_btl_wv_proc_modex_t *remote_proc_info,
                                  ompi_btl_wv_connect_base_module_data_t *remote_cpc_data);

/*
 * Invoke an error on the btl associated with an endpoint.  If we
 * don't have an endpoint, then just use the first one on the
 * component list of BTLs.
 */
void *mca_btl_wv_endpoint_invoke_error(void *endpoint);

static inline int post_recvs(mca_btl_base_endpoint_t *ep, const int qp,
        const int num_post)
{
    int i;
    struct wv_recv_wr *wr = NULL;
    mca_btl_wv_module_t *wv_btl = ep->endpoint_btl;
    HRESULT hr = 0;
    if(0 == num_post)
        return OMPI_SUCCESS;
    for(i = 0; i < num_post; i++) {
        int rc;
        ompi_free_list_item_t* item;
        OMPI_FREE_LIST_WAIT(&wv_btl->device->qps[qp].recv_free, item, rc);
        to_base_frag(item)->base.order = qp;
        to_com_frag(item)->endpoint = ep;
        wr = &to_recv_frag(item)->rd_desc;
        hr = ep->qps[qp].qp->lcl_qp->handle->PostReceive(wr->wr_id,wr->sg_list,
             wr->num_sge);
    }
    if(SUCCEEDED(hr)) {
        return OMPI_SUCCESS;
    }else {
        BTL_ERROR(("posting receive on qp %d", qp));
        return OMPI_ERROR;
    }
}

static inline int mca_btl_wv_endpoint_post_rr_nolock(
        mca_btl_base_endpoint_t *ep, const int qp)
{
    int rd_rsv = mca_btl_wv_component.qp_infos[qp].u.pp_qp.rd_rsv;
    int rd_num = mca_btl_wv_component.qp_infos[qp].rd_num;
    int rd_low = mca_btl_wv_component.qp_infos[qp].rd_low;
    int cqp = mca_btl_wv_component.credits_qp, rc;
    int cm_received = 0, num_post = 0;

    assert(BTL_WV_QP_TYPE_PP(qp));

    if(ep->qps[qp].u.pp_qp.rd_posted <= rd_low)
        num_post = rd_num - ep->qps[qp].u.pp_qp.rd_posted;

    assert(num_post >= 0);

    if(ep->qps[qp].u.pp_qp.cm_received >= (rd_rsv >> 2))
        cm_received = ep->qps[qp].u.pp_qp.cm_received;

    if((rc = post_recvs(ep, qp, num_post)) != OMPI_SUCCESS) {
        return rc;
    }
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.rd_posted, num_post);
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.rd_credits, num_post);

    /* post buffers for credit management on credit management qp */
    if((rc = post_recvs(ep, cqp, cm_received)) != OMPI_SUCCESS) {
        return rc;
    }
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.cm_return, cm_received);
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.cm_received, -cm_received);

    assert(ep->qps[qp].u.pp_qp.rd_credits <= rd_num &&
            ep->qps[qp].u.pp_qp.rd_credits >= 0);

    return OMPI_SUCCESS;
}

static inline int mca_btl_wv_endpoint_post_rr(
        mca_btl_base_endpoint_t *ep, const int qp)
{
    int ret;
    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    ret =  mca_btl_wv_endpoint_post_rr_nolock(ep, qp);
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    return ret;
}

#define BTL_WV_CREDITS_SEND_TRYLOCK(E, Q) \
    OPAL_ATOMIC_CMPSET_32(&(E)->qps[(Q)].rd_credit_send_lock, 0, 1)
#define BTL_WV_CREDITS_SEND_UNLOCK(E, Q) \
    OPAL_ATOMIC_CMPSET_32(&(E)->qps[(Q)].rd_credit_send_lock, 1, 0)
#define BTL_WV_GET_CREDITS(FROM, TO)                                        \
    do {                                                     \
        TO = FROM;                                           \
    } while(0 == OPAL_ATOMIC_CMPSET_32(&FROM, TO, 0))


static inline bool check_eager_rdma_credits(const mca_btl_wv_endpoint_t *ep)
{
    return (ep->eager_rdma_local.credits > ep->eager_rdma_local.rd_win) ? true :
        false;
}

static inline bool
check_send_credits(const mca_btl_wv_endpoint_t *ep, const int qp)
{

    if(!BTL_WV_QP_TYPE_PP(qp))
        return false;

    return (ep->qps[qp].u.pp_qp.rd_credits >=
            mca_btl_wv_component.qp_infos[qp].u.pp_qp.rd_win) ? true : false;
}

static inline void send_credits(mca_btl_wv_endpoint_t *ep, int qp)
{
    if(BTL_WV_QP_TYPE_PP(qp)) {
        if(check_send_credits(ep, qp))
            goto try_send;
    } else {
        qp = mca_btl_wv_component.credits_qp;
    }

    if(!check_eager_rdma_credits(ep))
        return;

try_send:
    if(BTL_WV_CREDITS_SEND_TRYLOCK(ep, qp))
        mca_btl_wv_endpoint_send_credits(ep, qp);
}

static inline int check_endpoint_state(mca_btl_wv_endpoint_t *ep,
        mca_btl_base_descriptor_t *des, opal_list_t *pending_list)
{
    int rc = OMPI_ERR_RESOURCE_BUSY;

    switch(ep->endpoint_state) {
        case MCA_BTL_IB_CLOSED:
            rc = ep->endpoint_local_cpc->cbm_start_connect(ep->endpoint_local_cpc, ep);
            if (OMPI_SUCCESS == rc) {
                rc = OMPI_ERR_RESOURCE_BUSY;
            }
            /*
             * As long as we expect a message from the peer (in order
             * to setup the connection) let the event engine pool the
             * OOB events. Note: we increment it once peer active
             * connection.
             */
            opal_progress_event_users_increment();
            /* fall through */
        default:
            opal_list_append(pending_list, (opal_list_item_t *)des);
            break;
        case MCA_BTL_IB_FAILED:
            rc = OMPI_ERR_UNREACH;
            break;
        case MCA_BTL_IB_CONNECTED:
            rc = OMPI_SUCCESS;
            break;
    }

    return rc;
}

static inline __opal_attribute_always_inline__ int
ib_send_flags(uint32_t size, mca_btl_wv_endpoint_qp_t *qp)
{
    return WV_SEND_SIGNALED |
        ((size <= qp->ib_inline_max) ? WV_SEND_INLINE : 0);
}

static inline int
acquire_eager_rdma_send_credit(mca_btl_wv_endpoint_t *endpoint)
{
    if(OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, -1) < 0) {
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

#define ntohll(x) (((_int64)(ntohl((int)((x << 32) >> 32))) << 32)|(unsigned int)ntohl(((int)(x >> 32)))) 
#define htonll(x) ntohll(x)

static inline int post_send(mca_btl_wv_endpoint_t *ep,
        mca_btl_wv_send_frag_t *frag, const bool rdma)
{
    mca_btl_wv_module_t *wv_btl = ep->endpoint_btl;
    mca_btl_base_segment_t *seg = &to_base_frag(frag)->segment;
    WV_SGE *sg = &to_com_frag(frag)->sg_entry;
    WV_SEND_REQUEST *sr_desc = &to_out_frag(frag)->sr_desc;
    WV_SEND_REQUEST *bad_wr;
    HRESULT hr = 0;
    int qp = to_base_frag(frag)->base.order;
    sg->Length = seg->seg_len + sizeof(mca_btl_wv_header_t) +
        (rdma ? sizeof(mca_btl_wv_footer_t) : 0) + frag->coalesced_length;
    sr_desc->Flags = ib_send_flags(sg->Length, &(ep->qps[qp]));
    if(ep->nbo)
        BTL_WV_HEADER_HTON(*frag->hdr);
    if(rdma) {
        int32_t head;
        mca_btl_wv_footer_t* ftr =
            (mca_btl_wv_footer_t*)(((char*)frag->hdr) + sg->Length -
                    sizeof(mca_btl_wv_footer_t));
        sr_desc->Opcode = WvRdmaWrite;
        MCA_BTL_WV_RDMA_FRAG_SET_SIZE(ftr, sg->Length);
        MCA_BTL_WV_RDMA_MAKE_LOCAL(ftr);
        if(ep->nbo)
            BTL_WV_FOOTER_HTON(*ftr);
        sr_desc->Wr.Rdma.Rkey = htonl(ep->eager_rdma_remote.rkey);
        MCA_BTL_WV_RDMA_MOVE_INDEX(ep->eager_rdma_remote.head, head);
        sr_desc->Wr.Rdma.RemoteAddress = 
            ep->eager_rdma_remote.base.lval +
            head * wv_btl->eager_rdma_frag_size +
            sizeof(mca_btl_wv_header_t) +
            mca_btl_wv_component.eager_limit +
            sizeof(mca_btl_wv_footer_t);
        sr_desc->Wr.Rdma.RemoteAddress = htonll(sr_desc->Wr.Rdma.RemoteAddress - sg->Length);
    } else {
        if(BTL_WV_QP_TYPE_PP(qp)) {
            sr_desc->Opcode = WvSend;
        } else {
            sr_desc->Opcode = WvSend;
            sr_desc->Flags |= WV_SEND_IMMEDIATE;
            sr_desc->ImmediateData = ep->rem_info.rem_index;
        }
    }
    assert(sg->pAddress == (void*)(uintptr_t)frag->hdr);
    hr = ep->qps[qp].qp->lcl_qp->handle->PostSend(sr_desc,
                                                  (WV_SEND_REQUEST**)&bad_wr);
    if(SUCCEEDED(hr))
        return 0;
    else
        return 1;
}

END_C_DECLS

#endif

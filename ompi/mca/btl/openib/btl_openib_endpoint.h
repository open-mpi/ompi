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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_IB_ENDPOINT_H
#define MCA_BTL_IB_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_eager_rdma.h"
#include <errno.h> 
#include <string.h> 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "connect/base.h"

BEGIN_C_DECLS

struct mca_btl_openib_frag_t;

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
} mca_btl_openib_endpoint_state_t;

struct mca_btl_openib_rem_qp_info_t { 
    uint32_t                    rem_qp_num; 
    /* Remote QP number */ 
    uint32_t                    rem_psn; 
    /* Remote processes port sequence number */ 
}; typedef struct mca_btl_openib_rem_qp_info_t mca_btl_openib_rem_qp_info_t; 

struct mca_btl_openib_rem_srq_info_t {
    /* Remote SRQ number */
    uint32_t                    rem_srq_num;
}; typedef struct mca_btl_openib_rem_srq_info_t mca_btl_openib_rem_srq_info_t;

struct mca_btl_openib_rem_info_t { 
    uint16_t                    rem_lid;
    /* Local identifier of the remote process */
    uint64_t                    rem_subnet_id; 
    /* subnet id of remote process */     
    uint32_t                    rem_mtu;
    /* MTU of remote process */
    uint32_t                    rem_index;
    /* index of remote endpoint in endpoint array */
    mca_btl_openib_rem_qp_info_t *rem_qps;
    /* remote xrc_srq info , used only with xrc connections */
    mca_btl_openib_rem_srq_info_t *rem_srqs;
}; typedef struct mca_btl_openib_rem_info_t mca_btl_openib_rem_info_t; 


/**
 *  Agggregates all per peer qp info for an endpoint 
 */
struct mca_btl_openib_endpoint_pp_qp_t { 
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
}; typedef struct mca_btl_openib_endpoint_pp_qp_t mca_btl_openib_endpoint_pp_qp_t;


/**
 *  Aggregates all srq qp info for an endpoint 
 */
struct mca_btl_openib_endpoint_srq_qp_t { 
    int32_t dummy;
}; typedef struct mca_btl_openib_endpoint_srq_qp_t mca_btl_openib_endpoint_srq_qp_t;

typedef struct mca_btl_openib_qp_t {
    struct ibv_qp *lcl_qp;
    uint32_t lcl_psn;
    int32_t  sd_wqe;      /**< number of available send wqe entries */
    opal_list_t pending_frags[2]; /**< put fragments here if there is no wqe
                                    available  */
    int users;
    opal_mutex_t lock;
} mca_btl_openib_qp_t;

typedef struct mca_btl_openib_endpoint_qp_t {
    mca_btl_openib_qp_t *qp;
    opal_list_t pending_frags[2]; /**< put fragment here if there is no credits
                                     available */
    int32_t  rd_credit_send_lock;  /**< Lock credit send fragment */
    mca_btl_openib_send_control_frag_t *credit_frag;
    union {
        mca_btl_openib_endpoint_srq_qp_t srq_qp;
        mca_btl_openib_endpoint_pp_qp_t pp_qp;
    } u;
} mca_btl_openib_endpoint_qp_t;

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_openib_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_openib_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    mca_btl_openib_endpoint_state_t     endpoint_state;
    /**< current state of the connection */

    size_t                      endpoint_retries;
    /**< number of connection retries attempted */

    double                      endpoint_tstamp;
    /**< timestamp of when the first connection was attempted */

    opal_mutex_t                endpoint_lock;
    /**< lock for concurrent access to endpoint state */
    
    opal_list_t                 pending_lazy_frags;
    /**< list of pending frags due to lazy connection establishment 
     *   for this endpotint 
     */
    
    mca_btl_openib_endpoint_qp_t *qps;
    struct ibv_qp *xrc_recv_qp; /* in xrc we will use it as recv qp */
    uint32_t xrc_recv_psn; 

       
    opal_list_t                 pending_get_frags; /**< list of pending rget ops */
    opal_list_t                 pending_put_frags; /**< list of pending rput ops */

    
    
    
    /* Local processes port sequence number (Low and High) */
 
    
    int32_t                     get_tokens;    /**< number of available get tokens */


    uint64_t subnet_id; /**< subnet id of this endpoint*/
    struct ib_address_t *ib_addr; /**< used only for xrc; pointer to struct
                                    that keeps remote port info */

    int32_t eager_recv_count; /**< number of eager received */
    mca_btl_openib_eager_rdma_remote_t eager_rdma_remote;
    /**< info about remote RDMA buffer */
    mca_btl_openib_eager_rdma_local_t eager_rdma_local;
    /**< info about local RDMA buffer */
    uint32_t index;           /**< index of the endpoint in endpoints array */
    
    /**< frags for sending explicit high priority credits */
    bool nbo;       /**< does the endpoint require network byte ordering? */
    bool use_eager_rdma; /**< use eager rdma for this peer? */
    
    mca_btl_openib_rem_info_t rem_info; 
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_openib_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_openib_endpoint_t);

static inline int32_t qp_get_wqe(mca_btl_openib_endpoint_t *ep, const int qp)
{
    return OPAL_THREAD_ADD32(&ep->qps[qp].qp->sd_wqe, -1);
}

static inline int32_t qp_put_wqe(mca_btl_openib_endpoint_t *ep, const int qp)
{
    return OPAL_THREAD_ADD32(&ep->qps[qp].qp->sd_wqe, 1);
}

int mca_btl_openib_endpoint_send(mca_btl_base_endpoint_t*,
        mca_btl_openib_send_frag_t*);
int mca_btl_openib_endpoint_post_send(mca_btl_openib_endpoint_t*,
        mca_btl_openib_send_frag_t*);
void mca_btl_openib_endpoint_send_credits(mca_btl_base_endpoint_t*, const int);
void mca_btl_openib_endpoint_connect_eager_rdma(mca_btl_openib_endpoint_t*);
int mca_btl_openib_endpoint_post_recvs(mca_btl_openib_endpoint_t*);
void mca_btl_openib_endpoint_connected(mca_btl_openib_endpoint_t*);
void mca_btl_openib_endpoint_init(mca_btl_openib_module_t*,
        mca_btl_base_endpoint_t*);

static inline int post_recvs(mca_btl_base_endpoint_t *ep, const int qp,
        const int num_post)
{
    int i;
    struct ibv_recv_wr *bad_wr, *wr_list = NULL, *wr = NULL;
    mca_btl_openib_module_t *openib_btl = ep->endpoint_btl;
   
    if(0 == num_post)
        return OMPI_SUCCESS;

    for(i = 0; i < num_post; i++) {
        int rc;
        ompi_free_list_item_t* item;
        OMPI_FREE_LIST_WAIT(&openib_btl->qps[qp].recv_free, item, rc);
        to_base_frag(item)->base.order = qp;
        to_com_frag(item)->endpoint = ep;
        if(NULL == wr)
            wr = wr_list = &to_recv_frag(item)->rd_desc;
        else
            wr = wr->next = &to_recv_frag(item)->rd_desc;
    }

    wr->next = NULL;

    if(!ibv_post_recv(ep->qps[qp].qp->lcl_qp, wr_list, &bad_wr))
        return OMPI_SUCCESS;

    BTL_ERROR(("error posting receive on qp %d\n", qp));
    return OMPI_ERROR;
}

static inline int mca_btl_openib_endpoint_post_rr(
        mca_btl_base_endpoint_t *ep, const int qp)
{
    int rd_rsv = mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
    int rd_num = mca_btl_openib_component.qp_infos[qp].rd_num;
    int rd_low = mca_btl_openib_component.qp_infos[qp].rd_low;
    int cqp = mca_btl_openib_component.credits_qp, rc;
    int cm_received = 0, num_post = 0;
    
    assert(BTL_OPENIB_QP_TYPE_PP(qp));

    OPAL_THREAD_LOCK(&ep->endpoint_lock);

    if(ep->qps[qp].u.pp_qp.rd_posted <= rd_low)
        num_post = rd_num - ep->qps[qp].u.pp_qp.rd_posted;

    assert(num_post >= 0);

    if(ep->qps[qp].u.pp_qp.cm_received >= (rd_rsv >> 2))
        cm_received = ep->qps[qp].u.pp_qp.cm_received;

    if((rc = post_recvs(ep, qp, num_post)) != OMPI_SUCCESS) {
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return rc;
    }
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.rd_posted, num_post);
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.rd_credits, num_post);

    /* post buffers for credit management on credit management qp */
    if((rc = post_recvs(ep, cqp, cm_received)) != OMPI_SUCCESS) {
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return rc;
    }
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.cm_return, cm_received);
    OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.cm_received, -cm_received);

    assert(ep->qps[qp].u.pp_qp.rd_credits <= rd_num &&
            ep->qps[qp].u.pp_qp.rd_credits >= 0);

    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    return OMPI_SUCCESS;
}

#define BTL_OPENIB_CREDITS_SEND_TRYLOCK(E, Q) \
    OPAL_ATOMIC_CMPSET_32(&(E)->qps[(Q)].rd_credit_send_lock, 0, 1)
#define BTL_OPENIB_CREDITS_SEND_UNLOCK(E, Q) \
    OPAL_ATOMIC_CMPSET_32(&(E)->qps[(Q)].rd_credit_send_lock, 1, 0)

static inline bool check_eager_rdma_credits(const mca_btl_openib_endpoint_t *ep)
{
    return (ep->eager_rdma_local.credits > ep->eager_rdma_local.rd_win) ? true :
        false;
}

static inline bool
check_send_credits(const mca_btl_openib_endpoint_t *ep, const int qp)
{

    if(!BTL_OPENIB_QP_TYPE_PP(qp))
        return false;

    return (ep->qps[qp].u.pp_qp.rd_credits >=
            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_win) ? true : false;
}

static inline void send_credits(mca_btl_openib_endpoint_t *ep, int qp)
{
    if(BTL_OPENIB_QP_TYPE_PP(qp)) {
        if(check_send_credits(ep, qp))
            goto try_send;
    } else {
        qp = mca_btl_openib_component.credits_qp;
    }

    if(!check_eager_rdma_credits(ep))
        return;
    
try_send:
    if(BTL_OPENIB_CREDITS_SEND_TRYLOCK(ep, qp))
        mca_btl_openib_endpoint_send_credits(ep, qp);
}

static inline int check_endpoint_state(mca_btl_openib_endpoint_t *ep,
        mca_btl_base_descriptor_t *des, opal_list_t *pending_list)
{
    int rc = ORTE_ERR_RESOURCE_BUSY;

    switch(ep->endpoint_state) {
        case MCA_BTL_IB_CLOSED:
            rc = ompi_btl_openib_connect.bcf_start_connect(ep);
            if(rc == OMPI_SUCCESS)
                rc = ORTE_ERR_RESOURCE_BUSY;
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

END_C_DECLS

#endif

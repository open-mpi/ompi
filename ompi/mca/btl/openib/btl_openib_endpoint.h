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
#include "btl_openib_frag.h"
#include "btl_openib.h"
#include "btl_openib_eager_rdma.h"
#include <errno.h> 
#include <string.h> 
#include "ompi/mca/btl/base/btl_base_error.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

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


struct mca_btl_openib_endpoint_qp_t { 
    struct ibv_qp*              lcl_qp; /* Local QP (Low and High) */
    struct ibv_qp_attr*         lcl_qp_attr; 
    /* Local QP attrnibutes (Low and High) */
    uint32_t lcl_psn; 
    int32_t  sd_wqe;      /**< number of available send wqe entries */
    int qp_type;
    opal_list_t                 pending_frags; /**< put fragments here if there
                                                    is no wqe available or, in
                                                    case of PP QP, if there is
                                                    no credit available */
    int32_t  rd_pending_credit_chks;  /**< number of outstanding return credit requests */
    struct mca_btl_openib_frag_t *credit_frag;
    union {
        mca_btl_openib_endpoint_srq_qp_t srq_qp;
        mca_btl_openib_endpoint_pp_qp_t pp_qp;
    } u;
    
}; typedef struct mca_btl_openib_endpoint_qp_t mca_btl_openib_endpoint_qp_t;


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
    
    mca_btl_openib_endpoint_qp_t * qps;
       
    opal_list_t                 pending_get_frags; /**< list of pending rget ops */
    opal_list_t                 pending_put_frags; /**< list of pending rput ops */

    
    
    
    /* Local processes port sequence number (Low and High) */
 
    
    int32_t                     get_tokens;    /**< number of available get tokens */


    uint64_t subnet_id; /**< subnet id of this endpoint*/

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

int  mca_btl_openib_endpoint_send(mca_btl_base_endpoint_t* endpoint,
                                  struct mca_btl_openib_frag_t* frag);
int  mca_btl_openib_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_openib_post_recv(void);
void mca_btl_openib_endpoint_send_credits(mca_btl_base_endpoint_t*, const int);
void mca_btl_openib_endpoint_connect_eager_rdma(mca_btl_openib_endpoint_t*);



static inline int mca_btl_openib_endpoint_post_rr(mca_btl_base_endpoint_t *endpoint,
                                                  const int additional,
                                                  const int qp)
{
    mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;
    int rd_rsv = mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
    int rd_num = mca_btl_openib_component.qp_infos[qp].rd_num;
        
    int cm_received, rd_posted, rd_low;
    
    assert(MCA_BTL_OPENIB_PP_QP == endpoint->qps[qp].qp_type);
    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
    cm_received = endpoint->qps[qp].u.pp_qp.cm_received;
    rd_posted = endpoint->qps[qp].u.pp_qp.rd_posted;
    rd_low = mca_btl_openib_component.qp_infos[qp].rd_low;

    if(cm_received >= (rd_rsv >> 2) || rd_posted <= rd_low) {
        int rc;
        int32_t i, num_post = rd_num - rd_posted;
        struct ibv_recv_wr* bad_wr;
        ompi_free_list_t *free_list;
        
        free_list = &openib_btl->qps[qp].recv_free;

        for(i = 0; i < (num_post + cm_received); i++) {
           ompi_free_list_item_t* item;
           mca_btl_openib_frag_t* frag;
           OMPI_FREE_LIST_WAIT(free_list, item, rc);
           frag = (mca_btl_openib_frag_t*)item;
           frag->endpoint = endpoint;
           frag->base.order = qp;
           if(ibv_post_recv(endpoint->qps[qp].lcl_qp, 
                            &frag->wr_desc.rd_desc,
                            &bad_wr)) {
               BTL_ERROR(("error posting receive errno says %s\n",
                          strerror(errno)));
               OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
               return OMPI_ERROR;
           }
        }
        if(num_post > 0) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_posted, num_post);
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_credits, num_post);
        }
        if(cm_received > 0) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_return,
                    cm_received);
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_received,
                    -cm_received);
        }
        assert(endpoint->qps[qp].u.pp_qp.rd_credits <= rd_num);
        assert(endpoint->qps[qp].u.pp_qp.rd_credits >= 0);
    }
    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
    return OMPI_SUCCESS;
}

static inline int mca_btl_openib_endpoint_post_rr_all(mca_btl_base_endpoint_t *endpoint,
                                                      const int additional) 
{
    int qp;
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++){ 
        if(MCA_BTL_OPENIB_PP_QP == mca_btl_openib_component.qp_infos[qp].type) { 
            mca_btl_openib_endpoint_post_rr(endpoint, additional, qp);
        }
    }
    return OMPI_SUCCESS;
}

static inline int btl_openib_check_send_credits(
        mca_btl_openib_endpoint_t *endpoint, const int qp)
{
    if(BTL_OPENIB_EAGER_RDMA_QP(qp)) { 
        if(endpoint->eager_rdma_local.credits >= endpoint->eager_rdma_local.rd_win) {
            return OPAL_THREAD_ADD32(&endpoint->qps[qp].rd_pending_credit_chks, 1) == 1;
        }
    }

    if(MCA_BTL_OPENIB_PP_QP != mca_btl_openib_component.qp_infos[qp].type)
        return 0;

    if(endpoint->qps[qp].u.pp_qp.rd_credits >= 
       mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_win) { 
        return OPAL_THREAD_ADD32(&endpoint->qps[qp].rd_pending_credit_chks, 1) == 1;
    } 
   
    return 0;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

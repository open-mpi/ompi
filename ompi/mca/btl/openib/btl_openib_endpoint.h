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

struct mca_btl_openib_rem_info_t { 
    
    uint32_t                    rem_qp_num_hp;
    uint32_t                    rem_qp_num_lp; 
    /* Remote QP number  (Low and High priority) */ 

    uint16_t                    rem_lid;
    /* Local identifier of the remote process */
    
    
    uint32_t                    rem_psn_hp; 
    uint32_t                    rem_psn_lp; 
    /* Remote processes port sequence number (Low and High) */ 
   
    uint64_t                    rem_subnet_id; 
    /* subnet id of remote process */     

    /* MTU of remote process */
    uint32_t                    rem_mtu;

    /* index of remote endpoint in endpoint array */
    uint32_t                    rem_index;
}; 
typedef struct mca_btl_openib_rem_info_t mca_btl_openib_rem_info_t; 



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
    
    opal_list_t                 pending_send_frags;
    /**< list of pending send frags for this endpotint */
    
    opal_list_t                 pending_frags[2]; /**< list of pending frags */ 
    opal_list_t                 pending_get_frags; /**< list of pending rget ops */
    opal_list_t                 pending_put_frags; /**< list of pending rput ops */

    mca_btl_openib_rem_info_t   rem_info;
    
    uint32_t                    lcl_psn_hp; 
    uint32_t                    lcl_psn_lp; 
    /* Local processes port sequence number (Low and High) */
 
    struct ibv_qp*              lcl_qp[2]; /* Local QP (Low and High) */

    struct ibv_qp_attr*         lcl_qp_attr_hp; 
    struct ibv_qp_attr*         lcl_qp_attr_lp; 
    /* Local QP attributes (Low and High) */
    
    int32_t                     sd_tokens[2];  /**< number of send tokens */
    int32_t                     get_tokens;    /**< number of available get tokens */

    int32_t rd_posted[2];   /**< number of descriptors posted to the nic*/
    int32_t rd_credits[2];  /**< number of credits to return to peer */
    int32_t sd_credits[2];  /**< number of send wqe entries being used to return credits */
    int32_t sd_wqe[2];      /**< number of available send wqe entries */

    uint64_t subnet_id; /**< subnet id of this endpoint*/

    int32_t eager_recv_count; /**< number of eager received */
    mca_btl_openib_eager_rdma_remote_t eager_rdma_remote;
    /**< info about remote RDMA buffer */
    mca_btl_openib_eager_rdma_local_t eager_rdma_local;
    /**< info about local RDMA buffer */
    uint32_t index;           /**< index of the endpoint in endpoints array */
    struct mca_btl_openib_frag_t *credit_frag[2]; /**< frags for sending explicit high priority credits */
    bool nbo;       /**< does the endpoint require network byte ordering? */
    bool use_eager_rdma; /**< use eager rdma for this peer? */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_openib_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_openib_endpoint_t);

int  mca_btl_openib_endpoint_send(mca_btl_base_endpoint_t* endpoint, struct mca_btl_openib_frag_t* frag);
int  mca_btl_openib_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_openib_post_recv(void);
void mca_btl_openib_endpoint_send_credits(mca_btl_base_endpoint_t*, const int);
void mca_btl_openib_endpoint_connect_eager_rdma(mca_btl_openib_endpoint_t*);

static inline int btl_openib_endpoint_post_rr(mca_btl_base_endpoint_t *endpoint,
        const int additional, const int prio)
{
    mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;

    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
    if(endpoint->rd_posted[prio] <=
            mca_btl_openib_component.rd_low + additional &&
            endpoint->rd_posted[prio] < openib_btl->rd_num) {
        int rc;
        int32_t i, num_post = openib_btl->rd_num - endpoint->rd_posted[prio];
        struct ibv_recv_wr* bad_wr;
        ompi_free_list_t *free_list;

        if(BTL_OPENIB_HP_QP == prio)
            free_list = &openib_btl->recv_free_eager;
        else
            free_list = &openib_btl->recv_free_max;

        for(i = 0; i < num_post; i++) {
           ompi_free_list_item_t* item;
           mca_btl_openib_frag_t* frag;
           OMPI_FREE_LIST_WAIT(free_list, item, rc);
           frag = (mca_btl_openib_frag_t*)item;
           frag->endpoint = endpoint;
           if(ibv_post_recv(endpoint->lcl_qp[prio], &frag->wr_desc.rd_desc,
                       &bad_wr)) {
               BTL_ERROR(("error posting receive errno says %s\n",
                           strerror(errno)));
               OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
               return OMPI_ERROR;
           }
        }
        OPAL_THREAD_ADD32(&endpoint->rd_posted[prio], num_post);
        OPAL_THREAD_ADD32(&endpoint->rd_credits[prio], num_post);
     }
     OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
     return OMPI_SUCCESS;
}

static inline int btl_openib_check_send_credits(
        mca_btl_openib_endpoint_t *endpoint, const int prio)
{
    if(!mca_btl_openib_component.use_srq &&
            endpoint->rd_credits[prio] >= mca_btl_openib_component.rd_win)
        return OPAL_THREAD_ADD32(&endpoint->sd_credits[prio], 1) == 1;

    if(BTL_OPENIB_LP_QP == prio) /* nothing more for low prio QP */
        return 0;

    /* for high prio check eager RDMA credits */
    if(endpoint->eager_rdma_local.credits >= mca_btl_openib_component.rd_win)
        return OPAL_THREAD_ADD32(&endpoint->sd_credits[prio], 1) == 1;

    return 0;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

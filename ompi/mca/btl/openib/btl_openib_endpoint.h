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
#include "ompi/mca/mpool/openib/mpool_openib.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_btl_openib_frag_t;

struct mca_btl_openib_port_info_t {
    uint32_t mtu;
    uint16_t subnet; 
};
typedef struct mca_btl_openib_port_info_t mca_btl_openib_port_info_t;


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
   
    uint16_t                    rem_subnet; 
    /* subnet of remote process */     

    /* MTU of remote process */
    uint32_t                    rem_mtu;
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

    mca_btl_openib_rem_info_t   rem_info;
    
    uint32_t                    lcl_psn_hp; 
    uint32_t                    lcl_psn_lp; 
    /* Local processes port sequence number (Low and High) */
 
    struct ibv_qp*              lcl_qp_hp;
    struct ibv_qp*              lcl_qp_lp;
    /* Local QP (Low and High) */

    struct ibv_qp_attr*         lcl_qp_attr_hp; 
    struct ibv_qp_attr*         lcl_qp_attr_lp; 
    /* Local QP attributes (Low and High) */
    
    int32_t                     sd_tokens[2];  /**< number of send tokens */
    int32_t                     get_tokens;    /**< number of available get tokens */

    int32_t rd_posted_hp;   /**< number of high priority descriptors posted to the nic*/
    int32_t rd_posted_lp;   /**< number of low priority descriptors posted to the nic*/
    int32_t rd_credits_hp;  /**< number of high priority credits to return to peer */
    int32_t rd_credits_lp;  /**< number of low priority credits to return to peer */
    int32_t sd_credits_hp;  /**< number of send wqe entries being used to return credits */
    int32_t sd_credits_lp;  /**< number of send wqe entries being used to return credits */
    int32_t sd_wqe[2];      /**< number of available send wqe entries */

    uint16_t subnet; /**< subnet of this endpoint*/

    int32_t eager_recv_count; /**< number of eager received */
    mca_btl_openib_eager_rdma_remote_t eager_rdma_remote;
    /**< info about remote RDMA buffer */
    mca_btl_openib_eager_rdma_local_t eager_rdma_local;
    /**< info about local RDMA buffer */
    int32_t eager_rdma_index; /**< index into RDMA buffers pointer array */
    struct mca_btl_openib_frag_t *hp_credit_frag; /**< frag for sending explicit high priority credits */
    struct mca_btl_openib_frag_t *lp_credit_frag; /**< frag for sending explicit low priority credits */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_openib_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_openib_endpoint_t);

int  mca_btl_openib_endpoint_send(mca_btl_base_endpoint_t* endpoint, struct mca_btl_openib_frag_t* frag);
int  mca_btl_openib_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_openib_post_recv(void);
void mca_btl_openib_endpoint_send_credits_hp(mca_btl_base_endpoint_t*);
void mca_btl_openib_endpoint_send_credits_lp(mca_btl_base_endpoint_t*);
void mca_btl_openib_endpoint_connect_eager_rdma(mca_btl_openib_endpoint_t*);
    
#define MCA_BTL_OPENIB_ENDPOINT_POST_RR_HIGH(endpoint, \
                                             additional) \
{ \
   do { \
    mca_btl_openib_module_t * openib_btl = endpoint->endpoint_btl; \
    OPAL_THREAD_LOCK(&openib_btl->ib_lock); \
    if(endpoint->rd_posted_hp <= mca_btl_openib_component.rd_low+additional && \
       endpoint->rd_posted_hp < openib_btl->rd_num) { \
        MCA_BTL_OPENIB_ENDPOINT_POST_RR_SUB(openib_btl->rd_num -  \
                                            endpoint->rd_posted_hp, \
                                            endpoint, \
                                            &openib_btl->recv_free_eager, \
                                            endpoint->rd_posted_hp, \
                                            endpoint->rd_credits_hp, \
                                            endpoint->lcl_qp_hp); \
    } \
    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock); \
   } while(0); \
}

#define MCA_BTL_OPENIB_ENDPOINT_POST_RR_LOW(endpoint, \
                                            additional) { \
    do { \
    mca_btl_openib_module_t * openib_btl = endpoint->endpoint_btl; \
    OPAL_THREAD_LOCK(&openib_btl->ib_lock); \
    if(endpoint->rd_posted_lp <= mca_btl_openib_component.rd_low+additional && \
       endpoint->rd_posted_lp < openib_btl->rd_num){ \
       MCA_BTL_OPENIB_ENDPOINT_POST_RR_SUB(openib_btl->rd_num - \
                                            endpoint->rd_posted_lp,  \
                                            endpoint, \
                                            &openib_btl->recv_free_max, \
                                            endpoint->rd_posted_lp, \
                                            endpoint->rd_credits_lp, \
                                            endpoint->lcl_qp_lp \
                                            ); } \
    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock); \
    } while(0); \
}

#define MCA_BTL_OPENIB_ENDPOINT_POST_RR_SUB(cnt, \
                                            my_endpoint, \
                                            frag_list, \
                                            rd_posted, \
                                            rd_credits, \
                                            qp ) \
do { \
    int32_t i; \
    int rc; \
    int32_t num_post = cnt; \
    struct ibv_recv_wr* bad_wr; \
    for(i = 0; i < num_post; i++) { \
        ompi_free_list_item_t* item; \
        mca_btl_openib_frag_t* frag; \
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); \
        frag = (mca_btl_openib_frag_t*) item; \
        frag->endpoint = my_endpoint; \
        frag->sg_entry.length = frag->size + \
            ((unsigned char*) frag->segment.seg_addr.pval-  \
             (unsigned char*) frag->hdr);  \
        if(ibv_post_recv(qp, \
            &frag->wr_desc.rd_desc, \
            &bad_wr)) { \
            BTL_ERROR(("error posting receive errno says %s\n", strerror(errno))); \
            return OMPI_ERROR; \
        }\
    }\
    OPAL_THREAD_ADD32(&(rd_posted), num_post); \
    OPAL_THREAD_ADD32(&(rd_credits), num_post); \
} while(0); 

#define BTL_OPENIB_INSERT_PENDING(frag, frag_list, tokens, lock) \
do{ \
     OPAL_THREAD_LOCK(&lock); \
     opal_list_append(&frag_list, (opal_list_item_t *)frag); \
     OPAL_THREAD_UNLOCK(&lock); \
     OPAL_THREAD_ADD32(&tokens, 1); \
 } while(0); 


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

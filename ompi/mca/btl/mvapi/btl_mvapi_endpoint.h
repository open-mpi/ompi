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
 * Copyright (c) 2007      Cisco, Inc.  All Rights reserved.
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
#include "btl_mvapi_frag.h"
#include "btl_mvapi.h"
#include "btl_mvapi_eager_rdma.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"

#include <vapi.h>
#include <mtl_common.h>
#include <vapi_common.h>


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OBJ_CLASS_DECLARATION(mca_btl_mvapi_endpoint_t);

struct mca_btl_mvapi_frag_t; 
   
struct mca_btl_mvapi_port_info_t {
    uint32_t subnet; 
};
typedef struct mca_btl_mvapi_port_info_t mca_btl_mvapi_port_info_t;


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
} mca_btl_mvapi_endpoint_state_t;


struct mca_btl_mvapi_rem_info_t { 
    
    VAPI_qp_num_t               rem_qp_num_hp;
    /* High priority remote side QP number */

    VAPI_qp_num_t               rem_qp_num_lp; 
    /* Low prioirty remote size QP number */ 

    IB_lid_t                    rem_lid;
    /* Local identifier of the remote process */

    uint32_t                    rem_subnet; 
    /* subnet of remote process */ 
    
} ; 
typedef struct mca_btl_mvapi_rem_info_t mca_btl_mvapi_rem_info_t; 


/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_mvapi_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_mvapi_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    mca_btl_mvapi_endpoint_state_t     endpoint_state;
    /**< current state of the connection */

    size_t                      endpoint_retries;
    /**< number of connection retries attempted */

    double                      endpoint_tstamp;
    /**< timestamp of when the first connection was attempted */

    opal_mutex_t                endpoint_lock;
    /**< lock for concurrent access to endpoint state */
    
    opal_list_t                 pending_send_frags;
    /**< list of pending send frags for this endpoint */
 
    opal_list_t                 pending_frags_hp;  /**< list of pending high priority frags */ 
    opal_list_t                 pending_frags_lp;  /**< list of pending low priority frags */ 

    mca_btl_mvapi_rem_info_t    rem_info; 

    VAPI_qp_hndl_t              lcl_qp_hndl_hp; /* High priority local QP handle */
    VAPI_qp_hndl_t              lcl_qp_hndl_lp; /* Low priority local QP handle */

    VAPI_qp_prop_t              lcl_qp_prop_hp; /* High priority local QP properties */
    VAPI_qp_prop_t              lcl_qp_prop_lp; /* Low priority local QP properties */

    int32_t                     sd_tokens_hp;  /**< number of high priority send tokens */
    int32_t                     sd_tokens_lp;  /**< number of low priority send tokens */
    int32_t                     get_tokens;    /**< number of available get tokens */
    
    int32_t rd_posted_hp;   /**< number of high priority descriptors posted to the nic*/ 
    int32_t rd_posted_lp;   /**< number of low priority descriptors posted to the nic*/ 
    int32_t rd_credits_hp;  /**< number of high priority credits to return to peer */
    int32_t rd_credits_lp;  /**< number of low priority credits to return to peer */
    int32_t sd_credits_hp;  /**< number of send wqe entries being used to return credits */
    int32_t sd_credits_lp;  /**< number of send wqe entries being used to return credits */
    int32_t sd_wqe_hp;      /**< number of available high priority send wqe entries */
    int32_t sd_wqe_lp;      /**< number of available low priority send wqe entries */
    
    uint32_t subnet; 

    uint32_t eager_recv_count; /**< number of eager received */
    mca_btl_mvapi_eager_rdma_remote_t eager_rdma_remote;
    /**< info about remote RDMA buffer */
    mca_btl_mvapi_eager_rdma_local_t eager_rdma_local;
    /**< info about local RDMA buffer */
    int32_t eager_rdma_index; /**< index into RDMA buffers pointer array */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_mvapi_endpoint_t;
int  mca_btl_mvapi_endpoint_send(mca_btl_base_endpoint_t* endpoint, struct mca_btl_mvapi_frag_t* frag);
int  mca_btl_mvapi_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_mvapi_endpoint_send_credits_hp(mca_btl_base_endpoint_t*);
void mca_btl_mvapi_endpoint_send_credits_lp(mca_btl_base_endpoint_t*);
void mca_btl_mvapi_post_recv(void);
void mca_btl_mvapi_endpoint_connect_eager_rdma(mca_btl_mvapi_endpoint_t*);

#define MCA_BTL_MVAPI_ENDPOINT_POST_RR_HIGH(endpoint, \
                                             additional) \
{ \
  do { \
    mca_btl_mvapi_module_t * mvapi_btl = endpoint->endpoint_btl; \
    OPAL_THREAD_LOCK(&mvapi_btl->ib_lock); \
    if(endpoint->rd_posted_hp <= mca_btl_mvapi_component.rd_low+additional && \
       endpoint->rd_posted_hp < mvapi_btl->rd_num){ \
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_SUB(mvapi_btl->rd_num -  \
                                            endpoint->rd_posted_hp, \
                                            endpoint, \
                                            &mvapi_btl->recv_free_eager, \
                                            endpoint->rd_posted_hp, \
                                            endpoint->rd_credits_hp, \
                                            mvapi_btl->nic, \
                                            endpoint->lcl_qp_hndl_hp); \
    } \
    OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock); \
  } while(0); \
}

#define MCA_BTL_MVAPI_ENDPOINT_POST_RR_LOW(endpoint, \
                                             additional) \
{ \
   do { \
    mca_btl_mvapi_module_t * mvapi_btl = endpoint->endpoint_btl; \
    OPAL_THREAD_LOCK(&mvapi_btl->ib_lock); \
    if(endpoint->rd_posted_lp <= mca_btl_mvapi_component.rd_low+additional && \
       endpoint->rd_posted_lp < mvapi_btl->rd_num){ \
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_SUB(mvapi_btl->rd_num -  \
                                            endpoint->rd_posted_lp, \
                                            endpoint, \
                                            &mvapi_btl->recv_free_max, \
                                            endpoint->rd_posted_lp, \
                                            endpoint->rd_credits_lp, \
                                            mvapi_btl->nic, \
                                            endpoint->lcl_qp_hndl_lp); \
    } \
    OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock); \
  } while(0); \
}


#define MCA_BTL_MVAPI_ENDPOINT_POST_RR_SUB(cnt, \
                                            my_endpoint, \
                                            frag_list, \
                                            rd_posted, \
                                            rd_credits, \
                                            nic, \
                                            qp ) \
{  \
do { \
    int32_t i; \
    int rc; \
    int32_t num_post = cnt; \
    mca_btl_mvapi_module_t *mvapi_btl = my_endpoint->endpoint_btl; \
    VAPI_rr_desc_t* desc_post = mvapi_btl->rr_desc_post; \
    for(i = 0; i < num_post; i++) { \
        ompi_free_list_item_t* item; \
        mca_btl_mvapi_frag_t* frag = NULL; \
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); \
        frag = (mca_btl_mvapi_frag_t*) item; \
        frag->endpoint = my_endpoint; \
        frag->sg_entry.len = frag->size + \
            ((unsigned char*) frag->segment.seg_addr.pval-  \
             (unsigned char*) frag->hdr);  \
       desc_post[i] = frag->desc.rr_desc; \
    }\
    rc = EVAPI_post_rr_list( nic, \
                             qp, \
                             num_post, \
                             desc_post); \
   if(VAPI_OK != rc) { \
        BTL_ERROR(("error posting receive descriptors: %s",\
                   VAPI_strerror(rc))); \
    } else { \
        OPAL_THREAD_ADD32(&(rd_posted), num_post); \
        OPAL_THREAD_ADD32(&(rd_credits), num_post); \
   }\
  } while(0); \
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

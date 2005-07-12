/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "btl_openib_frag.h"
#include "btl_openib.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_btl_openib_endpoint_t);

/**
 * State of IB endpoint connection.
 */

typedef enum {
    /* Defines the state in which this BTL instance
     * has started the process of connection */
    MCA_BTL_IB_CONNECTING,

    /* Waiting for ack from endpoint */
    MCA_BTL_IB_CONNECT_ACK,

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

    opal_mutex_t                endpoint_send_lock;
    /**< lock for concurrent access to endpoint state */

    opal_mutex_t                endpoint_recv_lock;
    /**< lock for concurrent access to endpoint state */

    opal_list_t                 pending_send_frags;
    /**< list of pending send frags for this endpoint */
    
    uint32_t                    rem_qp_num_high;
    uint32_t                    rem_qp_num_low; 
    /* Remote QP number  (Low and High priority) */ 

    uint16_t                    rem_lid;
    /* Local identifier of the remote process */
    
    
    uint32_t                    rem_psn_high; 
    uint32_t                    rem_psn_low; 
    /* Remote processes port sequence number (Low and High) */ 
    
    uint32_t                    lcl_psn_high; 
    uint32_t                    lcl_psn_low; 
    /* Local processes port sequence number (Low and High) */
 
    struct ibv_qp*              lcl_qp_high;
    struct ibv_qp*              lcl_qp_low;
    /* Local QP (Low and High) */

    struct ibv_qp_attr*         lcl_qp_attr_high; 
    struct ibv_qp_attr*         lcl_qp_attr_low; 
    /* Local QP attributes (Low and High) */
    
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_openib_endpoint_t;

int  mca_btl_openib_endpoint_send(mca_btl_base_endpoint_t* endpoint, struct mca_btl_openib_frag_t* frag);
int  mca_btl_openib_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_openib_post_recv(void);


void mca_btl_openib_progress_send_frags(mca_btl_openib_endpoint_t*);

static inline int mca_btl_openib_endpoint_post_rr_sub(int cnt, 
                                                      mca_btl_openib_endpoint_t* endpoint, 
                                                      ompi_free_list_t* frag_list, 
                                                      uint32_t* rr_posted, 
                                                      struct ibv_qp*  qp
                                                      )
{
    
    int rc, i; 
    opal_list_item_t* item; 
    mca_btl_openib_frag_t* frag; 
    mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;
    struct ibv_recv_wr* bad_wr; 
    
    /* prepare frags and post receive requests, given, this is ugly, 
     * if openib doesn't plan on supporting a post_list method than 
     * this should be changed to simply loop through and post receives 
     * without bothering with the rr_desc_post array as it is not needed
     */
    for(i = 0; i < cnt; i++) {
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); 
        frag = (mca_btl_openib_frag_t*) item; 
        frag->endpoint = endpoint; 
        frag->sg_entry.length = frag->size + 
            ((unsigned char*) frag->segment.seg_addr.pval- 
             (unsigned char*) frag->hdr);  
        
        rr_desc_post[i] = frag->rr_desc; 
    }
    
    for(i=0; i< cnt; i++){ 
        
        if(ibv_post_recv(qp, 
                         rr_desc_post[i], 
                         &bad_wr)) { 
            opal_output(0, "%s: error posting receive\n", __func__); 
            return OMPI_ERROR; 
    }
    
    return OMPI_SUCCESS; 
    }
    OPAL_THREAD_ADD32(rr_posted, cnt); 
    return OMPI_SUCCESS; 
}

static inline int mca_btl_openib_endpoint_post_rr( mca_btl_openib_endpoint_t * endpoint, int additional){ 
    mca_btl_openib_module_t * openib_btl = endpoint->endpoint_btl; 
    int rc; 
    OPAL_THREAD_LOCK(&openib_btl->ib_lock); 

    if(openib_btl->rr_posted_high <= mca_btl_openib_component.ib_rr_buf_min+additional && openib_btl->rr_posted_high < mca_btl_openib_component.ib_rr_buf_max){ 
        
        rc = mca_btl_openib_endpoint_post_rr_sub(mca_btl_openib_component.ib_rr_buf_max - openib_btl->rr_posted_high, 
                                             endpoint, 
                                             &openib_btl->recv_free_eager, 
                                             &openib_btl->rr_posted_high, 
                                             openib_btl->nic, 
                                             endpoint->lcl_qp_hndl_high
                                             ); 
        if(rc != OMPI_SUCCESS){ 
            OPAL_THREAD_UNLOCK(&openib_btl->ib_lock); 
            return rc; 
        }
    }
    if(openib_btl->rr_posted_low <= mca_btl_openib_component.ib_rr_buf_min+additional && openib_btl->rr_posted_low < mca_btl_openib_component.ib_rr_buf_max){ 
        
        rc = mca_btl_openib_endpoint_post_rr_sub(mca_btl_openib_component.ib_rr_buf_max - openib_btl->rr_posted_low, 
                                             endpoint, 
                                             &openib_btl->recv_free_max, 
                                             &openib_btl->rr_posted_low, 
                                             openib_btl->nic, 
                                             endpoint->lcl_qp_hndl_low
                                             ); 
        if(rc != OMPI_SUCCESS) {
            OPAL_THREAD_UNLOCK(&openib_btl->ib_lock); 
            return rc; 
        }

    }
    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock); 
    return OMPI_SUCCESS; 
    
    
}

#define DUMP_ENDPOINT(endpoint_ptr) {                                       \
    opal_output(0, "[%s:%d] ", __FILE__, __LINE__);                 \
    opal_output(0, "Dumping endpoint %d state",                         \
            endpoint->endpoint_proc->proc_guid.vpid);                       \
    opal_output(0, "Local QP hndl : %d",                            \
            endpoint_ptr->endpoint_conn->lres->qp_hndl);                    \
    opal_output(0, "Local QP num : %d",                             \
            endpoint_ptr->endpoint_conn->lres->qp_prop.qp_num);             \
    opal_output(0, "Remote QP num : %d",                            \
            endpoint_ptr->endpoint_conn->rres->qp_num);                     \
    opal_output(0, "Remote LID : %d",                               \
            endpoint_ptr->endpoint_conn->rres->lid);                        \
}



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

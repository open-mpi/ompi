
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
#include <errno.h> 
#include <string.h> 
#include "mca/btl/base/btl_base_error.h"

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
    /**< list of pending send frags for this endpotint */
    
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

    uint32_t rr_posted_high;  /**< number of high priority rr posted to the nic*/ 
    uint32_t rr_posted_low;  /**< number of low priority rr posted to the nic*/ 
    

};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_openib_endpoint_t;

int  mca_btl_openib_endpoint_send(mca_btl_base_endpoint_t* endpoint, struct mca_btl_openib_frag_t* frag);
int  mca_btl_openib_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_openib_post_recv(void);


void mca_btl_openib_progress_send_frags(mca_btl_openib_endpoint_t*);

    
#define MCA_BTL_OPENIB_ENDPOINT_POST_RR_HIGH(post_rr_high_endpoint, \
                                             post_rr_high_additional) \
{ \
    mca_btl_openib_module_t * post_rr_high_openib_btl = post_rr_high_endpoint->endpoint_btl; \
    OPAL_THREAD_LOCK(&post_rr_high_openib_btl->ib_lock); \
    if(post_rr_high_endpoint->rr_posted_high <= mca_btl_openib_component.ib_rr_buf_min+post_rr_high_additional && \
       post_rr_high_endpoint->rr_posted_high < mca_btl_openib_component.ib_rr_buf_max){ \
        MCA_BTL_OPENIB_ENDPOINT_POST_RR_SUB(mca_btl_openib_component.ib_rr_buf_max -  \
                                            post_rr_high_endpoint->rr_posted_high, \
                                            post_rr_high_endpoint, \
                                            &post_rr_high_openib_btl->recv_free_eager, \
                                            &post_rr_high_endpoint->rr_posted_high, \
                                            post_rr_high_endpoint->lcl_qp_high); \
    } \
    OPAL_THREAD_UNLOCK(&post_rr_high_openib_btl->ib_lock); \
}

#define MCA_BTL_OPENIB_ENDPOINT_POST_RR_LOW(post_rr_low_endpoint, \
                                            post_rr_low_additional) { \
    mca_btl_openib_module_t * post_rr_low_openib_btl = post_rr_low_endpoint->endpoint_btl; \
    OPAL_THREAD_LOCK(&post_rr_low_openib_btl->ib_lock); \
    if(post_rr_low_endpoint->rr_posted_low <= mca_btl_openib_component.ib_rr_buf_min+post_rr_low_additional && \
       post_rr_low_endpoint->rr_posted_low < mca_btl_openib_component.ib_rr_buf_max){ \
       MCA_BTL_OPENIB_ENDPOINT_POST_RR_SUB(mca_btl_openib_component.ib_rr_buf_max - \
                                            post_rr_low_endpoint->rr_posted_low,  \
                                            post_rr_low_endpoint, \
                                            &post_rr_low_openib_btl->recv_free_max, \
                                            &post_rr_low_endpoint->rr_posted_low, \
                                            post_rr_low_endpoint->lcl_qp_low \
                                            ); } \
    OPAL_THREAD_UNLOCK(&post_rr_low_openib_btl->ib_lock); \
}

#define MCA_BTL_OPENIB_ENDPOINT_POST_RR_SUB(post_rr_sub_cnt, \
                                            post_rr_sub_endpoint, \
                                            post_rr_sub_frag_list, \
                                            post_rr_sub_rr_posted, \
                                            post_rr_sub_qp ) \
{\
    uint32_t post_rr_sub_i; \
    int post_rr_sub_rc; \
    opal_list_item_t* post_rr_sub_item; \
    mca_btl_openib_frag_t* post_rr_sub_frag; \
    struct ibv_recv_wr* post_rr_sub_bad_wr; \
    for(post_rr_sub_i = 0; post_rr_sub_i < post_rr_sub_cnt; post_rr_sub_i++) { \
        OMPI_FREE_LIST_WAIT(post_rr_sub_frag_list, post_rr_sub_item, post_rr_sub_rc); \
        post_rr_sub_frag = (mca_btl_openib_frag_t*) post_rr_sub_item; \
        post_rr_sub_frag->endpoint = post_rr_sub_endpoint; \
        post_rr_sub_frag->sg_entry.length = post_rr_sub_frag->size + \
            ((unsigned char*) post_rr_sub_frag->segment.seg_addr.pval-  \
             (unsigned char*) post_rr_sub_frag->hdr);  \
        if(ibv_post_recv(post_rr_sub_qp, \
            &post_rr_sub_frag->wr_desc.rr_desc, \
            &post_rr_sub_bad_wr)) { \
            BTL_ERROR(("error posting receive errno says %s\n", strerror(errno))); \
            return OMPI_ERROR; \
        }\
    }\
    OPAL_THREAD_ADD32(post_rr_sub_rr_posted, post_rr_sub_cnt); \
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

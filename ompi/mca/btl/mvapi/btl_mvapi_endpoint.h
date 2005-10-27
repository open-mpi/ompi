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
#include "btl_mvapi_frag.h"
#include "btl_mvapi.h"

#include <vapi.h>
#include <mtl_common.h>
#include <vapi_common.h>


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
#define MAX_POST_RR (16) 
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
    
    VAPI_qp_num_t               rem_qp_num_high;
    /* High priority remote side QP number */

    VAPI_qp_num_t               rem_qp_num_low; 
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
 
    opal_list_t                 pending_frags_hp; 
    /**< list of pending high priority frags */ 

    opal_list_t                 pending_frags_lp; 
    /**< list of pending low priority frags */ 

    int32_t                     wr_sq_tokens_hp; 
    /**< number of high priority frags that  can be outstanding (down counter) */ 

    int32_t                     wr_sq_tokens_lp; 
    /**< number of low priority frags that  can be outstanding (down counter) */ 
    
    mca_btl_mvapi_rem_info_t    rem_info; 

    VAPI_qp_hndl_t              lcl_qp_hndl_high;
    /* High priority local QP handle */
    
    VAPI_qp_hndl_t              lcl_qp_hndl_low;
    /* Low priority local QP handle */

    VAPI_qp_prop_t              lcl_qp_prop_high;
    /* High priority local QP properties */

    VAPI_qp_prop_t              lcl_qp_prop_low;
    /* Low priority local QP properties */

    
    uint32_t rr_posted_high;  /**< number of high priority rr posted to the nic*/ 
    uint32_t rr_posted_low;  /**< number of low priority rr posted to the nic*/ 

    uint32_t subnet; 
#if 0
    mca_btl_mvapi_rdma_buf_t   *rdma_buf;
#endif
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_mvapi_endpoint_t;
int  mca_btl_mvapi_endpoint_send(mca_btl_base_endpoint_t* endpoint, struct mca_btl_mvapi_frag_t* frag);
int  mca_btl_mvapi_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_mvapi_post_recv(void);


#define MCA_BTL_MVAPI_ENDPOINT_POST_RR_HIGH(endpoint, \
                                             additional) \
{ \
  do { \
    mca_btl_mvapi_module_t * mvapi_btl = endpoint->endpoint_btl; \
    OPAL_THREAD_LOCK(&mvapi_btl->ib_lock); \
    if(endpoint->rr_posted_high <= mca_btl_mvapi_component.ib_rr_buf_min+additional && \
       endpoint->rr_posted_high < mvapi_btl->rd_buf_max){ \
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_SUB(mvapi_btl->rd_buf_max -  \
                                            endpoint->rr_posted_high, \
                                            endpoint, \
                                            &mvapi_btl->recv_free_eager, \
                                            &endpoint->rr_posted_high, \
                                            mvapi_btl->nic, \
                                            endpoint->lcl_qp_hndl_high); \
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
    if(endpoint->rr_posted_low <= mca_btl_mvapi_component.ib_rr_buf_min+additional && \
       endpoint->rr_posted_low < mvapi_btl->rd_buf_max){ \
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_SUB(mvapi_btl->rd_buf_max -  \
                                            endpoint->rr_posted_low, \
                                            endpoint, \
                                            &mvapi_btl->recv_free_max, \
                                            &endpoint->rr_posted_low, \
                                            mvapi_btl->nic, \
                                            endpoint->lcl_qp_hndl_low); \
    } \
    OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock); \
  } while(0); \
}


#define MCA_BTL_MVAPI_ENDPOINT_POST_RR_SUB(cnt, \
                                            my_endpoint, \
                                            frag_list, \
                                            rr_posted, \
                                            nic, \
                                            qp ) \
{\
  do { \
    uint32_t i; \
    int rc; \
    opal_list_item_t* item; \
    mca_btl_mvapi_frag_t* frag = NULL; \
    mca_btl_mvapi_module_t *mvapi_btl = my_endpoint->endpoint_btl; \
    VAPI_rr_desc_t* desc_post = mvapi_btl->rr_desc_post; \
    for(i = 0; i < cnt; i++) { \
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); \
        frag = (mca_btl_mvapi_frag_t*) item; \
        frag->endpoint = my_endpoint; \
        frag->sg_entry.len = frag->size + \
            ((unsigned char*) frag->segment.seg_addr.pval-  \
             (unsigned char*) frag->hdr);  \
       desc_post[i] = frag->rr_desc; \
    }\
    frag->ret = EVAPI_post_rr_list( nic, \
                                                qp, \
                                                cnt, \
                                                desc_post); \
   if(NULL != frag && VAPI_OK != frag->ret) { \
        BTL_ERROR(("error posting receive descriptors: %s",\
                   VAPI_strerror(frag->ret))); \
    } else if (NULL != frag){\
        OPAL_THREAD_ADD32(rr_posted, cnt); \
   }\
  } while(0); \
}

#define BTL_MVAPI_INSERT_PENDING(frag, frag_list, tokens, lock, rc) \
{ \
 do{ \
     OPAL_THREAD_LOCK(&lock); \
     opal_list_append(&frag_list, (opal_list_item_t *)frag); \
     OPAL_THREAD_UNLOCK(&lock); \
     OPAL_THREAD_ADD32(&tokens, 1); \
     rc =  OMPI_SUCCESS; \
 } while(0); \
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

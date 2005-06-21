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

#ifndef MCA_BMI_IB_ENDPOINT_H
#define MCA_BMI_IB_ENDPOINT_H

#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "bmi_ib_frag.h"
#include "bmi_ib.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
#define MAX_POST_RR (16) 
OBJ_CLASS_DECLARATION(mca_bmi_ib_endpoint_t);

/**
 * State of IB endpoint connection.
 */

typedef enum {
    /* Defines the state in which this BMI instance
     * has started the process of connection */
    MCA_BMI_IB_CONNECTING,

    /* Waiting for ack from endpoint */
    MCA_BMI_IB_CONNECT_ACK,

    /* Connected ... both sender & receiver have
     * buffers associated with this connection */
    MCA_BMI_IB_CONNECTED,

    /* Connection is closed, there are no resources
     * associated with this */
    MCA_BMI_IB_CLOSED,

    /* Maximum number of retries have been used.
     * Report failure on send to upper layer */
    MCA_BMI_IB_FAILED
} mca_bmi_ib_endpoint_state_t;

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_bmi_base_endpoint_t is associated w/ each process
 * and BMI pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_bmi_base_endpoint_t {
    ompi_list_item_t            super;

    struct mca_bmi_ib_module_t* endpoint_bmi;
    /**< BMI instance that created this connection */

    struct mca_bmi_ib_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    mca_bmi_ib_endpoint_state_t     endpoint_state;
    /**< current state of the connection */

    size_t                      endpoint_retries;
    /**< number of connection retries attempted */

    double                      endpoint_tstamp;
    /**< timestamp of when the first connection was attempted */

    ompi_mutex_t                endpoint_send_lock;
    /**< lock for concurrent access to endpoint state */

    ompi_mutex_t                endpoint_recv_lock;
    /**< lock for concurrent access to endpoint state */

    ompi_list_t                 pending_send_frags;
    /**< list of pending send frags for this endpoint */

    VAPI_qp_num_t               rem_qp_num_high;
    /* High priority remote side QP number */

    VAPI_qp_num_t               rem_qp_num_low; 
    /* Low prioirty remote size QP number */ 

    IB_lid_t                    rem_lid;
    /* Local identifier of the remote process */

    VAPI_qp_hndl_t              lcl_qp_hndl_high;
    /* High priority local QP handle */
    
    VAPI_qp_hndl_t              lcl_qp_hndl_low;
    /* Low priority local QP handle */

    VAPI_qp_prop_t              lcl_qp_prop_high;
    /* High priority local QP properties */

    VAPI_qp_prop_t              lcl_qp_prop_low;
    /* Low priority local QP properties */

};

typedef struct mca_bmi_base_endpoint_t mca_bmi_base_endpoint_t;
typedef mca_bmi_base_endpoint_t  mca_bmi_ib_endpoint_t;

int  mca_bmi_ib_endpoint_send(mca_bmi_base_endpoint_t* endpoint, struct mca_bmi_ib_frag_t* frag);
int  mca_bmi_ib_endpoint_connect(mca_bmi_base_endpoint_t*);
void mca_bmi_ib_post_recv(void);


void mca_bmi_ib_progress_send_frags(mca_bmi_ib_endpoint_t*);

static inline int mca_bmi_ib_endpoint_post_rr_sub(int cnt, 
                                              mca_bmi_ib_endpoint_t* endpoint, 
                                              ompi_free_list_t* frag_list, 
                                              uint32_t* rr_posted, 
                                              VAPI_hca_hndl_t nic, 
                                              VAPI_qp_hndl_t qp_hndl
                                              )
{
    
    int rc;
    uint32_t i; 
    ompi_list_item_t* item; 
    mca_bmi_ib_frag_t* frag; 
    mca_bmi_ib_module_t *ib_bmi = endpoint->endpoint_bmi;
    VAPI_rr_desc_t* rr_desc_post = ib_bmi->rr_desc_post; 
    
    /* prepare frags and post receive requests */
    for(i = 0; i < cnt; i++) {
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); 
        frag = (mca_bmi_ib_frag_t*) item; 
        frag->endpoint = endpoint; 
        frag->sg_entry.len = frag->size + ((unsigned char*) frag->segment.seg_addr.pval- (unsigned char*) frag->hdr);  /* sizeof(mca_bmi_ib_header_t);     */ 
        rr_desc_post[i] = frag->rr_desc; 
        
    }
    
    frag->ret = EVAPI_post_rr_list(nic,
                                   qp_hndl,
                                   cnt, 
                                   rr_desc_post);
    if(VAPI_OK != frag->ret) {
        MCA_BMI_IB_VAPI_ERROR(frag->ret, "EVAPI_post_rr_list");
        return OMPI_ERROR; 
    }
    OMPI_THREAD_ADD32(rr_posted, cnt); 
    return OMPI_SUCCESS; 
}

static inline int mca_bmi_ib_endpoint_post_rr( mca_bmi_ib_endpoint_t * endpoint, int additional){ 
    mca_bmi_ib_module_t * ib_bmi = endpoint->endpoint_bmi; 
    int rc; 
    OMPI_THREAD_LOCK(&ib_bmi->ib_lock); 

    if(ib_bmi->rr_posted_high <= mca_bmi_ib_component.ib_rr_buf_min+additional && ib_bmi->rr_posted_high < mca_bmi_ib_component.ib_rr_buf_max){ 
        
        rc = mca_bmi_ib_endpoint_post_rr_sub(mca_bmi_ib_component.ib_rr_buf_max - ib_bmi->rr_posted_high, 
                                             endpoint, 
                                             &ib_bmi->recv_free_eager, 
                                             &ib_bmi->rr_posted_high, 
                                             ib_bmi->nic, 
                                             endpoint->lcl_qp_hndl_high
                                             ); 
        if(rc != OMPI_SUCCESS){ 
            OMPI_THREAD_UNLOCK(&ib_bmi->ib_lock); 
            return rc; 
        }
    }
    if(ib_bmi->rr_posted_low <= mca_bmi_ib_component.ib_rr_buf_min+additional && ib_bmi->rr_posted_low < mca_bmi_ib_component.ib_rr_buf_max){ 
        
        rc = mca_bmi_ib_endpoint_post_rr_sub(mca_bmi_ib_component.ib_rr_buf_max - ib_bmi->rr_posted_low, 
                                             endpoint, 
                                             &ib_bmi->recv_free_max, 
                                             &ib_bmi->rr_posted_low, 
                                             ib_bmi->nic, 
                                             endpoint->lcl_qp_hndl_low
                                             ); 
        if(rc != OMPI_SUCCESS) {
            OMPI_THREAD_UNLOCK(&ib_bmi->ib_lock); 
            return rc; 
        }

    }
    OMPI_THREAD_UNLOCK(&ib_bmi->ib_lock); 
    return OMPI_SUCCESS; 
    
    
}

#define DUMP_ENDPOINT(endpoint_ptr) {                                       \
    ompi_output(0, "[%s:%d] ", __FILE__, __LINE__);                 \
    ompi_output(0, "Dumping endpoint %d state",                         \
            endpoint->endpoint_proc->proc_guid.vpid);                       \
    ompi_output(0, "Local QP hndl : %d",                            \
            endpoint_ptr->endpoint_conn->lres->qp_hndl);                    \
    ompi_output(0, "Local QP num : %d",                             \
            endpoint_ptr->endpoint_conn->lres->qp_prop.qp_num);             \
    ompi_output(0, "Remote QP num : %d",                            \
            endpoint_ptr->endpoint_conn->rres->qp_num);                     \
    ompi_output(0, "Remote LID : %d",                               \
            endpoint_ptr->endpoint_conn->rres->lid);                        \
}



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

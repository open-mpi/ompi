/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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
#include "bmi_ib_priv.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
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

    VAPI_qp_num_t               rem_qp_num;
    /* Remote side QP number */

    IB_lid_t                    rem_lid;
    /* Local identifier of the remote process */

    VAPI_qp_hndl_t              lcl_qp_hndl;
    /* Local QP handle */

    VAPI_qp_prop_t              lcl_qp_prop;
    /* Local QP properties */
};

typedef struct mca_bmi_base_endpoint_t mca_bmi_base_endpoint_t;
typedef mca_bmi_base_endpoint_t  mca_bmi_ib_endpoint_t;

int  mca_bmi_ib_endpoint_send(struct mca_bmi_base_endpoint_t* endpoint, struct mca_bmi_ib_frag_t* frag);
int  mca_bmi_ib_endpoint_connect(mca_bmi_base_endpoint_t*);
void mca_bmi_ib_post_recv(void);


void mca_bmi_ib_progress_send_frags(mca_bmi_ib_endpoint_t*);

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

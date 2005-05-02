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

#ifndef MCA_PTL_IB_PEER_H
#define MCA_PTL_IB_PEER_H

#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_ib_recvfrag.h"
#include "ptl_ib_sendfrag.h"
#include "ptl_ib_priv.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_ptl_ib_peer_t);

/**
 * State of IB peer connection.
 */

typedef enum {
    /* Defines the state in which this PTL instance
     * has started the process of connection */
    MCA_PTL_IB_CONNECTING,

    /* Waiting for ack from peer */
    MCA_PTL_IB_CONNECT_ACK,

    /* Connected ... both sender & receiver have
     * buffers associated with this connection */
    MCA_PTL_IB_CONNECTED,

    /* Connection is closed, there are no resources
     * associated with this */
    MCA_PTL_IB_CLOSED,

    /* Maximum number of retries have been used.
     * Report failure on send to upper layer */
    MCA_PTL_IB_FAILED
} mca_ptl_ib_peer_state_t;

/**
 * An abstraction that represents a connection to a peer process.
 * An instance of mca_ptl_base_peer_t is associated w/ each process
 * and PTL pair at startup. However, connections to the peer
 * are established dynamically on an as-needed basis:
 */

struct mca_ptl_base_peer_t {
    ompi_list_item_t            super;

    struct mca_ptl_ib_module_t* peer_ptl;
    /**< PTL instance that created this connection */

    struct mca_ptl_ib_proc_t*   peer_proc;
    /**< proc structure corresponding to peer */

    mca_ptl_ib_peer_state_t     peer_state;
    /**< current state of the connection */

    size_t                      peer_retries;
    /**< number of connection retries attempted */

    double                      peer_tstamp;
    /**< timestamp of when the first connection was attempted */

    ompi_mutex_t                peer_send_lock;
    /**< lock for concurrent access to peer state */

    ompi_mutex_t                peer_recv_lock;
    /**< lock for concurrent access to peer state */

    ompi_list_t                 pending_send_frags;
    /**< list of pending send frags for this peer */

    VAPI_qp_num_t               rem_qp_num;
    /* Remote side QP number */

    IB_lid_t                    rem_lid;
    /* Local identifier of the remote process */

    VAPI_qp_hndl_t              lcl_qp_hndl;
    /* Local QP handle */

    VAPI_qp_prop_t              lcl_qp_prop;
    /* Local QP properties */

    ib_buffer_t                *lcl_recv;
    /* Remote resources associated with this connection */
};

typedef struct mca_ptl_base_peer_t mca_ptl_base_peer_t;
typedef struct mca_ptl_base_peer_t mca_ptl_ib_peer_t;

int  mca_ptl_ib_peer_send(mca_ptl_base_peer_t*, mca_ptl_ib_send_frag_t*);
int  mca_ptl_ib_peer_connect(mca_ptl_base_peer_t*);
void mca_ptl_ib_post_recv(void);

void mca_ptl_ib_progress_send_frags(mca_ptl_ib_peer_t*);

#define DUMP_PEER(peer_ptr) {                                       \
    ompi_output(0, "[%s:%d] ", __FILE__, __LINE__);                 \
    ompi_output(0, "Dumping peer %d state",                         \
            peer->peer_proc->proc_guid.vpid);                       \
    ompi_output(0, "Local QP hndl : %d",                            \
            peer_ptr->peer_conn->lres->qp_hndl);                    \
    ompi_output(0, "Local QP num : %d",                             \
            peer_ptr->peer_conn->lres->qp_prop.qp_num);             \
    ompi_output(0, "Remote QP num : %d",                            \
            peer_ptr->peer_conn->rres->qp_num);                     \
    ompi_output(0, "Remote LID : %d",                               \
            peer_ptr->peer_conn->rres->lid);                        \
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

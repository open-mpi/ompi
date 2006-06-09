/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
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
#include "btl_ud_frag.h"
#include "btl_ud.h"
#include <errno.h>
#include <string.h>
#include <infiniband/verbs.h>
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/openib/mpool_openib.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_btl_ud_endpoint_t);


struct mca_btl_ud_frag_t;

struct mca_btl_ud_port_info_t {
    uint16_t subnet;
};
typedef struct mca_btl_ud_port_info_t mca_btl_ud_port_info_t;


/**
 * State of IB endpoint connection.
 */

typedef enum {
    /* Defines the state in which this BTL instance
     * has started the process of connection */
    MCA_BTL_IB_CONNECTING,

    /* Connected ... both sender & receiver have
     * buffers associated with this connection */
    MCA_BTL_IB_CONNECTED,

    /* Connection is closed, there are no resources
     * associated with this */
    MCA_BTL_IB_CLOSED,

    /* Maximum number of retries have been used.
     * Report failure on send to upper layer */
    MCA_BTL_IB_FAILED
} mca_btl_ud_endpoint_state_t;

struct mca_btl_ud_rem_info_t {

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
};
typedef struct mca_btl_ud_rem_info_t mca_btl_ud_rem_info_t;



/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup.  Normally connections are established as-needed.
 * The UD BTL is connectionless, so no connection is ever established.
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_ud_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_ud_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    mca_btl_ud_endpoint_state_t     endpoint_state;
    /**< current state of the connection */

    opal_mutex_t                endpoint_lock;
    /**< lock for concurrent access to endpoint state */

    opal_list_t                 pending_send_frags;
    /**< list of pending send frags for this endpoint */

    mca_btl_ud_rem_info_t   rem_info;

    struct ibv_ah*              rmt_ah_hp;
    struct ibv_ah*              rmt_ah_lp;
    /* Local Address Handle (Low and High) */

    uint16_t subnet; /**< subnet of this endpoint*/
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_ud_endpoint_t;

int  mca_btl_ud_endpoint_send(mca_btl_base_endpoint_t* endpoint, struct mca_btl_ud_frag_t* frag);
inline int mca_btl_ud_endpoint_post_send(struct mca_btl_ud_module_t* ud_btl,
                                         mca_btl_ud_endpoint_t * endpoint,
                                         struct mca_btl_ud_frag_t * frag);
int  mca_btl_ud_endpoint_connect(mca_btl_base_endpoint_t*);
void mca_btl_ud_post_recv(void);
int mca_btl_ud_endpoint_init_qp(
                                mca_btl_base_module_t* btl,
                                struct ibv_cq* cq,
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                struct ibv_srq* srq,
#endif
                                struct ibv_qp** qp,
                                uint32_t lcl_psn);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

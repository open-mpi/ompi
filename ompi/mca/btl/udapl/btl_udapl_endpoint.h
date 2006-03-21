/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#ifndef MCA_BTL_UDAPL_ENDPOINT_H
#define MCA_BTL_UDAPL_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "btl_udapl_frag.h"
#include "btl_udapl.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Structure used to publish uDAPL id information to peers.
 */
struct mca_btl_udapl_addr_t {
    DAT_CONN_QUAL port;
    DAT_SOCK_ADDR addr;
};
typedef struct mca_btl_udapl_addr_t mca_btl_udapl_addr_t;


/**
 * State of uDAPL endpoint connection.
 */

typedef enum {
    MCA_BTL_UDAPL_CONNECTING,
    MCA_BTL_UDAPL_CONNECTED,
    MCA_BTL_UDAPL_CLOSED,
    MCA_BTL_UDAPL_FAILED
} mca_btl_udapl_endpoint_state_t;


/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
*/

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_udapl_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_udapl_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    mca_btl_udapl_endpoint_state_t endpoint_state;
    /**< current state of the endpoint connection */

    opal_list_t endpoint_frags;
    /**< pending send frags on this endpoint */

    opal_mutex_t endpoint_send_lock;
    /**< lock for concurrent access to endpoint state */

    opal_mutex_t endpoint_recv_lock;
    /**< lock for concurrent access to endpoint state */

    mca_btl_udapl_addr_t endpoint_addr;

    DAT_EP_HANDLE endpoint_ep;
    /**< uDAPL endpoint handle */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_udapl_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_udapl_endpoint_t);


int mca_btl_udapl_endpoint_send(mca_btl_base_endpoint_t* endpoint,
                                mca_btl_udapl_frag_t* frag);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

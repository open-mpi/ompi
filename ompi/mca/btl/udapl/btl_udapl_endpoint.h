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
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "btl_udapl_frag.h"
#include "btl_udapl.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Structure used to publish uDAPL id information to peers.
 */
struct mca_btl_udapl_addr_t {
    int foo;    /* placeholder to prevent division by 0 */
};
typedef struct mca_btl_udapl_addr_t mca_btl_udapl_addr_t;
                                                                                                                

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

    mca_btl_udapl_addr_t endpoint_addr;
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_udapl_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_udapl_endpoint_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

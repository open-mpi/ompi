/*
 * Copyright (c) 2008-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_SICORTEX_ENDPOINT_H
#define MCA_BTL_SICORTEX_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "btl_sicortex_frag.h"
#include "btl_sicortex.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_sicortex_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_sicortex_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    struct peer_struct_t*      route;
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_sicortex_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_sicortex_endpoint_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

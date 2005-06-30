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

#ifndef MCA_BTL_GM_ENDPOINT_H
#define MCA_BTL_GM_ENDPOINT_H

#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "btl_gm_frag.h"
#include "btl_gm.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Structure used to publish GM id information to peers.
 */
struct mca_btl_gm_addr_t {
#if GM_API_VERSION > 0x200
    unsigned int global_id;
#else
    char global_id[GM_MAX_HOST_NAME_LEN];
#endif  /* GM_API_VERSION > 0x200 */
    unsigned int local_id;
    unsigned int port_id;
};
typedef struct mca_btl_gm_addr_t mca_btl_gm_addr_t;
                                                                                                                

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    ompi_list_item_t            super;

    struct mca_btl_gm_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_gm_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    mca_btl_gm_addr_t endpoint_addr;
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_gm_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_gm_endpoint_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

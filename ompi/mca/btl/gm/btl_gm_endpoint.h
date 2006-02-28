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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_GM_ENDPOINT_H
#define MCA_BTL_GM_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "btl_gm_frag.h"
#include "btl_gm.h"

#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

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
    unsigned int node_id;
    unsigned int port_id;
};
typedef struct mca_btl_gm_addr_t mca_btl_gm_addr_t;

#if GM_API_VERSION > 0x200
#define MCA_BTL_GM_ADDR_HTON(addr) \
    addr.global_id = htonl(addr.global_id); \
    addr.node_id = htonl(addr.node_id); \
    addr.port_id = htonl(addr.port_id);

#define MCA_BTL_GM_ADDR_NTOH(addr) \
    addr.global_id = ntohl(addr.global_id); \
    addr.node_id = ntohl(addr.node_id); \
    addr.port_id = ntohl(addr.port_id);
#else
#define MCA_BTL_GM_ADDR_HTON(addr) \
    addr.node_id = htonl(addr.node_id); \
    addr.port_id = htonl(addr.port_id);

#define MCA_BTL_GM_ADDR_NTOH(addr) \
    addr.node_id = ntohl(addr.node_id); \
    addr.port_id = ntohl(addr.port_id);
#endif

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

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

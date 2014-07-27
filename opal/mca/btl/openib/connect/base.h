/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_OPENIB_CONNECT_BASE_H
#define BTL_OPENIB_CONNECT_BASE_H

#include "opal/mca/btl/openib/connect/connect.h"

#ifdef OPAL_HAVE_RDMAOE
#define BTL_OPENIB_CONNECT_BASE_CHECK_IF_NOT_IB(btl)                       \
        (((IBV_TRANSPORT_IB != ((btl)->device->ib_dev->transport_type)) || \
        (IBV_LINK_LAYER_ETHERNET == ((btl)->ib_port_attr.link_layer))) ?   \
        true : false)
#else
#define BTL_OPENIB_CONNECT_BASE_CHECK_IF_NOT_IB(btl)                       \
        ((IBV_TRANSPORT_IB != ((btl)->device->ib_dev->transport_type)) ?   \
        true : false)
#endif

BEGIN_C_DECLS

/*
 * Forward declaration to resolve circular dependency
 */
struct mca_btl_base_endpoint_t;

/*
 * Open function
 */
int opal_btl_openib_connect_base_register(void);

/*
 * Component-wide CPC init
 */
int opal_btl_openib_connect_base_init(void);

/*
 * Query CPCs to see if they want to run on a specific module
 */
int opal_btl_openib_connect_base_select_for_local_port
    (mca_btl_openib_module_t *btl);

/*
 * Forward reference to avoid an include file loop
 */
struct mca_btl_openib_proc_modex_t;

/*
 * Select function
 */
int opal_btl_openib_connect_base_find_match
    (mca_btl_openib_module_t *btl,
     struct mca_btl_openib_proc_modex_t *peer_port,
     opal_btl_openib_connect_base_module_t **local_cpc,
     opal_btl_openib_connect_base_module_data_t **remote_cpc_data);

/*
 * Find a CPC's index so that we can send it in the modex
 */
int opal_btl_openib_connect_base_get_cpc_index
    (opal_btl_openib_connect_base_component_t *cpc);

/*
 * Lookup a CPC by its index (received from the modex)
 */
opal_btl_openib_connect_base_component_t *
    opal_btl_openib_connect_base_get_cpc_byindex(uint8_t index);

/*
 * Allocate a CTS frag
 */
int opal_btl_openib_connect_base_alloc_cts(
        struct mca_btl_base_endpoint_t *endpoint);

/*
 * Free a CTS frag
 */
int opal_btl_openib_connect_base_free_cts(
        struct mca_btl_base_endpoint_t *endpoint);

/*
 * Start a new connection to an endpoint
 */
int opal_btl_openib_connect_base_start(
        opal_btl_openib_connect_base_module_t *cpc,
        struct mca_btl_base_endpoint_t *endpoint);


/*
 * Component-wide CPC finalize
 */
void opal_btl_openib_connect_base_finalize(void);

END_C_DECLS

#endif

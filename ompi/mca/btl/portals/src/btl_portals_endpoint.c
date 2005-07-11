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


#include "ompi_config.h"

#include "btl_portals.h"
#include "btl_portals_endpoint.h" 


/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_portals_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = NULL;
    endpoint->endpoint_proc = NULL;

    endpoint->endpoint_ptl_id.nid = 0;
    endpoint->endpoint_ptl_id.pid = 0;
}


OBJ_CLASS_INSTANCE(
    mca_btl_portals_endpoint_t, 
    opal_list_item_t,
    mca_btl_portals_endpoint_construct, 
    NULL);


/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/dss/dss.h"
#include "btl_elan.h"
#include "btl_elan_endpoint.h" 
#include "btl_elan_proc.h"
#include "btl_elan_frag.h"


/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_elan_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl      = NULL;
    endpoint->endpoint_proc     = NULL;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_elan_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
}


OBJ_CLASS_INSTANCE(mca_btl_elan_endpoint_t, 
                   opal_list_item_t, 
                   mca_btl_elan_endpoint_construct, 
                   mca_btl_elan_endpoint_destruct);


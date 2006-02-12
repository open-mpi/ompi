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


#include "ompi_config.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"
#include "btl_gm.h"
#include "btl_gm_endpoint.h" 
#include "btl_gm_proc.h"
#include "btl_gm_frag.h"


/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_gm_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_gm_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
}


OBJ_CLASS_INSTANCE(
    mca_btl_gm_endpoint_t, 
    opal_list_item_t, 
    mca_btl_gm_endpoint_construct, 
    mca_btl_gm_endpoint_destruct);


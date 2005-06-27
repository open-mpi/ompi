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
#include <sys/time.h>
#include <time.h>
#include "include/types.h"
#include "mca/ns/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/rml/rml.h"
#include "mca/errmgr/errmgr.h"
#include "dps/dps.h"
#include "bmi_template.h"
#include "bmi_template_endpoint.h" 
#include "bmi_template_proc.h"
#include "bmi_template_frag.h"


/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_bmi_template_endpoint_construct(mca_bmi_base_endpoint_t* endpoint)
{
    endpoint->endpoint_bmi = 0;
    endpoint->endpoint_proc = 0;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_bmi_template_endpoint_destruct(mca_bmi_base_endpoint_t* endpoint)
{
}


OBJ_CLASS_INSTANCE(
    mca_bmi_template_endpoint_t, 
    ompi_list_item_t, 
    mca_bmi_template_endpoint_construct, 
    mca_bmi_template_endpoint_destruct);


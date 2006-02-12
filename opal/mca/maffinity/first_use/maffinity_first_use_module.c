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

#include "opal_config.h"

#include <string.h>

#include "opal/constants.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "maffinity_first_use.h"


/*
 * Local functions
 */
static int first_use_module_init(void);
static int first_use_module_set(opal_maffinity_base_segment_t *segments,
                                size_t num_segments);

/*
 * First_Use maffinity module
 */
static const opal_maffinity_base_module_1_0_0_t module = {

    /* Initialization function */

    first_use_module_init,

    /* Module function pointers */

    first_use_module_set
};


const opal_maffinity_base_module_1_0_0_t *
opal_maffinity_first_use_component_query(int *query)
{
    int param;

    param = mca_base_param_find("maffinity", "first_use", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
}


static int first_use_module_init(void)
{
    /* Nothing to do */

    return OPAL_SUCCESS;
}


static int first_use_module_set(opal_maffinity_base_segment_t *segments,
                                size_t num_segments)
{
    size_t i;

    /* Crude: zero out all the segments that belong to me.  We could
       probably get away with touching a byte in each page (which
       would potentially be much faster), but this is during setup so
       it's not a huge deal.  Consider this a target for future
       optimization... */

    for (i = 0; i < num_segments; ++i) {
        memset(segments[i].mbs_start_addr, 0, segments[i].mbs_len);
    }

    return OPAL_SUCCESS;
}

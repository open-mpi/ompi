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
#include <numa.h>

#include "opal/constants.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "maffinity_libnuma.h"


/*
 * Local functions
 */
static int libnuma_module_init(void);
static int libnuma_module_set(opal_maffinity_base_segment_t *segments,
                              size_t num_segments);

/*
 * Libnuma maffinity module
 */
static const opal_maffinity_base_module_1_0_0_t module = {

    /* Initialization function */

    libnuma_module_init,

    /* Module function pointers */

    libnuma_module_set
};


const opal_maffinity_base_module_1_0_0_t *
opal_maffinity_libnuma_component_query(int *query)
{
    int param;

    if (-1 == numa_available()) {
        return NULL;
    }
    param = mca_base_param_find("maffinity", "libnuma", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
}


static int libnuma_module_init(void)
{
    /* Tell libnuma that we want all memory affinity to be local (but
       it's not an error if we can't -- prefer running in degraded
       mode to not running at all!). */

    numa_set_strict(0);
    numa_set_localalloc();

    return OPAL_SUCCESS;
}


static int libnuma_module_set(opal_maffinity_base_segment_t *segments,
                              size_t num_segments)
{
    size_t i;

    /* Kinda crummy that we have to allocate each portion individually
       rather than provide a top-level function call that does it all,
       but the libnuma() interface doesn't seem to allow that
       flexability -- they allow "interleaving", but not fine grained
       placement of pages. */

    for (i = 0; i < num_segments; ++i) {
        numa_setlocal_memory(segments[i].mbs_start_addr,
                             segments[i].mbs_len);
    }

    return OPAL_SUCCESS;
}

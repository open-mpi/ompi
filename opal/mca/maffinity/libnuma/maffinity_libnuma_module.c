/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <numa.h>
#include <numaif.h>

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
static int libnuma_module_node_name_to_id(char *, int *);
static int libnuma_modules_bind(opal_maffinity_base_segment_t *, size_t, int);

/*
 * Libnuma maffinity module
 */
static const opal_maffinity_base_module_1_0_0_t loc_module = {
    /* Initialization function */
    libnuma_module_init,

    /* Module function pointers */
    libnuma_module_set,
    libnuma_module_node_name_to_id,
    libnuma_modules_bind
};

int opal_maffinity_libnuma_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    if (-1 == numa_available()) {
        return OPAL_ERROR;
    }
    param = mca_base_param_find("maffinity", "libnuma", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


static int libnuma_module_init(void)
{
    struct bitmask * mask;

    /* If we have a strict policy set, then bind all memory to this
       numa node.  Note that maffinity won't be invoked unless the
       process is already bound to a specific processor, so we're
       guaranteed to get a good value back from
       numa_get_run_node_mask(). */
    if (MPOL_BIND == mca_maffinity_libnuma_component.libnuma_policy) {
        mask = numa_get_run_node_mask();
        numa_set_membind(mask);
    }

    /* We want libnuma to fail to alloc if it can't allocate on the
       specified node */
    numa_set_strict(1);

    return OPAL_SUCCESS;
}


static int libnuma_module_set(opal_maffinity_base_segment_t *segments,
                              size_t num_segments)
{
    size_t i;

    /* Explicitly set the memory binding policy for a set of
       segments */
    for (i = 0; i < num_segments; ++i) {
        numa_setlocal_memory(segments[i].mbs_start_addr,
                             segments[i].mbs_len);
    }

    return OPAL_SUCCESS;
}

static int libnuma_module_node_name_to_id(char *node_name, int *id)
{
    /* GLB: fix me */
    *id = atoi(node_name + 3);

    return OPAL_SUCCESS;
}

static int libnuma_modules_bind(opal_maffinity_base_segment_t *segs,
        size_t count, int node_id)
{
    size_t i;
    int rc;
    unsigned long node_mask = (1 << node_id);

    for(i = 0; i < count; i++) {
        rc = mbind(segs[i].mbs_start_addr, segs[i].mbs_len, 
                   mca_maffinity_libnuma_component.libnuma_policy,
                   &node_mask, sizeof(node_mask) * 8, 
#ifdef HAVE_MPOL_MF_MOVE
                   MPOL_MF_MOVE
#else
                   MPOL_MF_STRICT
#endif
                   );
        if (0 != rc) {
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

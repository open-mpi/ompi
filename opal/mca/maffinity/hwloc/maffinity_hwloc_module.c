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
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on Hwloc, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "maffinity_hwloc.h"
#include "opal/mca/common/hwloc/hwloc/include/hwloc.h"

/*
 * Local functions
 */
static int hwloc_module_init(void);
static int hwloc_module_set(opal_maffinity_base_segment_t *segments,
                              size_t num_segments);
static int hwloc_module_node_name_to_id(char *, int *);
static int hwloc_modules_bind(opal_maffinity_base_segment_t *, size_t, int);

/*
 * Hwloc maffinity module
 */
static const opal_maffinity_base_module_1_0_0_t local_module = {
    /* Initialization function */
    hwloc_module_init,

    /* Module function pointers */
    hwloc_module_set,
    hwloc_module_node_name_to_id,
    hwloc_modules_bind
};

int opal_maffinity_hwloc_component_query(mca_base_module_t **module, 
                                         int *priority)
{
    if (NULL == mca_maffinity_hwloc_component.topology) {
        return OPAL_ERROR;
    }

    *priority = mca_maffinity_hwloc_component.priority;
    *module = (mca_base_module_t *) &local_module;

    return OPAL_SUCCESS;
}


static int hwloc_module_init(void)
{
    int rc;
    hwloc_cpuset_t cpuset;

    /* Set the default memory binding policy to allocate locally */
    cpuset = hwloc_bitmap_alloc();
    hwloc_get_cpubind(mca_maffinity_hwloc_component.topology, cpuset, 0);
    rc = hwloc_set_membind(mca_maffinity_hwloc_component.topology, 
                           cpuset, HWLOC_MEMBIND_BIND, 
                           mca_maffinity_hwloc_component.bind_policy);
    hwloc_bitmap_free(cpuset);

    return (0 == rc) ? OPAL_SUCCESS : OPAL_ERROR;
}


static int hwloc_module_set(opal_maffinity_base_segment_t *segments,
                              size_t num_segments)
{
    size_t i;
    hwloc_cpuset_t cpuset;

    /* This module won't be used unless the process is already
       processor-bound.  So find out where we're processor bound, and
       bind our memory there, too. */
    cpuset = hwloc_bitmap_alloc();
    hwloc_get_cpubind(mca_maffinity_hwloc_component.topology, cpuset, 0);
    for (i = 0; i < num_segments; ++i) {
        if (0 != hwloc_set_area_membind(mca_maffinity_hwloc_component.topology, 
                                        segments[i].mbs_start_addr,
                                        segments[i].mbs_len, cpuset,
                                        HWLOC_MEMBIND_BIND, 
                                        HWLOC_MEMBIND_STRICT)) {
            hwloc_bitmap_free(cpuset);
            return OPAL_ERROR;
        }
    }

    hwloc_bitmap_free(cpuset);
    return OPAL_SUCCESS;
}

static int hwloc_module_node_name_to_id(char *node_name, int *id)
{
    /* GLB: fix me */
    *id = atoi(node_name + 3);

    return OPAL_SUCCESS;
}

static int hwloc_modules_bind(opal_maffinity_base_segment_t *segs,
                              size_t count, int node_id)
{
    size_t i;
    hwloc_cpuset_t cpuset;

    cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_set(cpuset, node_id);
    for(i = 0; i < count; i++) {
        if (0 != hwloc_set_area_membind(mca_maffinity_hwloc_component.topology, 
                                        segs[i].mbs_start_addr,
                                        segs[i].mbs_len, cpuset,
                                        HWLOC_MEMBIND_BIND, 
                                        HWLOC_MEMBIND_STRICT)) {
            hwloc_bitmap_free(cpuset);
            return OPAL_ERROR;
        }
    }

    hwloc_bitmap_free(cpuset);
    return OPAL_SUCCESS;
}

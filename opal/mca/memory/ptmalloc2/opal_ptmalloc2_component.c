/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#include "opal/constants.h"
#include "opal/mca/memory/memory.h"
#include "opal/memoryhooks/memory.h"

static int opal_memory_ptmalloc2_open(void);

const opal_memory_base_component_2_0_0_t mca_memory_ptmalloc2_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_MEMORY_BASE_VERSION_2_0_0,

        /* Component name and version */
        "ptmalloc2",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_memory_ptmalloc2_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};


static int
opal_memory_ptmalloc2_open(void)
{
    /* we always provide munmap support as part of libopen-pal.la.
       Will also provide malloc/free support if user linked us in.  In
       that case, we've already likely called set_support, so don't
       crush what has already been done */
    if (0 == opal_mem_hooks_support_level()) {
        opal_mem_hooks_set_support(OPAL_MEMORY_MUNMAP_SUPPORT);
    }
    return OPAL_SUCCESS;
}

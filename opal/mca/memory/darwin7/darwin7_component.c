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

#include "opal/memory/memory_internal.h"
#include "opal/mca/memory/memory.h"
#include "opal/include/constants.h"

static int
darwin7_open(void)
{
    /* there isn't a good "init hook" exposed in the way we did the
       Darwin 7 intercept code.  This should provde "good enough" - if
       we got this far, the linker hasn't killed anyone yet (Darwin
       has a mean, vengeful linker), so the rest of the code is most
       likely also along for the ride */
    opal_mem_free_set_free_support(1);

    return OPAL_SUCCESS;
}


const opal_memory_base_component_1_0_0_t mca_memory_darwin7_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a memory v1.0.0 component (which also
           implies a specific MCA version) */
        OPAL_MEMORY_BASE_VERSION_1_0_0,

        /* Component name and version */
        "darwin7",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        darwin7_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        true
    },
};

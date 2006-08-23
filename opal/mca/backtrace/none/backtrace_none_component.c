/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/backtrace/backtrace.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
    OPAL_DECLSPEC extern const opal_backtrace_base_component_1_0_0_t mca_backtrace_none_component;
#endif
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

const opal_backtrace_base_component_1_0_0_t mca_backtrace_none_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a backtrace v1.0.0 component (which also
           implies a specific MCA version) */
        OPAL_BACKTRACE_BASE_VERSION_1_0_0,

        /* Component name and version */
        "none",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        true
    },
};

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

/* NOTE: This framework is really for build system only.  There is no
   set of function pointers that must be called or set interface or
   any of that.  The only two functions that a component must call
   (note: call, not implement) are defined in
   opal/memoryhooks/memory_internal.h.  Other than that, to each his
   own.. 

   Components should make some attempt to provide a component struct
   via the usual means, just so ompi_info has something rational to
   display.
*/

#ifndef OPAL_MCA_MEMORY_MEMORY_H
#define OPAL_MCA_MEMORY_MEMORY_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

/**
 * Structure for memory v1.0.0 components.
 * Chained to MCA v1.0.0
 */
struct opal_memory_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t memoryc_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t memoryc_data;
};
/**
 * Convenience typedef
 */
typedef struct opal_memory_base_component_1_0_0_t opal_memory_base_component_1_0_0_t;

/*
 * Macro for use in components that are of type memory v1.0.0
 */
#define OPAL_MEMORY_BASE_VERSION_1_0_0 \
    /* memory v1.0 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* memory v1.0 */ \
    "memory", 1, 0, 0

#endif /* OPAL_MCA_MEMORY_MEMORY_H */

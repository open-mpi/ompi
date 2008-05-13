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
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Filter output to format it for specific purposes
 * such as XML or just do a no-op!
 */

#ifndef MCA_FILTER_H
#define MCA_FILTER_H

#include "opal_config.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_object.h"

BEGIN_C_DECLS


/**
 * Module initialization function.
 * Returns OPAL_SUCCESS
 */
typedef int (*opal_filter_base_module_init_fn_t)(void);

/**
 * Module finalization function.
 * Returns OPAL_SUCCESS
 */
typedef int (*opal_filter_base_module_finalize_fn_t)(void);

/**
 * Process a string through the selected filter - returns
 * filtered string that can be free'd by caller, or returns
 * NULL if no filtering was done
 */
typedef char* (*opal_filter_base_module_process_fn_t)(char *str, int major_id, int minor_id, int num_tags, char **tags);


/**
 * Structure for FILTER v1.0.0 components.
 */
struct opal_filter_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t base_data;
};
typedef struct opal_filter_base_component_1_0_0_t opal_filter_base_component_1_0_0_t;
typedef struct opal_filter_base_component_1_0_0_t opal_filter_base_component_t;

/**
 * Structure for FILTER v1.0.0 modules
 */
struct opal_filter_base_module_1_0_0_t {
    /** Initialization Function */
    opal_filter_base_module_init_fn_t       init;
    /** Finalization Function */
    opal_filter_base_module_finalize_fn_t   finalize;

    /** Filter processing interface */
    opal_filter_base_module_process_fn_t    process;

};
typedef struct opal_filter_base_module_1_0_0_t opal_filter_base_module_1_0_0_t;
typedef struct opal_filter_base_module_1_0_0_t opal_filter_base_module_t;

OPAL_DECLSPEC extern opal_filter_base_module_t opal_filter;

/**
 * Macro for use in components that are of type FILTER v1.0.0
 */
#define OPAL_FILTER_BASE_VERSION_1_0_0 \
    /* FILTER v1.0 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* FILTER v1.0 */ \
    "filter", 1, 0, 0

END_C_DECLS

#endif /* OPAL_FILTER_H */


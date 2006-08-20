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
 *
 */

#ifndef OPAL_PAFFINITY_BASE_INTERNAL_H
#define OPAL_PAFFINITY_BASE_INTERNAL_H

#include "opal_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/paffinity/paffinity.h"


/*
 * Global functions for MCA overall paffinity open and close
 */
/** @file */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Indication of whether a component was successfully selected or
     * not
     */
    OPAL_DECLSPEC extern bool opal_paffinity_base_selected;

    /**
     * Global component struct for the selected component
     */
    OPAL_DECLSPEC extern const opal_paffinity_base_component_1_0_0_t 
        *opal_paffinity_base_component;
    /**
     * Global module struct for the selected module
     */
    OPAL_DECLSPEC extern const opal_paffinity_base_module_1_0_0_t 
        *opal_paffinity_base_module;

    /**
     * Indicator as to whether the list of opened paffinity components
     * is valid or not.
     */
    OPAL_DECLSPEC extern bool opal_paffinity_base_components_opened_valid;
    /**
     * List of all opened components; created when the paffinity
     * framework is initialized and destroyed when we reduce the list
     * to all available paffinity components.
     */
    OPAL_DECLSPEC extern opal_list_t opal_paffinity_base_components_opened;

    /**
     * Debugging output stream
     */
    OPAL_DECLSPEC extern int opal_paffinity_base_output;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OPAL_PAFFINITY_BASE_INTERNAL_H */

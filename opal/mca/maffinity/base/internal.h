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
 *
 */

#ifndef OPAL_MAFFINITY_BASE_INTERNAL_H
#define OPAL_MAFFINITY_BASE_INTERNAL_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "mca/maffinity/maffinity.h"


/** 
 * @file 
 *
 * Global functions for MCA overall maffinity open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Indication of whether a component was successfully selected or
     * not
     */
    OMPI_DECLSPEC extern bool opal_maffinity_base_selected;

    /**
     * Global component struct for the selected component
     */
    OMPI_DECLSPEC extern const opal_maffinity_base_component_1_0_0_t 
        *opal_maffinity_base_component;
    /**
     * Global module struct for the selected module
     */
    OMPI_DECLSPEC extern const opal_maffinity_base_module_1_0_0_t 
        *opal_maffinity_base_module;

    /**
     * Indicator as to whether the list of opened maffinity components
     * is valid or not.
     */
    OMPI_DECLSPEC extern bool opal_maffinity_base_components_opened_valid;
    /**
     * List of all opened components; created when the maffinity
     * framework is initialized and destroyed when we reduce the list
     * to all available maffinity components.
     */
    OMPI_DECLSPEC extern opal_list_t opal_maffinity_base_components_opened;

    /**
     * Debugging output stream
     */
    OMPI_DECLSPEC extern int opal_maffinity_base_output;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OPAL_MAFFINITY_BASE_INTERNAL_H */

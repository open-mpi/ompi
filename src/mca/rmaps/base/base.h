/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 * rmaps framework base functionality.
 */

#ifndef ORTE_MCA_RMAPS_BASE_H
#define ORTE_MCA_RMAPS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "include/orte_constants.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/ns/ns_types.h"

#include "mca/rmaps/rmaps.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


    /**
     * Struct to hold data global to the rmaps framework
     */
    typedef struct orte_rmaps_base_t {
        /** Verbose/debug output stream */
        int rmaps_output;
        /** List of opened components */
        ompi_list_t rmaps_opened;
        /** Sorted list of available components (highest priority first) */
        ompi_list_t rmaps_available;
    } orte_rmaps_base_t;

    /**
     * Global instance of rmaps-wide framework data
     */
    OMPI_DECLSPEC extern orte_rmaps_base_t orte_rmaps_base;

    /**
     * RMAPS component/module/priority tuple
     */
    struct orte_rmaps_base_cmp_t {
        /** Base object */
        ompi_list_item_t super;
        /** rmaps component */
        orte_rmaps_base_component_t *component;
        /** rmaps module */
        orte_rmaps_base_module_t* module;
        /** This component's priority */
        int priority;
    };
    /** Convenience typedef */
    typedef struct orte_rmaps_base_cmp_t orte_rmaps_base_cmp_t;
    /** Class declaration */
    OMPI_DECLSPEC OBJ_CLASS_DECLARATION(orte_rmaps_base_cmp_t);


    /**
     * Open the rmaps framework
     */
    OMPI_DECLSPEC int orte_rmaps_base_open(void);

    /**
     * Select an rmaps component / module
     */
    OMPI_DECLSPEC orte_rmaps_base_module_t *orte_rmaps_base_select(char *preferred);

    /**
     * Utility routines to get/set vpid mapping for the job
     */ 
                                                                      
    OMPI_DECLSPEC int orte_rmaps_base_get_vpid_range(orte_jobid_t jobid, 
        orte_vpid_t *start, orte_vpid_t *range);
    OMPI_DECLSPEC int orte_rmaps_base_set_vpid_range(orte_jobid_t jobid, 
        orte_vpid_t start, orte_vpid_t range);

    /**
     * Close down the rmaps framework
     */
    OMPI_DECLSPEC int orte_rmaps_base_close(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

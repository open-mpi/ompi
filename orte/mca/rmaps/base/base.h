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
/** @file:
 * rmaps framework base functionality.
 */

#ifndef ORTE_MCA_RMAPS_BASE_H
#define ORTE_MCA_RMAPS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"

#include "orte/mca/rmaps/rmaps.h"


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
        /** Whether or not the NO_OP module is in use */
        bool no_op_selected;
        /** List of opened components */
        opal_list_t rmaps_opened;
        /** Sorted list of available components (highest priority first) */
        opal_list_t rmaps_available;
        /* user specified mapping policy? */
        bool user_specified_policy;
        /* map by node or not */
        bool bynode;
        /** whether or not we allow oversubscription of nodes */
        bool oversubscribe;
        /** do we want one ppn if num_procs not specified */
        bool per_node;
        /** number of ppn for n_per_node mode */
        int n_per_node;
        /* do we not allow use of the localhost */
        bool no_use_local;
        /* display the map after it is computed */
        bool display_map;
    } orte_rmaps_base_t;
    ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_rmaps_base_t);
    /**
     * Global instance of rmaps-wide framework data
     */
    ORTE_DECLSPEC extern orte_rmaps_base_t orte_rmaps_base;

    /**
     * Open the rmaps framework
     */
    ORTE_DECLSPEC int orte_rmaps_base_open(void);

    /**
     * Select an rmaps component / module
     */
    ORTE_DECLSPEC int orte_rmaps_base_find_available(void);

    /**
     * Utility routines to get/set vpid mapping for the job
     */ 
                                                                      
    ORTE_DECLSPEC int orte_rmaps_base_get_vpid_range(orte_jobid_t jobid, 
        orte_vpid_t *start, orte_vpid_t *range);
    ORTE_DECLSPEC int orte_rmaps_base_set_vpid_range(orte_jobid_t jobid, 
        orte_vpid_t start, orte_vpid_t range);

    /**
     * Close down the rmaps framework
     */
    ORTE_DECLSPEC int orte_rmaps_base_finalize(void);
    ORTE_DECLSPEC int orte_rmaps_base_close(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

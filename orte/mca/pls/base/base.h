/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 */

#ifndef MCA_PLS_BASE_H
#define MCA_PLS_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"
#include "opal/threads/condition.h"

#include "orte/mca/pls/pls.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Struct to hold data global to the pls framework
     */
    typedef struct orte_pls_base_t {
        /** Verbose/debug output stream */
        int pls_output;
        /** List of opened components */
        opal_list_t available_components;
        /** indicate a component has been selected */
        bool selected;
        /** selected component */
        orte_pls_base_component_t selected_component;
        /* orted cmd comm lock */
        opal_mutex_t orted_cmd_lock;
        /* orted cmd cond */
        opal_condition_t orted_cmd_cond;
        /** reuse daemons flag */
        bool reuse_daemons;
        /** request for timing measurement reports */
        bool timing;
    } orte_pls_base_t;
    
    /**
     * Global instance of pls-wide framework data
     */
    ORTE_DECLSPEC extern orte_pls_base_t orte_pls_base;

    /*
     * Global functions for MCA overall collective open and close
     */

    /**
     * Open the pls framework
     */
    ORTE_DECLSPEC int orte_pls_base_open(void);
    /**
     * Select a pls module
     */
    ORTE_DECLSPEC int orte_pls_base_select(void);

    /**
     * Close the pls framework
     */
    ORTE_DECLSPEC int orte_pls_base_finalize(void);
    ORTE_DECLSPEC int orte_pls_base_close(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

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
 */

#ifndef MCA_PLS_BASE_H
#define MCA_PLS_BASE_H

/*
 * includes
 */
#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/pls/pls.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Struct to hold data globale to the pls framework
     */
    typedef struct orte_pls_base_t {
        /** Verbose/debug output stream */
        int pls_output;
        /** List of opened components */
        ompi_list_t pls_opened;
        /** Sorted list of available components (highest priority first) */
        ompi_list_t pls_available;
    } orte_pls_base_t;
    
    /**
     * Global instance of pls-wide framework data
     */
    OMPI_DECLSPEC extern orte_pls_base_t orte_pls_base;

    /**
     * pls component/module/priority tuple
     */
    struct orte_pls_base_cmp_t {
        /** Base object */
        ompi_list_item_t super;
        /** pls component */
        orte_pls_base_component_t *component;
        /** pls module */
        orte_pls_base_module_t* module;
        /** This component's priority */
        int priority;
    };
    /** Convenience typedef */
    typedef struct orte_pls_base_cmp_t orte_pls_base_cmp_t;
    /** Class declaration */
    OMPI_DECLSPEC OBJ_CLASS_DECLARATION(orte_pls_base_cmp_t);

    /*
     * Global functions for MCA overall collective open and close
     */

    /**
     * Open the pls framework
     */
    OMPI_DECLSPEC int orte_pls_base_open(void);
    /**
     * Select a pls module
     */
    OMPI_DECLSPEC orte_pls_base_module_t *orte_pls_base_select(char *preferred);
    /**
     * Close the pls framework
     */
    OMPI_DECLSPEC int orte_pls_base_close(void);
    /**
     * Utility routine to get/set proces pid
     */
    OMPI_DECLSPEC int orte_pls_base_set_proc_pid(const orte_process_name_t*, pid_t);
    OMPI_DECLSPEC int orte_pls_base_get_proc_pid(const orte_process_name_t*, pid_t*);
    /**
     * Utility routine to retreive all process pids w/in a specified job.
     */
    OMPI_DECLSPEC int orte_pls_base_get_proc_pids(orte_jobid_t jobid, pid_t** pids, size_t* num_pids);
    /**
     * Utility routine to get/set daemon pid
     */
    OMPI_DECLSPEC int orte_pls_base_set_node_pid(orte_cellid_t cellid, char* node_name, orte_jobid_t jobid, pid_t pid);
    OMPI_DECLSPEC int orte_pls_base_get_node_pids(orte_jobid_t jobid, pid_t** pids, size_t* num_pids);
  
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

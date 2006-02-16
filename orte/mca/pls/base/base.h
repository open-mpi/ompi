/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
 */

#ifndef MCA_PLS_BASE_H
#define MCA_PLS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "opal/mca/mca.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/ras/base/ras_base_node.h"


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
        opal_list_t pls_opened;
        /** Whether the list of opened components is valid */
        bool pls_opened_valid;
        /** Sorted list of available components (highest priority first) */
        opal_list_t pls_available;
        /** Whether the list of available components is valid */
        bool pls_available_valid;
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
        opal_list_item_t super;
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
    OMPI_DECLSPEC int orte_pls_base_finalize(void);
    OMPI_DECLSPEC int orte_pls_base_close(void);
    /**
     * Utility routine to get/set procesS pid
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
  
    /**
     * Utility routine to set progress engine schedule
     */
    OMPI_DECLSPEC int orte_pls_base_set_progress_sched(int sched);


    /**
     * Utilities for pls components that use proxy daemons
     */
    int orte_pls_base_proxy_set_node_name(orte_ras_node_t* node, 
                                          orte_jobid_t jobid, 
                                          orte_process_name_t* name);
    int orte_pls_base_proxy_mca_argv(int *argc, char ***argv);
    int orte_pls_base_proxy_terminate_job(orte_jobid_t jobid);
    int orte_pls_base_proxy_terminate_proc(const orte_process_name_t *proc);

    /**
     * Check that the cwd in an app context exists and is accessible.
     * If the user specified the cwd and we can chdir to it, print an
     * error and fail.  If the user didn't specify it (i.e., it's a
     * default), then see if chdir($HOME) would succeed.
     *
     * If either chdir() would succeed and do_chdir is true, then
     * actually do the chdir().
     *
     * If we fall back to the chdir($HOME), set context->cwd to be a
     * string pointing to the home directory name (owned by the
     * context; safe to free at destruction).
     */
    int orte_pls_base_check_context_cwd(orte_app_context_t *context,
                                        bool do_chdir);

    /**
     * Check that the app exists and is executable.  If it is not,
     * print and error and fail.  If it is, and if the app was a naked
     * executable (i.e., no relative or absolute path), replace the
     * app with the string containing the absolute pathname to the
     * exectuable (owned by the context; safe to free at destruction).
     */
    int orte_pls_base_check_context_app(orte_app_context_t *context);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

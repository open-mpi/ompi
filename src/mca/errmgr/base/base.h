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

#ifndef ORTE_MCA_ERRMGR_BASE_H
#define ORTE_MCA_ERRMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "include/orte_constants.h"

#include "class/ompi_list.h"

#include "mca/mca.h"
#include "mca/ns/ns_types.h"
#include "mca/errmgr/errmgr.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Internal definitions
 */
/*
 * function definitions
 */
OMPI_DECLSPEC    int orte_errmgr_base_open(void);
OMPI_DECLSPEC    int orte_errmgr_base_select(bool *allow_multi_user_threads,
			                                bool *have_hidden_threads);
OMPI_DECLSPEC    int orte_errmgr_base_close(void);

    /*
     * Base functions that are common to all implementations - can be overridden
     */

OMPI_DECLSPEC    void orte_errmgr_base_log(int error_code, char *filename, int line);

OMPI_DECLSPEC    void orte_errmgr_base_proc_aborted(orte_process_name_t *proc);

OMPI_DECLSPEC    void orte_errmgr_base_incomplete_start(orte_jobid_t job);

OMPI_DECLSPEC    void orte_errmgr_base_error_detected(int error_code);

OMPI_DECLSPEC    int orte_errmgr_base_register_job(orte_jobid_t job);

OMPI_DECLSPEC    void orte_errmgr_base_abort(void);

/*
 * globals that might be needed
 */

OMPI_DECLSPEC extern int orte_errmgr_base_output;
OMPI_DECLSPEC extern bool orte_errmgr_base_selected;
OMPI_DECLSPEC extern bool orte_errmgr_initialized;
OMPI_DECLSPEC extern ompi_list_t orte_errmgr_base_components_available;
OMPI_DECLSPEC extern mca_errmgr_base_component_t orte_errmgr_base_selected_component;

/*
 * external API functions will be documented in the mca/errmgr/errmgr.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

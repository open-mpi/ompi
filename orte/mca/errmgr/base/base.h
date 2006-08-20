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
 */

#ifndef ORTE_MCA_ERRMGR_BASE_H
#define ORTE_MCA_ERRMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"

#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/errmgr/errmgr.h"


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
ORTE_DECLSPEC    int orte_errmgr_base_open(void);
ORTE_DECLSPEC    int orte_errmgr_base_select(bool *allow_multi_user_threads,
			                                bool *have_hidden_threads);
ORTE_DECLSPEC    int orte_errmgr_base_close(void);

    /*
     * Base functions that are common to all implementations - can be overridden
     */

ORTE_DECLSPEC    void orte_errmgr_base_log(int error_code, char *filename, int line);

ORTE_DECLSPEC    void orte_errmgr_base_proc_aborted(orte_process_name_t *proc);

ORTE_DECLSPEC    void orte_errmgr_base_incomplete_start(orte_jobid_t job);

ORTE_DECLSPEC    void orte_errmgr_base_error_detected(int error_code);

ORTE_DECLSPEC    int orte_errmgr_base_register_job(orte_jobid_t job);

ORTE_DECLSPEC    void orte_errmgr_base_abort(void);

/*
 * globals that might be needed
 */

ORTE_DECLSPEC extern int orte_errmgr_base_output;
ORTE_DECLSPEC extern bool orte_errmgr_base_selected;
ORTE_DECLSPEC extern bool orte_errmgr_initialized;
ORTE_DECLSPEC extern opal_list_t orte_errmgr_base_components_available;
ORTE_DECLSPEC extern mca_errmgr_base_component_t orte_errmgr_base_selected_component;

/*
 * external API functions will be documented in the mca/errmgr/errmgr.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

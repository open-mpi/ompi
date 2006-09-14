/* -*- C -*-
 *
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
#ifndef ORTE_ERRMGR_HNP_H
#define ORTE_ERRMGR_HNP_H


#include "orte_config.h"
#include "orte/orte_types.h"

#include "orte/mca/ns/ns_types.h"

#include "orte/mca/errmgr/errmgr.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_errmgr_hnp_open(void);
int orte_errmgr_hnp_close(void);


/*
 * Startup / Shutdown
 */
orte_errmgr_base_module_t*
orte_errmgr_hnp_component_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);

int orte_errmgr_hnp_finalize(void);

/*
 * globals used within the component
 */
typedef struct {
    int debug;
} orte_errmgr_hnp_globals_t;


extern orte_errmgr_hnp_globals_t orte_errmgr_hnp_globals;

/*
 * Component API functions
 */
int orte_errmgr_hnp_proc_aborted(orte_gpr_notify_message_t *msg);

int orte_errmgr_hnp_incomplete_start(orte_gpr_notify_message_t *msg);

void orte_errmgr_hnp_error_detected(int error_code, char *fmt, ...);

void orte_errmgr_hnp_abort(void);

int orte_errmgr_hnp_register_job(orte_jobid_t job);

int orte_errmgr_hnp_abort_procs_request(orte_process_name_t *procs, orte_std_cntr_t nprocs);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

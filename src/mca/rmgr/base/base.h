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

#ifndef ORTE_RMGR_BASE_H
#define ORTE_RMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/gpr/gpr_types.h"
#include "mca/rmgr/rmgr.h"


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
OMPI_DECLSPEC int orte_rmgr_base_open(void);
OMPI_DECLSPEC int orte_rmgr_base_select(void);
OMPI_DECLSPEC int orte_rmgr_base_close(void);

OMPI_DECLSPEC int orte_rmgr_base_get_app_context(
    orte_jobid_t jobid,
    orte_app_context_t*** app_context,
    size_t* num_context);

OMPI_DECLSPEC int orte_rmgr_base_put_app_context(
    orte_jobid_t jobid,
    orte_app_context_t** app_context,
    size_t num_context);

OMPI_DECLSPEC int orte_rmgr_base_get_job_slots(
    orte_jobid_t jobid, 
    size_t* num_slots);

OMPI_DECLSPEC int orte_rmgr_base_set_job_slots(
    orte_jobid_t jobid, 
    size_t num_slots);

/*
 * Base functions that are common to all implementations - can be overridden
 */
int orte_rmgr_base_create_not_available(
    orte_app_context_t** app_context, 
    size_t num_context, 
    orte_jobid_t* jobid);
int orte_rmgr_base_query_not_available(void);
int orte_rmgr_base_allocate_not_available(orte_jobid_t);
int orte_rmgr_base_deallocate_not_available(orte_jobid_t);
int orte_rmgr_base_map_not_available(orte_jobid_t);
int orte_rmgr_base_launch_not_available(orte_jobid_t);
int orte_rmgr_base_terminate_job_not_available(orte_jobid_t);
int orte_rmgr_base_terminate_proc_not_available(const orte_process_name_t*);
int orte_rmgr_base_proc_stage_gate_init(orte_jobid_t job);
int orte_rmgr_base_proc_stage_gate_subscribe(orte_jobid_t job, orte_gpr_notify_cb_fn_t, void*);
void orte_rmgr_base_proc_stage_gate_mgr(
        orte_gpr_notify_data_t *data,
        void *user_tag);
void orte_rmgr_base_proc_stage_gate_mgr_abort(
        orte_gpr_notify_data_t *data,
        void *user_tag);
int orte_rmgr_base_spawn_not_available(
    orte_app_context_t** app_context, 
    size_t num_context, 
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfn);
int orte_rmgr_base_finalize_not_available(void);

/*
 * globals that might be needed
 */

typedef struct orte_rmgr_base_t {
    int rmgr_output;
    ompi_list_t rmgr_components;
} orte_rmgr_base_t;

OMPI_DECLSPEC extern orte_rmgr_base_t orte_rmgr_base;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

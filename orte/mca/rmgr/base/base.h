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

#ifndef ORTE_RMGR_BASE_H
#define ORTE_RMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/class/opal_list.h"
#include "orte/dss/dss.h"
#include "opal/mca/mca.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rmgr/rmgr.h"


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
    orte_std_cntr_t* num_context);

OMPI_DECLSPEC int orte_rmgr_base_put_app_context(
    orte_jobid_t jobid,
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context);

OMPI_DECLSPEC int orte_rmgr_base_get_job_slots(
    orte_jobid_t jobid,
    orte_std_cntr_t* num_slots);

OMPI_DECLSPEC int orte_rmgr_base_set_job_slots(
    orte_jobid_t jobid,
    orte_std_cntr_t num_slots);


/*
 *  Pack/unpack
 */

OMPI_DECLSPEC int orte_rmgr_base_pack_cmd(
    orte_buffer_t* buffer,
    orte_rmgr_cmd_t cmd,
    orte_jobid_t jobid);

OMPI_DECLSPEC int orte_rmgr_base_pack_create_cmd(
    orte_buffer_t* buffer,
    orte_app_context_t** context,
    orte_std_cntr_t num_context);

OMPI_DECLSPEC int orte_rmgr_base_pack_terminate_proc_cmd(
    orte_buffer_t* buffer,
    const orte_process_name_t* name);

OMPI_DECLSPEC int orte_rmgr_base_pack_signal_job_cmd(
    orte_buffer_t* buffer,
    orte_jobid_t job,
    int32_t signal);

OMPI_DECLSPEC int orte_rmgr_base_pack_signal_proc_cmd(
    orte_buffer_t* buffer,
    const orte_process_name_t* name,
    int32_t signal);

OMPI_DECLSPEC int orte_rmgr_base_unpack_rsp(
    orte_buffer_t* buffer);

OMPI_DECLSPEC int orte_rmgr_base_unpack_create_rsp(
    orte_buffer_t* buffer,
    orte_jobid_t*);

OMPI_DECLSPEC int orte_rmgr_base_cmd_dispatch(
    orte_buffer_t* req,
    orte_buffer_t* rsp);

/*
 * Base functions that are common to all implementations - can be overridden
 */
int orte_rmgr_base_create_not_available(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid);
int orte_rmgr_base_query_not_available(void);
int orte_rmgr_base_allocate_not_available(orte_jobid_t);
int orte_rmgr_base_deallocate_not_available(orte_jobid_t);
int orte_rmgr_base_map_not_available(orte_jobid_t);
int orte_rmgr_base_launch_not_available(orte_jobid_t);
int orte_rmgr_base_terminate_job_not_available(orte_jobid_t);
int orte_rmgr_base_terminate_proc_not_available(const orte_process_name_t*);
int orte_rmgr_base_signal_job_not_available(orte_jobid_t, int32_t);
int orte_rmgr_base_signal_proc_not_available(const orte_process_name_t*, int32_t);
int orte_rmgr_base_proc_stage_gate_init(orte_jobid_t job);
int orte_rmgr_base_proc_stage_gate_subscribe(orte_jobid_t job, orte_gpr_notify_cb_fn_t, void*, orte_proc_state_t);
int orte_rmgr_base_proc_stage_gate_mgr(
        orte_gpr_notify_message_t *msg);
int orte_rmgr_base_proc_stage_gate_mgr_abort(
        orte_gpr_notify_message_t *msg);
int orte_rmgr_base_spawn_not_available(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions);
int orte_rmgr_base_finalize_not_available(void);

/*
 * DATA TYPE PACKING FUNCTIONS
 */
int orte_rmgr_base_pack_app_context(orte_buffer_t *buffer, void *src,
                                    orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_rmgr_base_pack_app_context_map(orte_buffer_t *buffer, void *src,
                                        orte_std_cntr_t num_vals, orte_data_type_t type);

/*
 * DATA TYPE UNPACKING FUNCTIONS
 */
int orte_rmgr_base_unpack_app_context(orte_buffer_t *buffer, void *dest,
                                      orte_std_cntr_t *num_vals, orte_data_type_t type);

int orte_rmgr_base_unpack_app_context_map(orte_buffer_t *buffer, void *dest,
                                          orte_std_cntr_t *num_vals, orte_data_type_t type);


/*
 * COMPARE FUNCTIONS
 */
int orte_rmgr_base_compare_app_context(orte_app_context_t *value1, orte_app_context_t *value2, orte_data_type_t type);

int orte_rmgr_base_compare_app_context_map(orte_app_context_map_t *value1, orte_app_context_map_t *value2, orte_data_type_t type);

/*
 * COPY FUNCTIONS
 */
int orte_rmgr_base_copy_app_context(orte_app_context_t **dest, orte_app_context_t *src, orte_data_type_t type);

int orte_rmgr_base_copy_app_context_map(orte_app_context_map_t **dest, orte_app_context_map_t *src, orte_data_type_t type);

/*
 * PRINT FUNCTIONS
 */
int orte_rmgr_base_print_app_context(char **output, char *prefix, orte_app_context_t *src, orte_data_type_t type);

int orte_rmgr_base_print_app_context_map(char **output, char *prefix, orte_app_context_map_t *src, orte_data_type_t type);

/*
 * SIZE FUNCTIONS
 */
int orte_rmgr_base_size_app_context(size_t *size, orte_app_context_t *src, orte_data_type_t type);

int orte_rmgr_base_size_app_context_map(size_t *size, orte_app_context_map_t *src, orte_data_type_t type);

/*
 * RELEASE FUNCTIONS
 */
void orte_rmgr_base_std_obj_release(orte_data_value_t *value);

/*
 * globals that might be needed
 */

typedef struct orte_rmgr_base_t {
    int rmgr_output;
    opal_list_t rmgr_components;
} orte_rmgr_base_t;

OMPI_DECLSPEC extern orte_rmgr_base_t orte_rmgr_base;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

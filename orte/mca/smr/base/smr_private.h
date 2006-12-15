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

#ifndef MCA_SMR_PRIVATE_H
#define MCA_SMR_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/smr/base/base.h"


/*
 * private functions for use inside SMR components
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Define an object for internally tracking node states
 */
typedef struct {
    opal_list_item_t super;
    orte_cellid_t cell;
    char *nodename;
    orte_node_state_t state;
} orte_smr_node_state_tracker_t;
OBJ_CLASS_DECLARATION(orte_smr_node_state_tracker_t);
    
int orte_smr_base_get_proc_state(orte_proc_state_t *state,
                               int *status,
                               orte_process_name_t *proc);

int orte_smr_base_set_proc_state(orte_process_name_t *proc,
                               orte_proc_state_t state,
                               int status);

int orte_smr_base_get_node_state(orte_node_state_t *state,
                                 orte_cellid_t cell,
                                 char *nodename);

int orte_smr_base_set_node_state(orte_cellid_t cell,
                                 char *nodename,
                                 orte_node_state_t state);

int orte_smr_base_get_job_state(orte_job_state_t *state,
                              orte_jobid_t jobid);

int orte_smr_base_set_job_state(orte_jobid_t jobid,
                              orte_job_state_t state);

int orte_smr_base_init_job_stage_gates(orte_jobid_t job,
                                       orte_gpr_trigger_cb_fn_t cbfunc,
                                       void *user_tag);

int orte_smr_base_init_orted_stage_gates(orte_jobid_t job,
                                         orte_std_cntr_t num_orteds,
                                         orte_gpr_trigger_cb_fn_t cbfunc,
                                         void *user_tag);
    
int orte_smr_base_define_alert_monitor(orte_jobid_t job,
                                        char *trigger_name,
                                        char *counter_key,
                                        orte_std_cntr_t init_value,
                                        orte_std_cntr_t alert_value,
                                        bool one_shot,
                                        orte_gpr_trigger_cb_fn_t cbfunc,
                                        void *user_tag);

int orte_smr_base_job_stage_gate_subscribe(orte_jobid_t job,
                                           orte_gpr_notify_cb_fn_t cbfunc, void* cbdata,
                                           orte_proc_state_t cb_conditions);

int orte_smr_base_begin_monitoring_not_available(orte_job_map_t *map,
                                                 orte_gpr_trigger_cb_fn_t cbfunc,
                                                 void *user_tag);


int orte_smr_base_module_finalize_not_available (void);

/*
 * DATA TYPE PACKING FUNCTIONS
 */
int orte_smr_base_pack_exit_code(orte_buffer_t *buffer, void *src,
                                 orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_smr_base_pack_node_state(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_smr_base_pack_proc_state(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_smr_base_pack_job_state(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type);

/*
 * DATA TYPE UNPACKING FUNCTIONS
 */
int orte_smr_base_unpack_exit_code(orte_buffer_t *buffer, void *dest,
                                 orte_std_cntr_t *num_vals, orte_data_type_t type);

int orte_smr_base_unpack_node_state(orte_buffer_t *buffer, void *dest,
                                  orte_std_cntr_t *num_vals, orte_data_type_t type);

int orte_smr_base_unpack_proc_state(orte_buffer_t *buffer, void *dest,
                                  orte_std_cntr_t *num_vals, orte_data_type_t type);

int orte_smr_base_unpack_job_state(orte_buffer_t *buffer, void *dest,
                                  orte_std_cntr_t *num_vals, orte_data_type_t type);

/*
 * DATA TYPE COMPARE FUNCTIONS
 */
int orte_smr_base_compare_exit_code(orte_exit_code_t *value1,
                                    orte_exit_code_t *value2,
                                    orte_data_type_t type);

int orte_smr_base_compare_node_state(orte_node_state_t *value1,
                                     orte_node_state_t *value2,
                                     orte_node_state_t type);

int orte_smr_base_compare_proc_state(orte_proc_state_t *value1,
                                     orte_proc_state_t *value2,
                                     orte_proc_state_t type);

int orte_smr_base_compare_job_state(orte_job_state_t *value1,
                                    orte_job_state_t *value2,
                                    orte_job_state_t type);

/*
 * DATA TYPE COPY FUNCTIONS
 */
int orte_smr_base_copy_proc_state(orte_proc_state_t **dest, orte_proc_state_t *src, orte_data_type_t type);

int orte_smr_base_copy_job_state(orte_job_state_t **dest, orte_job_state_t *src, orte_data_type_t type);

int orte_smr_base_copy_node_state(orte_node_state_t **dest, orte_node_state_t *src, orte_data_type_t type);

int orte_smr_base_copy_exit_code(orte_exit_code_t **dest, orte_exit_code_t *src, orte_data_type_t type);

/*
 * DATA TYPE PRINT FUNCTIONS
 */
int orte_smr_base_std_print(char **output, char *prefix, void *src, orte_data_type_t type);

/*
 * DATA TYPE SIZE FUNCTIONS
 */
int orte_smr_base_std_size(size_t *size, void *src, orte_data_type_t type);

/*
 * DATA TYPE RELEASE FUNCTIONS
 */
void orte_smr_base_std_release(orte_data_value_t *value);

/*
 * globals that might be needed within the framework
 */

ORTE_DECLSPEC extern int orte_smr_base_output;
ORTE_DECLSPEC extern bool orte_smr_base_selected;


/*
 * external API functions will be documented in the mca/smr/smr.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

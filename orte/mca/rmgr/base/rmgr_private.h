/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#ifndef ORTE_RMGR_PRIVATE_H
#define ORTE_RMGR_PRIVATE_H

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
#include "orte/mca/rml/rml_types.h"

#include "orte/mca/rmgr/rmgr.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Constants for command values
 */
#define ORTE_RMGR_SETUP_JOB_CMD      1
#define ORTE_RMGR_SPAWN_JOB_CMD      2
#define ORTE_RMGR_SETUP_GATES_CMD	 3
    
#define ORTE_RMGR_CMD  ORTE_UINT8
typedef uint8_t orte_rmgr_cmd_t;
    
/*
 * Base functions that are common to all implementations - can be overridden
 */
ORTE_DECLSPEC int orte_rmgr_base_get_app_context(
    orte_jobid_t jobid,
    orte_app_context_t*** app_context,
    orte_std_cntr_t* num_context);

ORTE_DECLSPEC int orte_rmgr_base_put_app_context(
    orte_jobid_t jobid,
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context);

ORTE_DECLSPEC int orte_rmgr_base_get_job_slots(
    orte_jobid_t jobid,
    orte_std_cntr_t* num_slots);

ORTE_DECLSPEC int orte_rmgr_base_set_job_slots(
    orte_jobid_t jobid,
    orte_std_cntr_t num_slots);

ORTE_DECLSPEC int orte_rmgr_base_check_context_app(orte_app_context_t *context);

ORTE_DECLSPEC int orte_rmgr_base_check_context_cwd(orte_app_context_t *context,
                                                   bool want_chdir);

ORTE_DECLSPEC int orte_rmgr_base_set_vpid_range(orte_jobid_t jobid, orte_vpid_t start, orte_vpid_t range);

ORTE_DECLSPEC int orte_rmgr_base_get_vpid_range(orte_jobid_t jobid, orte_vpid_t *start, orte_vpid_t *range);

ORTE_DECLSPEC int orte_rmgr_base_connect(orte_std_cntr_t num_connect,
                                         orte_process_name_t *connect);

ORTE_DECLSPEC int orte_rmgr_base_disconnect(orte_std_cntr_t num_disconnect,
                                            orte_process_name_t *disconnect);

ORTE_DECLSPEC orte_gpr_keyval_t* orte_rmgr_base_find_attribute(opal_list_t* attr_list, char* key);

ORTE_DECLSPEC int orte_rmgr_base_add_attribute(opal_list_t* attr_list, char* key,
                                               orte_data_type_t type, void *data,
                                               bool overwrite);

ORTE_DECLSPEC int orte_rmgr_base_merge_attributes(opal_list_t* target, opal_list_t* source, bool override);

ORTE_DECLSPEC int orte_rmgr_base_delete_attribute(opal_list_t* attr_list, char* key);


/*
 * Internal definitions
 */

int orte_rmgr_base_setup_job_not_available(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid, opal_list_t *attrs);

int orte_rmgr_base_spawn_not_available(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_std_cntr_t num_connect,
    orte_process_name_t *connect,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions,
    opal_list_t *attributes);

int orte_rmgr_base_finalize_not_available(void);

/*
 * Support functions
 */
ORTE_DECLSPEC void orte_rmgr_base_purge_mca_params(char ***env);

ORTE_DECLSPEC int orte_rmgr_base_proc_stage_gate_init(orte_jobid_t job);

ORTE_DECLSPEC int orte_rmgr_base_proc_stage_gate_mgr(orte_gpr_notify_message_t *msg);

ORTE_DECLSPEC int orte_rmgr_base_comm_start(void);

ORTE_DECLSPEC int orte_rmgr_base_comm_stop(void);

void orte_rmgr_base_recv(int status, orte_process_name_t* sender,
                         orte_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata);
    
/*
 * DATA TYPE PACKING FUNCTIONS
 */
int orte_rmgr_base_pack_app_context(orte_buffer_t *buffer, void *src,
                                    orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_rmgr_base_pack_app_context_map(orte_buffer_t *buffer, void *src,
                                        orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_rmgr_base_pack_attr_list(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_rmgr_base_pack_attribute(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type);


/*
 * DATA TYPE UNPACKING FUNCTIONS
 */
ORTE_DECLSPEC int orte_rmgr_base_unpack_app_context(orte_buffer_t *buffer, void *dest,
                                                    orte_std_cntr_t *num_vals, orte_data_type_t type);

ORTE_DECLSPEC int orte_rmgr_base_unpack_app_context_map(orte_buffer_t *buffer, void *dest,
                                                        orte_std_cntr_t *num_vals, orte_data_type_t type);

ORTE_DECLSPEC int orte_rmgr_base_unpack_attr_list(orte_buffer_t *buffer, void *dest,
                                                  orte_std_cntr_t *num_vals, orte_data_type_t type);

ORTE_DECLSPEC int orte_rmgr_base_unpack_attribute(orte_buffer_t *buffer, void *dest,
                                                  orte_std_cntr_t *num_vals, orte_data_type_t type);


/*
 * COMPARE FUNCTIONS
 */
int orte_rmgr_base_compare_app_context(orte_app_context_t *value1, orte_app_context_t *value2, orte_data_type_t type);

int orte_rmgr_base_compare_app_context_map(orte_app_context_map_t *value1, orte_app_context_map_t *value2, orte_data_type_t type);

int orte_rmgr_base_compare_attr_list(opal_list_t *value1, opal_list_t *value2, orte_data_type_t type);

int orte_rmgr_base_compare_attribute(orte_attribute_t *value1, orte_attribute_t *value2, orte_data_type_t type);

/*
 * COPY FUNCTIONS
 */
int orte_rmgr_base_copy_app_context(orte_app_context_t **dest, orte_app_context_t *src, orte_data_type_t type);

int orte_rmgr_base_copy_app_context_map(orte_app_context_map_t **dest, orte_app_context_map_t *src, orte_data_type_t type);

int orte_rmgr_base_copy_attr_list(opal_list_t **dest, opal_list_t *src, orte_data_type_t type);

int orte_rmgr_base_copy_attribute(orte_attribute_t **dest, orte_attribute_t *src, orte_data_type_t type);

/*
 * PRINT FUNCTIONS
 */
int orte_rmgr_base_print_app_context(char **output, char *prefix, orte_app_context_t *src, orte_data_type_t type);

int orte_rmgr_base_print_app_context_map(char **output, char *prefix, orte_app_context_map_t *src, orte_data_type_t type);

int orte_rmgr_base_print_attribute(char **output, char *prefix, orte_attribute_t *src, orte_data_type_t type);

int orte_rmgr_base_print_attr_list(char **output, char *prefix, opal_list_t *src, orte_data_type_t type);


/*
 * SIZE FUNCTIONS
 */
int orte_rmgr_base_size_app_context(size_t *size, orte_app_context_t *src, orte_data_type_t type);

int orte_rmgr_base_size_app_context_map(size_t *size, orte_app_context_map_t *src, orte_data_type_t type);

int orte_rmgr_base_size_attr_list(size_t *size, opal_list_t *src, orte_data_type_t type);

int orte_rmgr_base_size_attribute(size_t *size, orte_attribute_t *src, orte_data_type_t type);

/*
 * RELEASE FUNCTIONS
 */
void orte_rmgr_base_std_obj_release(orte_data_value_t *value);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

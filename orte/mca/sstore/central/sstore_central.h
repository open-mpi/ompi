/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * 
 * CENTRAL SSTORE component
 *
 */

#ifndef MCA_SSTORE_CENTRAL_EXPORT_H
#define MCA_SSTORE_CENTRAL_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"

#include "orte/mca/sstore/sstore.h"

BEGIN_C_DECLS

typedef uint8_t orte_sstore_central_cmd_flag_t;
#define ORTE_SSTORE_CENTRAL_CMD OPAL_UINT8
#define ORTE_SSTORE_CENTRAL_PULL 1
#define ORTE_SSTORE_CENTRAL_PUSH 2

    /*
     * Local Component structures
     */
    struct orte_sstore_central_component_t {
        /** Base SSTORE component */
        orte_sstore_base_component_t super;
    };
    typedef struct orte_sstore_central_component_t orte_sstore_central_component_t;
    ORTE_MODULE_DECLSPEC extern orte_sstore_central_component_t mca_sstore_central_component;

    int orte_sstore_central_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int orte_sstore_central_module_init(void);
    int orte_sstore_central_module_finalize(void);

    int orte_sstore_central_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
    int orte_sstore_central_request_restart_handle(orte_sstore_base_handle_t *handle, char *basedir, char *ref, int seq,
                                                   orte_sstore_base_global_snapshot_info_t *snapshot);
    int orte_sstore_central_request_global_snapshot_data(orte_sstore_base_handle_t *handle,
                                                         orte_sstore_base_global_snapshot_info_t *snapshot);
    int orte_sstore_central_register(orte_sstore_base_handle_t handle);

    int orte_sstore_central_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
    int orte_sstore_central_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);

    int orte_sstore_central_sync(orte_sstore_base_handle_t handle);
    int orte_sstore_central_remove(orte_sstore_base_handle_t handle);

    int orte_sstore_central_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
    int orte_sstore_central_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);

    int orte_sstore_central_fetch_app_deps(orte_app_context_t *app);
    int orte_sstore_central_wait_all_deps(void);

    /*
     * HNP functions
     */
int orte_sstore_central_global_module_init(void);
int orte_sstore_central_global_module_finalize(void);
int orte_sstore_central_global_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
int orte_sstore_central_global_request_global_snapshot_data(orte_sstore_base_handle_t *handle,
                                                            orte_sstore_base_global_snapshot_info_t *snapshot);
int orte_sstore_central_global_request_restart_handle(orte_sstore_base_handle_t *handle, char *basedir,
                                                      char *ref, int seq,
                                                      orte_sstore_base_global_snapshot_info_t *snapshot);
int orte_sstore_central_global_register(orte_sstore_base_handle_t handle);
int orte_sstore_central_global_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
int orte_sstore_central_global_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);
int orte_sstore_central_global_sync(orte_sstore_base_handle_t handle);
int orte_sstore_central_global_remove(orte_sstore_base_handle_t handle);
int orte_sstore_central_global_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
int orte_sstore_central_global_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);

    /*
     * Orted functions
     */
int orte_sstore_central_local_module_init(void);
int orte_sstore_central_local_module_finalize(void);
int orte_sstore_central_local_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
int orte_sstore_central_local_register(orte_sstore_base_handle_t handle);
int orte_sstore_central_local_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
int orte_sstore_central_local_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);
int orte_sstore_central_local_sync(orte_sstore_base_handle_t handle);
int orte_sstore_central_local_remove(orte_sstore_base_handle_t handle);
int orte_sstore_central_local_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
int orte_sstore_central_local_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);
void orte_sstore_central_local_recv(int status, orte_process_name_t* sender, opal_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);

    /*
     * Application functions
     */
int orte_sstore_central_app_module_init(void);
int orte_sstore_central_app_module_finalize(void);
int orte_sstore_central_app_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
int orte_sstore_central_app_register(orte_sstore_base_handle_t handle);
int orte_sstore_central_app_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
int orte_sstore_central_app_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);
int orte_sstore_central_app_sync(orte_sstore_base_handle_t handle);
int orte_sstore_central_app_remove(orte_sstore_base_handle_t handle);
int orte_sstore_central_app_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
int orte_sstore_central_app_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);

    /*
     * Internal utility functions
     */

END_C_DECLS

#endif /* MCA_SSTORE_CENTRAL_EXPORT_H */

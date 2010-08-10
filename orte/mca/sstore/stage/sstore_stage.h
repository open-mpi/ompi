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
 * STAGE SSTORE component
 *
 */

#ifndef MCA_SSTORE_STAGE_EXPORT_H
#define MCA_SSTORE_STAGE_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"

#include "orte/mca/sstore/sstore.h"

BEGIN_C_DECLS

typedef uint8_t orte_sstore_stage_cmd_flag_t;
#define ORTE_SSTORE_STAGE_CMD OPAL_UINT8
#define ORTE_SSTORE_STAGE_PULL   1
#define ORTE_SSTORE_STAGE_PUSH   2
#define ORTE_SSTORE_STAGE_REMOVE 3
#define ORTE_SSTORE_STAGE_DONE   4

#define ORTE_SSTORE_LOCAL_SNAPSHOT_DIR_NAME       ("openmpi-local-snapshot")
#define ORTE_SSTORE_LOCAL_SNAPSHOT_STAGE_DIR_NAME   ("stage")
#define ORTE_SSTORE_LOCAL_SNAPSHOT_RESTART_DIR_NAME ("restart")
#define ORTE_SSTORE_LOCAL_SNAPSHOT_CACHE_DIR_NAME   ("cache")

    /*
     * Local Component structures
     */
    struct orte_sstore_stage_component_t {
        /** Base SSTORE component */
        orte_sstore_base_component_t super;
    };
    typedef struct orte_sstore_stage_component_t orte_sstore_stage_component_t;
    ORTE_MODULE_DECLSPEC extern orte_sstore_stage_component_t mca_sstore_stage_component;

    extern char * orte_sstore_stage_local_snapshot_dir;
    extern bool   orte_sstore_stage_global_is_shared;
    extern bool   orte_sstore_stage_skip_filem;
    extern bool   orte_sstore_stage_enabled_caching;
    extern bool   orte_sstore_stage_enabled_compression;
    extern int    orte_sstore_stage_compress_delay;
    extern int    orte_sstore_stage_progress_meter;

    int orte_sstore_stage_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int orte_sstore_stage_module_init(void);
    int orte_sstore_stage_module_finalize(void);

    int orte_sstore_stage_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
    int orte_sstore_stage_request_restart_handle(orte_sstore_base_handle_t *handle, char *basedir, char *ref, int seq,
                                                   orte_sstore_base_global_snapshot_info_t *snapshot);
    int orte_sstore_stage_request_global_snapshot_data(orte_sstore_base_handle_t *handle,
                                                       orte_sstore_base_global_snapshot_info_t *snapshot);
    int orte_sstore_stage_register(orte_sstore_base_handle_t handle);

    int orte_sstore_stage_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
    int orte_sstore_stage_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);

    int orte_sstore_stage_sync(orte_sstore_base_handle_t handle);
    int orte_sstore_stage_remove(orte_sstore_base_handle_t handle);

    int orte_sstore_stage_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
    int orte_sstore_stage_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);

    int orte_sstore_stage_fetch_app_deps(orte_app_context_t *app);
    int orte_sstore_stage_wait_all_deps(void);

    /*
     * HNP functions
     */
int orte_sstore_stage_global_module_init(void);
int orte_sstore_stage_global_module_finalize(void);
int orte_sstore_stage_global_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
int orte_sstore_stage_global_request_global_snapshot_data(orte_sstore_base_handle_t *handle,
                                                          orte_sstore_base_global_snapshot_info_t *snapshot);
int orte_sstore_stage_global_register(orte_sstore_base_handle_t handle);
int orte_sstore_stage_global_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
int orte_sstore_stage_global_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);
int orte_sstore_stage_global_sync(orte_sstore_base_handle_t handle);
int orte_sstore_stage_global_remove(orte_sstore_base_handle_t handle);
int orte_sstore_stage_global_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
int orte_sstore_stage_global_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);

    /*
     * Orted functions
     */
int orte_sstore_stage_local_module_init(void);
int orte_sstore_stage_local_module_finalize(void);
int orte_sstore_stage_local_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
int orte_sstore_stage_local_register(orte_sstore_base_handle_t handle);
int orte_sstore_stage_local_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
int orte_sstore_stage_local_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);
int orte_sstore_stage_local_sync(orte_sstore_base_handle_t handle);
int orte_sstore_stage_local_remove(orte_sstore_base_handle_t handle);
int orte_sstore_stage_local_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
int orte_sstore_stage_local_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);
int orte_sstore_stage_local_fetch_app_deps(orte_app_context_t *app);
int orte_sstore_stage_local_wait_all_deps(void);

void orte_sstore_stage_local_process_cmd(int fd,
                                         short event,
                                         void *cbdata);
int orte_sstore_stage_local_process_cmd_action(orte_process_name_t *sender,
                                               orte_sstore_stage_cmd_flag_t command,
                                               orte_sstore_base_handle_t loc_id,
                                               opal_buffer_t* buffer);
    /*
     * Application functions
     */
int orte_sstore_stage_app_module_init(void);
int orte_sstore_stage_app_module_finalize(void);
int orte_sstore_stage_app_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid);
int orte_sstore_stage_app_register(orte_sstore_base_handle_t handle);
int orte_sstore_stage_app_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value);
int orte_sstore_stage_app_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value);
int orte_sstore_stage_app_sync(orte_sstore_base_handle_t handle);
int orte_sstore_stage_app_remove(orte_sstore_base_handle_t handle);
int orte_sstore_stage_app_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle);
int orte_sstore_stage_app_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle);

    /*
     * Internal utility functions
     */

END_C_DECLS

#endif /* MCA_SSTORE_STAGE_EXPORT_H */

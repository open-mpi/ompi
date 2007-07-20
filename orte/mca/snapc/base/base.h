/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#ifndef ORTE_SNAPC_BASE_H
#define ORTE_SNAPC_BASE_H

#include "orte_config.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/dss/dss.h"

#include "orte/mca/snapc/snapc.h"

/*
 * Global functions for MCA overall SNAPC
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Global Snapshot Coordinator RML tags
     */
#define ORTE_SNAPC_GLOBAL_INIT_CMD    0x01
#define ORTE_SNAPC_GLOBAL_TERM_CMD    0x02
    
    /**
     * Global Snapshot Object Maintenance functions
     */
    void orte_snapc_base_snapshot_construct(orte_snapc_base_snapshot_t *obj);
    void orte_snapc_base_snapshot_destruct( orte_snapc_base_snapshot_t *obj);

    void orte_snapc_base_global_snapshot_construct(orte_snapc_base_global_snapshot_t *obj);
    void orte_snapc_base_global_snapshot_destruct( orte_snapc_base_global_snapshot_t *obj);

    /**
     * Initialize the SNAPC MCA framework
     *
     * @retval ORTE_SUCCESS Upon success
     * @retval ORTE_ERROR   Upon failures
     * 
     * This function is invoked during orte_init();
     */
    ORTE_DECLSPEC int orte_snapc_base_open(void);
    
    /**
     * Select an available component.
     *
     * @retval ORTE_SUCCESS Upon Success
     * @retval ORTE_NOT_FOUND If no component can be selected
     * @retval ORTE_ERROR Upon other failure
     *
     */
    ORTE_DECLSPEC int orte_snapc_base_select(bool seed, bool app);
    
    /**
     * Finalize the SNAPC MCA framework
     *
     * @retval ORTE_SUCCESS Upon success
     * @retval ORTE_ERROR   Upon failures
     * 
     * This function is invoked during orte_finalize();
     */
    ORTE_DECLSPEC int orte_snapc_base_close(void);

    /**
     * 'None' component functions
     * These are to be used when no component is selected.
     * They just return success, and empty strings as necessary.
     */
    int orte_snapc_base_none_open(void);
    int orte_snapc_base_none_close(void);

    int orte_snapc_base_module_init(bool seed, bool app);
    int orte_snapc_base_module_finalize(void);
    int orte_snapc_base_none_setup_job(orte_jobid_t jobid);
    int orte_snapc_base_none_release_job(orte_jobid_t jobid);

    ORTE_DECLSPEC extern int  orte_snapc_base_output;
    ORTE_DECLSPEC extern opal_list_t orte_snapc_base_components_available;
    ORTE_DECLSPEC extern orte_snapc_base_component_t orte_snapc_base_selected_component;
    ORTE_DECLSPEC extern orte_snapc_base_module_t orte_snapc;

    /**
     * Globals
     */
#define orte_snapc_base_metadata_filename (strdup("global_snapshot_meta.data"))

    ORTE_DECLSPEC extern char * orte_snapc_base_global_snapshot_dir;
    ORTE_DECLSPEC extern bool   orte_snapc_base_store_in_place;
    ORTE_DECLSPEC extern size_t orte_snapc_base_snapshot_seq_number;


    /**
     * Some utility functions
     */
    ORTE_DECLSPEC char * orte_snapc_ckpt_state_str(size_t state);

    ORTE_DECLSPEC char * orte_snapc_base_unique_global_snapshot_name(pid_t pid);
    ORTE_DECLSPEC char * orte_snapc_base_get_global_snapshot_metadata_file(char *uniq_snapshot_name);
    ORTE_DECLSPEC char * orte_snapc_base_get_global_snapshot_directory(char *uniq_global_snapshot_name);
    ORTE_DECLSPEC int    orte_snapc_base_init_global_snapshot_directory(char *uniq_global_snapshot_name);
    ORTE_DECLSPEC int    orte_snapc_base_get_job_ckpt_info( orte_jobid_t jobid,
                                                            size_t *ckpt_state,
                                                            char **ckpt_snapshot_ref,
                                                            char **ckpt_snapshot_loc);
    ORTE_DECLSPEC int    orte_snapc_base_set_job_ckpt_info( orte_jobid_t jobid,
                                                            size_t ckpt_state,
                                                            char  *ckpt_snapshot_ref,
                                                            char  *ckpt_snapshot_loc);
    ORTE_DECLSPEC int    orte_snapc_base_get_vpid_ckpt_info( orte_process_name_t proc,
                                                             size_t *ckpt_state,
                                                             char **ckpt_ref,
                                                             char **ckpt_loc);
    ORTE_DECLSPEC int    orte_snapc_base_set_vpid_ckpt_info( orte_process_name_t proc,
                                                             size_t ckpt_state,
                                                             char *ckpt_ref,
                                                             char *ckpt_loc);
    ORTE_DECLSPEC int orte_snapc_base_add_timestamp(char * global_snapshot_ref);
    ORTE_DECLSPEC int orte_snapc_base_add_vpid_metadata(orte_process_name_t *proc,
                                                        char * global_snapshot_ref,
                                                        char *snapshot_ref,
                                                        char *snapshot_location);
    ORTE_DECLSPEC int orte_snapc_base_finalize_metadata(char * global_snapshot_ref);
    ORTE_DECLSPEC int orte_snapc_base_extract_metadata(orte_snapc_base_global_snapshot_t *snapshot);

    /*******************************
     * Global Coordinator functions
     *******************************/
    /* Initial handshake with the orte_checkpoint command */
    ORTE_DECLSPEC int orte_snapc_base_global_coord_ckpt_init_cmd(orte_process_name_t* peer, bool *term, orte_jobid_t *jobid);
    ORTE_DECLSPEC int orte_snapc_base_global_coord_ckpt_update_cmd(orte_process_name_t* peer, char *global_snapshot_handle, int seq_num, int ckpt_status);

    ORTE_DECLSPEC int orte_snapc_base_global_coord_recv_ack(orte_process_name_t* peer, bool *ack);
    ORTE_DECLSPEC int orte_snapc_base_global_coord_send_ack(orte_process_name_t* peer, bool  ack);

    ORTE_DECLSPEC int orte_snapc_base_global_init_request(orte_jobid_t jobid,
                                                          orte_rml_buffer_callback_fn_t rml_cbfunc, void* rml_cbdata,
                                                          orte_gpr_notify_cb_fn_t       gpr_cbfunc, void* gpr_cbdata);

    ORTE_DECLSPEC int orte_snapc_base_extract_gpr_vpid_ckpt_info(orte_gpr_notify_data_t *data,
                                                                 orte_process_name_t **proc,
                                                                 size_t *ckpt_state,
                                                                 char **ckpt_snapshot_ref,
                                                                 char **ckpt_snapshot_loc);
    ORTE_DECLSPEC int orte_snapc_base_extract_gpr_job_ckpt_info(orte_gpr_notify_data_t *data,
                                                                size_t *ckpt_state,
                                                                char **ckpt_snapshot_ref,
                                                                char **ckpt_snapshot_loc);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_SNAPC_BASE_H */

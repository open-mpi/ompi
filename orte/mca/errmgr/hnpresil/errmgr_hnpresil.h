/*
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * 
 */

#ifndef MCA_ERRMGR_HNPRESIL_EXPORT_H
#define MCA_ERRMGR_HNPRESIL_EXPORT_H

#include "orte_config.h"

#include "orte/mca/errmgr/errmgr.h"

BEGIN_C_DECLS

/*
 * Local Component structures
 */
struct orte_errmgr_hnpresil_component_t {
    orte_errmgr_base_component_t super;  /** Base Errmgr component */

    bool ignore_current_update;
    bool term_in_progress;

#if OPAL_ENABLE_FT_CR
    /* State of the Recovery */
    bool crmig_in_progress;
    bool autor_in_progress;

    /* CRMig Options */
    bool crmig_enabled;
    bool crmig_timing_enabled;

    /* AutoR Options */
    bool autor_enabled;
    bool autor_timing_enabled;
    int  autor_recovery_delay;
    bool autor_skip_oldnode;
#endif
};
typedef struct orte_errmgr_hnpresil_component_t orte_errmgr_hnpresil_component_t;
OPAL_MODULE_DECLSPEC extern orte_errmgr_hnpresil_component_t mca_errmgr_hnpresil_component;

int orte_errmgr_hnpresil_component_query(mca_base_module_t **module, int *priority);

void orte_errmgr_hnpresil_update_proc(orte_job_t *jdata,
                                   orte_process_name_t *proc,
                                   orte_proc_state_t state,
                                   pid_t pid,
                                   orte_exit_code_t exit_code);

/***************************
 * Module functions: Global
 ***************************/
int orte_errmgr_hnpresil_global_module_init(void);
int orte_errmgr_hnpresil_global_module_finalize(void);

int orte_errmgr_hnpresil_global_update_state(orte_jobid_t job,
                                        orte_job_state_t jobstate,
                                        orte_process_name_t *proc_name,
                                        orte_proc_state_t state,
                                        pid_t pid,
                                        orte_exit_code_t exit_code);
int orte_errmgr_hnpresil_global_predicted_fault(opal_list_t *proc_list,
                                           opal_list_t *node_list,
                                           opal_list_t *suggested_map);
int orte_errmgr_hnpresil_global_suggest_map_targets(orte_proc_t *proc,
                                               orte_node_t *oldnode,
                                               opal_list_t *node_list);
int orte_errmgr_hnpresil_global_ft_event(int state);
int orte_errmgr_hnpresil_global_post_startup(void);
int orte_errmgr_hnpresil_global_pre_shutdown(void);
int orte_errmgr_hnpresil_global_mark_processes_as_dead(opal_pointer_array_t *dead_procs);
int orte_errmgr_hnpresil_global_failure_notification(orte_process_name_t *sender, opal_buffer_t *buffer);
int orte_errmgr_hnpresil_record_dead_process(orte_process_name_t *proc);

/* hnpresil Versions */
int orte_errmgr_hnpresil_base_global_init(void);
int orte_errmgr_hnpresil_base_global_finalize(void);
int orte_errmgr_hnpresil_base_global_update_state(orte_jobid_t job,
                                             orte_job_state_t jobstate,
                                             orte_process_name_t *proc,
                                             orte_proc_state_t state,
                                             pid_t pid,
                                             orte_exit_code_t exit_code);
int orte_errmgr_hnpresil_base_global_ft_event(int state);

#if OPAL_ENABLE_FT_CR
/* CRMig Versions */
int orte_errmgr_hnpresil_crmig_global_module_init(void);
int orte_errmgr_hnpresil_crmig_global_module_finalize(void);

int orte_errmgr_hnpresil_crmig_global_update_state(orte_jobid_t job,
                                                orte_job_state_t jobstate,
                                                orte_process_name_t *proc_name,
                                                orte_proc_state_t state,
                                                pid_t pid,
                                                orte_exit_code_t exit_code);
int orte_errmgr_hnpresil_crmig_global_predicted_fault(opal_list_t *proc_list,
                                                   opal_list_t *node_list,
                                                   opal_list_t *suggested_map);
int orte_errmgr_hnpresil_crmig_global_suggest_map_targets(orte_proc_t *proc,
                                                       orte_node_t *oldnode,
                                                       opal_list_t *node_list);
int orte_errmgr_hnpresil_crmig_global_ft_event(int state);

/* AutoR Versions */
int orte_errmgr_hnpresil_autor_global_module_init(void);
int orte_errmgr_hnpresil_autor_global_module_finalize(void);

int orte_errmgr_hnpresil_autor_global_update_state(orte_jobid_t job,
                                                orte_job_state_t jobstate,
                                                orte_process_name_t *proc_name,
                                                orte_proc_state_t state,
                                                pid_t pid,
                                                orte_exit_code_t exit_code);
int orte_errmgr_hnpresil_autor_global_suggest_map_targets(orte_proc_t *proc,
                                                       orte_node_t *oldnode,
                                                       opal_list_t *node_list);
int orte_errmgr_hnpresil_autor_global_ft_event(int state);
#endif

END_C_DECLS

#endif /* MCA_ERRMGR_HNPRESIL_EXPORT_H */

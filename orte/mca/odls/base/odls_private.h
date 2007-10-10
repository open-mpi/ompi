/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#ifndef MCA_ODLS_PRIVATE_H
#define MCA_ODLS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rmgr/rmgr_types.h"
#include "orte/mca/smr/smr_types.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/gpr/gpr_types.h"

#include "orte/mca/odls/odls_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * General ODLS types
 */

/*
 * List object to locally store the process names and pids of
 * our children. This can subsequently be used to order termination
 * or pass signals without looking the info up again.
 */
typedef struct orte_odls_child_t {
    opal_list_item_t super;      /* required to place this on a list */
    orte_process_name_t *name;   /* the OpenRTE name of the proc */
    orte_vpid_t local_rank;      /* local rank of the proc on this node */
    orte_std_cntr_t num_procs;   /* number of procs from this job on this node */
    pid_t pid;                   /* local pid of the proc */
    orte_std_cntr_t app_idx;     /* index of the app_context for this proc */
    bool alive;                  /* is this proc alive? */
    orte_proc_state_t state;     /* the state of the process */
    int exit_code;               /* process exit code */
    unsigned long cpu_set;
    bool sync_required;          /* require sync before termination */
} orte_odls_child_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_odls_child_t);
    
/*
 * List object to locally store app_contexts returned by the
 * registry subscription. Since we don't know how many app_contexts will
 * be returned, we need to store them on a list.
 */
typedef struct orted_odls_app_context_t {
    opal_list_item_t super;      /* required to place this on a list */
    orte_app_context_t *app_context;
} orte_odls_app_context_t;
OBJ_CLASS_DECLARATION(orte_odls_app_context_t);


typedef struct orte_odls_globals_t {
    /** Verbose/debug output stream */
    int output;
    /** Time to allow process to forcibly die */
    int timeout_before_sigkill;
    /* mutex */
    opal_mutex_t mutex;
    /* condition variable */
    opal_condition_t cond;
    /* list of children for this orted */
    opal_list_t children;
} orte_odls_globals_t;

ORTE_DECLSPEC extern orte_odls_globals_t orte_odls_globals;
        
ORTE_DECLSPEC int orte_odls_base_report_spawn(opal_list_t *children);

ORTE_DECLSPEC void orte_odls_base_purge_mca_params(char ***env);

/*
 * Default functions that are common to most environments - can
 * be overridden by specific environments if they need something
 * different (e.g., bproc)
 */
int orte_odls_base_default_get_add_procs_data(orte_gpr_notify_data_t **data,
                                              orte_job_map_t *map);

int orte_odls_base_default_construct_child_list(orte_gpr_notify_data_t *data,
                                                orte_jobid_t *job,
                                                orte_vpid_t *vpid_start,
                                                orte_vpid_t *vpid_range,
                                                orte_std_cntr_t *total_slots_allocated,
                                                bool *node_included,
                                                bool *oversubscribed,
                                                bool *override_oversubscribed,
                                                opal_list_t *app_context_list);

/* define a function that will fork a local proc */
typedef int (*orte_odls_base_fork_local_proc_fn_t)(orte_app_context_t *context,
                                                   orte_odls_child_t *child,
                                                   char **environ_copy);

int orte_odls_base_default_launch_local(orte_jobid_t job, opal_list_t *app_context_list,
                                        orte_vpid_t vpid_start, orte_vpid_t vpid_range,
                                        orte_std_cntr_t total_slots_allocated,
                                        bool oversubscribed,
                                        bool override_oversubscribed,
                                        orte_odls_base_fork_local_proc_fn_t fork_local);

int orte_odls_base_default_extract_proc_map_info(orte_process_name_t *daemon,
                                                 orte_process_name_t *proc,
                                                 orte_gpr_value_t *value);

int orte_odls_base_default_deliver_message(orte_jobid_t job, orte_buffer_t *buffer, orte_rml_tag_t tag);

void odls_base_default_wait_local_proc(pid_t pid, int status, void* cbdata);

/* define a function type to signal a local proc */
typedef int (*orte_odls_base_signal_local_fn_t)(pid_t pid, int signum);

int orte_odls_base_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal,
                                              orte_odls_base_signal_local_fn_t signal_local);

/* define a function type for killing a local proc */
typedef int (*orte_odls_base_kill_local_fn_t)(pid_t pid, int signum);

/* define a function type to detect that a child died */
typedef bool (*orte_odls_base_child_died_fn_t)(pid_t pid, unsigned int timeout, int *exit_status);

int orte_odls_base_default_kill_local_procs(orte_jobid_t job, bool set_state,
                                            orte_odls_base_kill_local_fn_t kill_local,
                                            orte_odls_base_child_died_fn_t child_died);

int orte_odls_base_default_require_sync(orte_process_name_t *proc);

/*
 * data type functions
 */

int orte_odls_compare_daemon_cmd(orte_daemon_cmd_flag_t *value1, orte_daemon_cmd_flag_t *value2, orte_data_type_t type);

int orte_odls_copy_daemon_cmd(orte_daemon_cmd_flag_t **dest, orte_daemon_cmd_flag_t *src, orte_data_type_t type);

int orte_odls_pack_daemon_cmd(orte_buffer_t *buffer, const void *src,
                              orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_odls_print_daemon_cmd(char **output, char *prefix, orte_daemon_cmd_flag_t *src, orte_data_type_t type);

void orte_odls_std_release(orte_data_value_t *value);

int orte_odls_size_daemon_cmd(size_t *size, orte_daemon_cmd_flag_t *src, orte_data_type_t type);

int orte_odls_unpack_daemon_cmd(orte_buffer_t *buffer, void *dest,
                                orte_std_cntr_t *num_vals, orte_data_type_t type);

/*
 * Preload binary/files functions
 */
ORTE_DECLSPEC int orte_odls_base_preload_files_app_context(orte_app_context_t* context);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

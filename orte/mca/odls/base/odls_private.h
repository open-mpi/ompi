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
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/odls/odls_types.h"

BEGIN_C_DECLS

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
    pid_t pid;                   /* local pid of the proc */
    orte_std_cntr_t app_idx;     /* index of the app_context for this proc */
    bool alive;                  /* is this proc alive? */
    orte_std_cntr_t num_nodes;   /* #nodes involved in launching this child */
    bool coll_recvd;             /* collective operation recvd */
    orte_proc_state_t state;     /* the state of the process */
    orte_exit_code_t exit_code;  /* process exit code */
    unsigned long cpu_set;
    char *rml_uri;               /* contact info for this child */
    char *slot_list;             /* list of slots for this child */
} orte_odls_child_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_odls_child_t);

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
        
/*
 * Default functions that are common to most environments - can
 * be overridden by specific environments if they need something
 * different (e.g., bproc)
 */
ORTE_DECLSPEC int
orte_odls_base_default_get_add_procs_data(opal_buffer_t *data,
                                          orte_jobid_t job);

ORTE_DECLSPEC int
orte_odls_base_default_construct_child_list(opal_buffer_t *data,
                                            orte_jobid_t *job,
                                            orte_std_cntr_t *num_local_procs,
                                            orte_vpid_t *vpid_range,
                                            orte_std_cntr_t *total_slots_allocated,
                                            bool *node_included,
                                            bool *oversubscribed,
                                            bool *override_oversubscribed,
                                            orte_std_cntr_t *num_contexts,
                                            orte_app_context_t ***app_contexts);

/* define a function that will fork a local proc */
typedef int (*orte_odls_base_fork_local_proc_fn_t)(orte_app_context_t *context,
                                                   orte_odls_child_t *child,
                                                   char **environ_copy);

ORTE_DECLSPEC int
orte_odls_base_default_launch_local(orte_jobid_t job,
                                    orte_std_cntr_t num_apps,
                                    orte_app_context_t **apps,
                                    orte_std_cntr_t num_local_procs,
                                    orte_vpid_t vpid_range,
                                    orte_std_cntr_t total_slots_allocated,
                                    bool oversubscribed,
                                    bool override_oversubscribed,
                                    orte_odls_base_fork_local_proc_fn_t fork_local);

ORTE_DECLSPEC int
orte_odls_base_default_deliver_message(orte_jobid_t job, opal_buffer_t *buffer, orte_rml_tag_t tag);

ORTE_DECLSPEC void odls_base_default_wait_local_proc(pid_t pid, int status, void* cbdata);

/* define a function type to signal a local proc */
typedef int (*orte_odls_base_signal_local_fn_t)(pid_t pid, int signum);

ORTE_DECLSPEC int
orte_odls_base_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal,
                                          orte_odls_base_signal_local_fn_t signal_local);

/* define a function type for killing a local proc */
typedef int (*orte_odls_base_kill_local_fn_t)(pid_t pid, int signum);

/* define a function type to detect that a child died */
typedef bool (*orte_odls_base_child_died_fn_t)(pid_t pid, unsigned int timeout, int *exit_status);

ORTE_DECLSPEC int
orte_odls_base_default_kill_local_procs(orte_jobid_t job, bool set_state,
                                        orte_odls_base_kill_local_fn_t kill_local,
                                        orte_odls_base_child_died_fn_t child_died);

ORTE_DECLSPEC int orte_odls_base_default_require_sync(orte_process_name_t *proc, opal_buffer_t *buf);

/*
 * Preload binary/files functions
 */
ORTE_DECLSPEC int orte_odls_base_preload_files_app_context(orte_app_context_t* context);

/*
 * Collect data to support collective operations across the procs
 */
ORTE_DECLSPEC int orte_odls_base_default_collect_data(orte_process_name_t *proc, opal_buffer_t *buf);

END_C_DECLS

#endif

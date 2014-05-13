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
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_bitmap.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/odls/odls_types.h"

BEGIN_C_DECLS

/*
 * General ODLS types
 */

typedef struct {
    /** Verbose/debug output stream */
    int output;
    /** Time to allow process to forcibly die */
    int timeout_before_sigkill;
    /* list of ranks to be displayed on separate xterms */
    opal_list_t xterm_ranks;
    /* the xterm cmd to be used */
    char **xtermcmd;
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
                                            orte_jobid_t *job);

/* define a function that will fork a local proc */
typedef int (*orte_odls_base_fork_local_proc_fn_t)(orte_app_context_t *context,
                                                   orte_proc_t *child,
                                                   char **environ_copy,
                                                   orte_job_t *jdata);

/* define an object for starting local launch */
typedef struct {
    opal_object_t object;
    opal_event_t *ev;
    orte_jobid_t job;
    orte_odls_base_fork_local_proc_fn_t fork_local;
    int retries;
} orte_odls_launch_local_t;
OBJ_CLASS_DECLARATION(orte_odls_launch_local_t);

#define ORTE_ACTIVATE_LOCAL_LAUNCH(j, f)                                \
    do {                                                                \
        orte_odls_launch_local_t *ll;                                   \
        ll = OBJ_NEW(orte_odls_launch_local_t);                         \
        ll->job = (j);                                                  \
        ll->fork_local = (f);                                           \
        opal_event_set(orte_event_base, ll->ev, -1, OPAL_EV_WRITE,      \
                       orte_odls_base_default_launch_local, ll);        \
        opal_event_set_priority(ll->ev, ORTE_SYS_PRI);                  \
        opal_event_active(ll->ev, OPAL_EV_WRITE, 1);                    \
    } while(0);

ORTE_DECLSPEC void orte_odls_base_default_launch_local(int fd, short sd, void *cbdata);

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
typedef bool (*orte_odls_base_child_died_fn_t)(orte_proc_t *child);

ORTE_DECLSPEC int
orte_odls_base_default_kill_local_procs(opal_pointer_array_t *procs,
                                        orte_odls_base_kill_local_fn_t kill_local,
                                        orte_odls_base_child_died_fn_t child_died);

ORTE_DECLSPEC int orte_odls_base_default_require_sync(orte_process_name_t *proc,
                                                      opal_buffer_t *buffer,
                                                      bool drop_nidmap);

ORTE_DECLSPEC int orte_odls_base_default_restart_proc(orte_proc_t *child,
                                                      orte_odls_base_fork_local_proc_fn_t fork_local);

/*
 * Preload binary/files functions
 */
ORTE_DECLSPEC int orte_odls_base_preload_files_app_context(orte_app_context_t* context);

/*
 * Obtain process stats on a child proc
 */
ORTE_DECLSPEC int orte_odls_base_get_proc_stats(opal_buffer_t *answer, orte_process_name_t *proc);

END_C_DECLS

#endif

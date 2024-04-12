/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_ODLS_BASE_H
#define MCA_ODLS_BASE_H

/*
 * includes
 */
#include "prte_config.h"

#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/iof/base/iof_base_setup.h"
#include "src/mca/odls/odls.h"

BEGIN_C_DECLS

/*
 * General ODLS types
 */

typedef struct {
    /** Verbose/debug output stream */
    int output;
    /* list of ranks to be displayed on separate xterms */
    pmix_list_t xterm_ranks;
    /* the xterm cmd to be used */
    char **xtermcmd;
    /* thread pool */
    int max_threads;
    int num_threads;
    int cutoff;
    prte_event_base_t **ev_bases; // event base array for progress threads
    char **ev_threads;            // event progress thread names
    int next_base;                // counter to load-level thread use
    bool signal_direct_children_only;
    pmix_lock_t lock;
    char *exec_agent;
} prte_odls_globals_t;

PRTE_EXPORT extern prte_odls_globals_t prte_odls_globals;

/*
 * MCA framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_odls_base_framework;
/*
 * Select an available component.
 */
PRTE_EXPORT int prte_odls_base_select(void);

/*
 * Default functions that are common to most environments - can
 * be overridden by specific environments if they need something
 * different (e.g., bproc)
 */
PRTE_EXPORT int prte_odls_base_default_get_add_procs_data(pmix_data_buffer_t *data,
                                                          pmix_nspace_t job);

PRTE_EXPORT int prte_odls_base_default_construct_child_list(pmix_data_buffer_t *data,
                                                            pmix_nspace_t *job);

PRTE_EXPORT void prte_odls_base_spawn_proc(int fd, short sd, void *cbdata);

/* define a function that will fork a local proc */
typedef int (*prte_odls_base_fork_local_proc_fn_t)(void *cd);

/* define an object for fork/exec the local proc */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    char *cmd;
    char *wdir;
    char **argv;
    char **env;
    prte_job_t *jdata;
    prte_app_context_t *app;
    prte_proc_t *child;
    bool index_argv;
    prte_iof_base_io_conf_t opts;
    prte_odls_base_fork_local_proc_fn_t fork_local;
} prte_odls_spawn_caddy_t;
PMIX_CLASS_DECLARATION(prte_odls_spawn_caddy_t);

/* define an object for starting local launch */
typedef struct {
    pmix_object_t object;
    prte_event_t *ev;
    pmix_nspace_t job;
    prte_odls_base_fork_local_proc_fn_t fork_local;
    int retries;
} prte_odls_launch_local_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_odls_launch_local_t);

#define PRTE_ACTIVATE_LOCAL_LAUNCH(j, f)                           \
    do {                                                           \
        prte_odls_launch_local_t *ll;                              \
        ll = PMIX_NEW(prte_odls_launch_local_t);                   \
        PMIX_LOAD_NSPACE(ll->job, (j));                            \
        ll->fork_local = (f);                                      \
        prte_event_set(prte_event_base, ll->ev, -1, PRTE_EV_WRITE, \
                       prte_odls_base_default_launch_local, ll);   \
        prte_event_active(ll->ev, PRTE_EV_WRITE, 1);               \
    } while (0);

PRTE_EXPORT void prte_odls_base_default_launch_local(int fd, short sd, void *cbdata);

PRTE_EXPORT void prte_odls_base_default_wait_local_proc(int fd, short sd, void *cbdata);

/* define a function type to signal a local proc */
typedef int (*prte_odls_base_signal_local_fn_t)(pid_t pid, int signum);

PRTE_EXPORT int
prte_odls_base_default_signal_local_procs(const pmix_proc_t *proc, int32_t signal,
                                          prte_odls_base_signal_local_fn_t signal_local);

/* define a function type for killing a local proc */
typedef int (*prte_odls_base_kill_local_fn_t)(pid_t pid, int signum);

/* define a function type to detect that a child died */
typedef bool (*prte_odls_base_child_died_fn_t)(prte_proc_t *child);

PRTE_EXPORT int prte_odls_base_default_kill_local_procs(pmix_pointer_array_t *procs,
                                                        prte_odls_base_kill_local_fn_t kill_local);

PRTE_EXPORT int prte_odls_base_default_restart_proc(prte_proc_t *child,
                                                    prte_odls_base_fork_local_proc_fn_t fork_local);

/*
 * Preload binary/files functions
 */
PRTE_EXPORT int prte_odls_base_preload_files_app_context(prte_app_context_t *context);

PRTE_EXPORT void prte_odls_base_start_threads(prte_job_t *jdata);

PRTE_EXPORT void prte_odls_base_harvest_threads(void);

#define PRTE_ODLS_SET_ERROR(ns, s, j)                                                   \
do {                                                                                    \
    int _idx;                                                                           \
    prte_proc_t *_cld;                                                                  \
    unsigned int _j = (unsigned int)j;                                                  \
    for (_idx = 0; _idx < prte_local_children->size; _idx++) {                          \
        _cld = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, _idx);  \
        if (NULL == _cld) {                                                             \
            continue;                                                                   \
        }                                                                               \
        if (PMIX_CHECK_NSPACE(ns, _cld->name.nspace) &&                                 \
            (UINT_MAX == _j || _j == _cld->app_idx)) {                                  \
            _cld->exit_code = s;                                                        \
            PRTE_ACTIVATE_PROC_STATE(&_cld->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);    \
        }                                                                               \
    }                                                                                   \
} while(0)

END_C_DECLS
#endif

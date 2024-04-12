/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_PLM_PRIVATE_H
#define MCA_PLM_PRIVATE_H

/*
 * includes
 */
#include "prte_config.h"
#include "types.h"

#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/mca/base/pmix_mca_base_framework.h"

#include "src/mca/odls/odls_types.h"
#include "src/mca/plm/plm_types.h"
#include "src/rml/rml_types.h"
#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

PRTE_EXPORT extern pmix_mca_base_framework_t prte_plm_base_framework;

/* globals for use solely within PLM framework */
typedef struct {
    /* base nspace for this DVM */
    char *base_nspace;
    /* next jobid */
    uint32_t next_jobid;
    /* time when daemons started launch */
    struct timeval daemonlaunchstart;
    /* tree spawn cmd */
    pmix_data_buffer_t tree_spawn_cmd;
    /* daemon nodes assigned at launch */
    bool daemon_nodes_assigned_at_launch;
    size_t node_regex_threshold;
    pmix_list_t daemon_cache;
    bool daemon1_has_reported;
    char **cache;
} prte_plm_globals_t;
/**
 * Global instance of PLM framework data
 */
PRTE_EXPORT extern prte_plm_globals_t prte_plm_globals;

/**
 * Utility routine to set progress engine schedule
 */
PRTE_EXPORT int prte_plm_base_set_progress_sched(int sched);

/*
 * Launch support
 */
PRTE_EXPORT void prte_plm_base_daemon_callback(int status, pmix_proc_t *sender,
                                               pmix_data_buffer_t *buffer, prte_rml_tag_t tag,
                                               void *cbdata);
PRTE_EXPORT void prte_plm_base_daemon_failed(int status, pmix_proc_t *sender,
                                             pmix_data_buffer_t *buffer, prte_rml_tag_t tag,
                                             void *cbdata);
PRTE_EXPORT void prte_plm_base_daemon_topology(int status, pmix_proc_t *sender,
                                               pmix_data_buffer_t *buffer, prte_rml_tag_t tag,
                                               void *cbdata);

PRTE_EXPORT int prte_plm_base_create_jobid(prte_job_t *jdata);
PRTE_EXPORT int prte_plm_base_set_hnp_name(void);
PRTE_EXPORT void prte_plm_base_reset_job(prte_job_t *jdata);
PRTE_EXPORT int prte_plm_base_setup_prted_cmd(int *argc, char ***argv);
PRTE_EXPORT void prte_plm_base_check_all_complete(int fd, short args, void *cbdata);
PRTE_EXPORT int prte_plm_base_setup_virtual_machine(prte_job_t *jdata);

/**
 * Utilities for plm components that use proxy daemons
 */
PRTE_EXPORT int prte_plm_base_prted_exit(prte_daemon_cmd_flag_t command);
PRTE_EXPORT int prte_plm_base_prted_terminate_job(pmix_nspace_t jobid);
PRTE_EXPORT int prte_plm_base_prted_kill_local_procs(pmix_pointer_array_t *procs);
PRTE_EXPORT int prte_plm_base_prted_signal_local_procs(pmix_nspace_t job, int32_t signal);

/*
 * communications utilities
 */
PRTE_EXPORT int prte_plm_base_comm_start(void);
PRTE_EXPORT int prte_plm_base_comm_stop(void);
PRTE_EXPORT void prte_plm_base_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                    prte_rml_tag_t tag, void *cbdata);

/**
 * Construct basic PRTE Daemon command line arguments
 */
PRTE_EXPORT int prte_plm_base_prted_append_basic_args(int *argc, char ***argv, char *ess_module,
                                                      int *proc_vpid_index);

/*
 * Proxy functions for use by daemons and application procs
 * needing dynamic operations
 */
PRTE_EXPORT int prte_plm_proxy_init(void);
PRTE_EXPORT int prte_plm_proxy_spawn(prte_job_t *jdata);
PRTE_EXPORT int prte_plm_proxy_finalize(void);

END_C_DECLS

#endif /* MCA_PLS_PRIVATE_H */

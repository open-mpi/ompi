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
/**
 * @file:
 * Part of the rsh launcher. See pls_rsh.h for an overview of how it works.
 */

#ifndef ORTE_PLS_RSH_EXPORT_H
#define ORTE_PLS_RSH_EXPORT_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/threads/condition.h"
#include "opal/mca/mca.h"

#include "orte/mca/pls/pls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_pls_rsh_component_open(void);
int orte_pls_rsh_component_close(void);
orte_pls_base_module_t* orte_pls_rsh_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_pls_rsh_finalize(void);

/*
 * Interface
 */
int orte_pls_rsh_launch(orte_jobid_t);
int orte_pls_rsh_terminate_job(orte_jobid_t, struct timeval *timeout, opal_list_t*);
int orte_pls_rsh_terminate_orteds(orte_jobid_t, struct timeval *timeout, opal_list_t*);
int orte_pls_rsh_terminate_proc(const orte_process_name_t* proc_name);
int orte_pls_rsh_signal_job(orte_jobid_t, int32_t, opal_list_t*);
int orte_pls_rsh_signal_proc(const orte_process_name_t* proc_name, int32_t);
int orte_pls_rsh_cancel_operation(void);

/**
 * PLS Component
 */
struct orte_pls_rsh_component_t {
    orte_pls_base_component_t super;
    bool debug;
    bool debug_malloc;
    bool debug_daemons;
    bool timing;
    bool reap;
    bool assume_same_shell;
    bool force_rsh;
    int delay;
    int priority;
    char *agent_param;
    char** agent_argv;
    int agent_argc;
    char* agent_path;
    char* orted;
    orte_std_cntr_t num_children;
    orte_std_cntr_t num_concurrent;
    opal_mutex_t lock;
    opal_condition_t cond;
};
typedef struct orte_pls_rsh_component_t orte_pls_rsh_component_t;

ORTE_MODULE_DECLSPEC extern orte_pls_rsh_component_t mca_pls_rsh_component;
extern orte_pls_base_module_t orte_pls_rsh_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_RSH_EXPORT_H */

/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file:
 * Part of the submit launcher. See pls_submit.h for an overview of how it works.
 */

#ifndef ORTE_PLS_SUBMIT_EXPORT_H
#define ORTE_PLS_SUBMIT_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/pls/pls.h"
#include "opal/threads/condition.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_pls_submit_component_open(void);
int orte_pls_submit_component_close(void);
orte_pls_base_module_t* orte_pls_submit_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_pls_submit_finalize(void);

/*
 * Interface
 */
int orte_pls_submit_launch(orte_jobid_t);
int orte_pls_submit_terminate_job(orte_jobid_t, struct timeval *timeout, opal_list_t*);
int orte_pls_submit_terminate_orteds(struct timeval *timeout, opal_list_t*);
int orte_pls_submit_terminate_proc(const orte_process_name_t* proc_name);
int orte_pls_submit_signal_job(orte_jobid_t, int32_t, opal_list_t*);
int orte_pls_submit_signal_proc(const orte_process_name_t* proc_name, int32_t);

/**
 * PLS Component
 */
struct orte_pls_submit_component_t {
    orte_pls_base_component_t super;
    bool debug;
    bool debug_daemons;
    bool timing;
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
typedef struct orte_pls_submit_component_t orte_pls_submit_component_t;

ORTE_MODULE_DECLSPEC extern orte_pls_submit_component_t mca_pls_submit_component;
extern orte_pls_base_module_t orte_pls_submit_module;

END_C_DECLS

#endif /* ORTE_PLS_SUBMIT_EXPORT_H */

/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_PLS_PROCESS_EXPORT_H
#define ORTE_PLS_PROCESS_EXPORT_H

#include "orte_config.h"

#include "opal/threads/condition.h"
#include "opal/mca/mca.h"
#include "orte/mca/pls/pls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_pls_process_component_open(void);
int orte_pls_process_component_close(void);
orte_pls_base_module_t* orte_pls_process_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_pls_process_finalize(void);


/*
 * Interface
 */
int orte_pls_process_launch(orte_jobid_t);
int orte_pls_process_terminate_job(orte_jobid_t);
int orte_pls_process_terminate_proc(const orte_process_name_t* proc_name);
int orte_pls_process_signal_job(orte_jobid_t, int32_t);
int orte_pls_process_signal_proc(const orte_process_name_t* proc_name, int32_t signal);

/**
 * PLS Component
 */
struct orte_pls_process_component_t {
    orte_pls_base_component_t super;
    int debug;
    int priority;
    int reap;
    int timeout_before_sigkill;
    int num_children;
    opal_mutex_t lock;
    opal_condition_t cond;
};
typedef struct orte_pls_process_component_t orte_pls_process_component_t;


ORTE_MODULE_DECLSPEC extern orte_pls_process_component_t mca_pls_process_component;
extern orte_pls_base_module_t orte_pls_process_module;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_PROCESS_EXPORT_H */

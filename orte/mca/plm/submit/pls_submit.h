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
 * Part of the submit launcher. See plm_submit.h for an overview of how it works.
 */

#ifndef ORTE_PLM_SUBMIT_EXPORT_H
#define ORTE_PLM_SUBMIT_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/plm/plm.h"
#include "opal/threads/condition.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_plm_submit_component_open(void);
int orte_plm_submit_component_close(void);
orte_plm_base_module_t* orte_plm_submit_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_plm_submit_finalize(void);

/*
 * Interface
 */
int orte_plm_submit_launch(orte_job_t*);
int orte_plm_submit_terminate_job(orte_jobid_t);
int orte_plm_submit_terminate_orteds(void);
int orte_plm_submit_signal_job(orte_jobid_t, int32_t);

/**
 * PLM Component
 */
struct orte_plm_submit_component_t {
    orte_plm_base_component_t super;
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
typedef struct orte_plm_submit_component_t orte_plm_submit_component_t;

ORTE_MODULE_DECLSPEC extern orte_plm_submit_component_t mca_plm_submit_component;
extern orte_plm_base_module_t orte_plm_submit_module;

END_C_DECLS

#endif /* ORTE_PLM_SUBMIT_EXPORT_H */

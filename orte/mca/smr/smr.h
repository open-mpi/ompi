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
/** @file:
 *
 * The Open MPI State-of-Health Monitoring Subsystem
 *
 */

#ifndef ORTE_SMR_H
#define ORTE_SMR_H

/*
 * includes
 */

#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/mca/mca.h"

#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/smr/smr_types.h"
#include "orte/mca/rmaps/rmaps_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Component functions - all MUST be provided!
 */


/*
 * Query a process state
 */
typedef int (*orte_smr_base_module_get_proc_state_fn_t)(orte_proc_state_t *state,
                                                      int *status,
                                                      orte_process_name_t *proc);

/*
 * Set a process state
 */
typedef int (*orte_smr_base_module_set_proc_state_fn_t)(orte_process_name_t *proc,
                                            orte_proc_state_t state, int status);

/*
 * Query a node state
 */
typedef int (*orte_smr_base_module_get_node_state_fn_t)(orte_node_state_t *state,
                                                      orte_cellid_t cell,
                                                      char *nodename);
/*
 * Set a node state
 */
typedef int (*orte_smr_base_module_set_node_state_fn_t)(orte_cellid_t cell,
                                                      char *nodename,
                                                      orte_node_state_t state);

/*
 * Query a job state
 */
typedef int (*orte_smr_base_module_get_job_state_fn_t)(orte_job_state_t *state,
                                                      orte_jobid_t jobid);

/*
 * Set a job state
 */
typedef int (*orte_smr_base_module_set_job_state_fn_t)(orte_jobid_t jobid,
                                                     orte_job_state_t state);

/*
 * Define the job-specific standard stage gates
 * This function creates all of the ORTE-standard stage gates. 
 */
typedef int (*orte_smr_base_module_job_stage_gate_init_fn_t)(orte_jobid_t job,
                                                             orte_gpr_trigger_cb_fn_t cbfunc,
                                                             void *user_tag);

/*
 * Define the orted standard stage gates
 * This function creates all of the orted-standard stage gates. 
 */
typedef int (*orte_smr_base_module_orted_stage_gate_init_fn_t)(orte_jobid_t job,
                                                               orte_std_cntr_t num_orteds,
                                                               orte_gpr_trigger_cb_fn_t cbfunc,
                                                               void *user_tag);

/*
 * Define an "alert" monitor
 * This function will establish an appropriate trigger to notify the specified
 * callback function when an event takes place. In this case, event is defined
 * by the specified memory location achieving the specified value - e.g., a
 * location could be monitored for a value being set to 1, indicating that a
 * process has aborted.
 *
 * @param job The job that is to be monitored.
 *
 * @param *trigger_name The name of the trigger to be defined.
 *
 * @param *counter_key A string defining the key name of the counter on the registry.
 *
 * @param *counter A pointer to a data_value object that contains the initial
 * value to which the counter should be set.
 *
 * @param *alert_value A pointer to a data_value object that contains the value of
 * the counter that should cause the alert to be sent.
 *
 * @param one_shot Whether or not the trigger should be a one-shot
 *
 * @param cbfunc A registry callback function to be called when the alert fires.
 *
 * @param *user_tag Whatever data the user would like to have passed back to them
 * when the alert is received
 *
 * NOTE: alerts are intended solely for purposes of alerting the caller when
 * an event happens. Thus, they do not convey any information beyond the fact that
 * they fired.
 */
typedef int (*orte_smr_base_module_define_alert_monitor_fn_t)(orte_jobid_t job,
                                                              char *trigger_name,
                                                              char *counter_key,
                                                              orte_std_cntr_t counter,
                                                              orte_std_cntr_t alert_value,
                                                              bool one_shot,
                                                              orte_gpr_trigger_cb_fn_t cbfunc,
                                                              void *user_tag);

/*
 * Initiate monitoring of a job
 * This function notifies the smr that it should initiate monitoring of the specified
 * jobid. It is called by a PLS component at an appropriate point in the launch procedure. Calling
 * the function allows smr components (e.g., the BProc component that monitors daemons
 * via the BProc-provided centralized alerting system) to make the necessary connections
 * for monitoring the job.
 */
typedef int (*orte_smr_base_module_begin_monitoring_fn_t)(orte_job_map_t *map,
                                                          orte_gpr_trigger_cb_fn_t cbfunc,
                                                          void *user_tag);

/*
 * Subscribe to a job stage gate
 */
typedef int (*orte_smr_base_module_job_stage_gate_subscribe_fn_t)(orte_jobid_t job,
                                                                  orte_gpr_notify_cb_fn_t cbfunc, void* cbdata,
                                                                  orte_proc_state_t cb_conditions);

    
/* Shutdown the module nicely 
 */

typedef int (*orte_smr_base_module_finalize_fn_t)(void);



/* below are the prototypes needed by the MCA */
                                                     
/*
 * Ver 1.3.0
 */
struct orte_smr_base_module_1_3_0_t {
    orte_smr_base_module_get_proc_state_fn_t            get_proc_state;
    orte_smr_base_module_set_proc_state_fn_t            set_proc_state;
    orte_smr_base_module_get_node_state_fn_t            get_node_state;
    orte_smr_base_module_set_node_state_fn_t            set_node_state;
    orte_smr_base_module_get_job_state_fn_t             get_job_state;
    orte_smr_base_module_set_job_state_fn_t             set_job_state;
    orte_smr_base_module_begin_monitoring_fn_t          begin_monitoring;
    /* TRIGGER INIT FUNCTIONS */
    orte_smr_base_module_job_stage_gate_init_fn_t       init_job_stage_gates;
    orte_smr_base_module_orted_stage_gate_init_fn_t     init_orted_stage_gates;
    orte_smr_base_module_define_alert_monitor_fn_t      define_alert_monitor;
    orte_smr_base_module_job_stage_gate_subscribe_fn_t  job_stage_gate_subscribe;
    orte_smr_base_module_finalize_fn_t                  finalize;
};

typedef struct orte_smr_base_module_1_3_0_t orte_smr_base_module_1_3_0_t;
typedef orte_smr_base_module_1_3_0_t orte_smr_base_module_t;

/*
 * SOH Component
 */

typedef orte_smr_base_module_t* (*orte_smr_base_component_init_fn_t)(
    int *priority);

typedef int (*orte_smr_base_component_finalize_fn_t)(void);
 
/*
 * the standard component data structure
 */

struct orte_smr_base_component_1_3_0_t {
    mca_base_component_t smr_version;
    mca_base_component_data_1_0_0_t smr_data;
    orte_smr_base_component_init_fn_t  smr_init;
    orte_smr_base_component_finalize_fn_t smr_finalize;
};

typedef struct orte_smr_base_component_1_3_0_t orte_smr_base_component_1_3_0_t;

typedef orte_smr_base_component_1_3_0_t orte_smr_base_component_t;



/*
 * Macro for use in components that are of type ns v1.0.0
 */
#define ORTE_SMR_BASE_VERSION_1_3_0 \
  /* smr v1.3 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* smr v1.3 */ \
  "smr", 1, 3, 0

ORTE_DECLSPEC extern orte_smr_base_module_t orte_smr;  /* holds selected module's function pointers */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_SMR_H */

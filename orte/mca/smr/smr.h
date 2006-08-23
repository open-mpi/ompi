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
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/smr/smr_types.h"

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
 * Initiate monitoring of a job
 * This function notifies the smr that it should initiate monitoring of the specified
 * jobid. It is called by the resource manager once a job has been launched. Calling
 * the function, allows smr components (e.g., the BProc component that monitors daemons
 * via the BProc-provided centralized alerting system) to make the necessary connections
 * for monitoring the job.
 */
typedef int (*orte_smr_base_module_begin_monitoring_fn_t)(orte_jobid_t job);

/* Shutdown the module nicely 
 */

typedef int (*orte_smr_base_module_finalize_fn_t)(void);



/* below are the prototypes needed by the MCA */
                                                     
/*
 * Ver 1.3.0
 */
struct orte_smr_base_module_1_3_0_t {
    orte_smr_base_module_get_proc_state_fn_t      get_proc_state;
    orte_smr_base_module_set_proc_state_fn_t      set_proc_state;
    orte_smr_base_module_get_node_state_fn_t      get_node_state;
    orte_smr_base_module_set_node_state_fn_t      set_node_state;
    orte_smr_base_module_get_job_state_fn_t       get_job_state;
    orte_smr_base_module_set_job_state_fn_t       set_job_state;
    orte_smr_base_module_begin_monitoring_fn_t  begin_monitoring_job;
    orte_smr_base_module_finalize_fn_t          finalize;
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

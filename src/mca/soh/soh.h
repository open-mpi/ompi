/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#ifndef ORTE_SOH_H
#define ORTE_SOH_H

/*
 * includes
 */

#include "orte_config.h"
#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "mca/mca.h"
#include "mca/ns/ns_types.h"
#include "mca/soh/soh_types.h"

#include "soh_types.h"	/* gpr keys and external datatypes needed for prototyping */

/*
 * Component functions - all MUST be provided!
 */


/*
 * Query the state-of-health of a process
 */
typedef int (*orte_soh_base_module_get_proc_soh_fn_t)(orte_proc_state_t *state,
                                                      int *status,
                                                      orte_process_name_t *proc);

/*
 * Set the state-of-health of a process
 */
typedef int (*orte_soh_base_module_set_proc_soh_fn_t)(orte_process_name_t *proc,
                                            orte_proc_state_t state, int status);

/*
 * Query SOH of a node 
 */
typedef int (*orte_soh_base_module_get_node_soh_fn_t)(orte_node_state_t *state,
                                                      orte_cellid_t cell,
                                                      char *nodename);
/*
 * Set SOH of a node
 */
typedef int (*orte_soh_base_module_set_node_soh_fn_t)(orte_cellid_t cell,
                                                      char *nodename,
                                                      orte_node_state_t state);

/*
 * Initiate monitoring of a job
 * This function notifies the soh that it should initiate monitoring of the specified
 * jobid. It is called by the resource manager once a job has been launched. Calling
 * the function, allows soh components (e.g., the BProc component that monitors daemons
 * via the BProc-provided centralized alerting system) to make the necessary connections
 * for monitoring the job.
 */
typedef int (*orte_soh_base_module_begin_monitoring_fn_t)(orte_jobid_t job);

/* Shutdown the module nicely 
 */

typedef int (*orte_soh_base_module_finalize_fn_t)(void);



/* below are the prototypes needed by the MCA */
                                                     
/*
 * Ver 1.0.0
 */
struct orte_soh_base_module_1_0_0_t {
    orte_soh_base_module_get_proc_soh_fn_t      get_proc_soh;
    orte_soh_base_module_set_proc_soh_fn_t      set_proc_soh;
    orte_soh_base_module_get_node_soh_fn_t      get_node_soh;
    orte_soh_base_module_set_node_soh_fn_t      set_node_soh;
    orte_soh_base_module_begin_monitoring_fn_t  begin_monitoring_job;
    orte_soh_base_module_finalize_fn_t          finalize;
};

typedef struct orte_soh_base_module_1_0_0_t orte_soh_base_module_1_0_0_t;
typedef orte_soh_base_module_1_0_0_t orte_soh_base_module_t;

/*
 * SOH Component
 */

typedef orte_soh_base_module_t* (*orte_soh_base_component_init_fn_t)(
    int *priority);

typedef int (*orte_soh_base_component_finalize_fn_t)(void);
 
/*
 * the standard component data structure
 */

struct orte_soh_base_component_1_0_0_t {
    mca_base_component_t 							soh_version;
    mca_base_component_data_1_0_0_t 				soh_data;

    orte_soh_base_component_init_fn_t 				soh_init;
    orte_soh_base_component_finalize_fn_t          soh_finalize;
};

typedef struct orte_soh_base_component_1_0_0_t orte_soh_base_component_1_0_0_t;

typedef orte_soh_base_component_1_0_0_t orte_soh_base_component_t;



/*
 * Macro for use in components that are of type ns v1.0.0
 */
#define MCA_SOH_BASE_VERSION_1_0_0 \
  /* soh v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* soh v1.0 */ \
  "soh", 1, 0, 0

/**
  * Global structure for accessing SOH functions
  */

OMPI_DECLSPEC extern orte_soh_base_module_t orte_soh;  /* holds selected module's function pointers */



#endif /* ORTE_SOH_H */

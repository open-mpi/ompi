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
 * The Open RTE Resource Manager (RMGR) Subsystem
 * 
 * The resource manager (RMGR) subsystem serves as the central
 * switchyard for all resource management activities, including
 * resource discovery, resource allocation, process mapping, and
 * process launch.
 */

#ifndef ORTE_RMGR_H
#define ORTE_RMGR_H

/*
 * includes
 */

#include "orte_config.h"
#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "mca/mca.h"
#include "mca/ns/ns_types.h"
#include "mca/gpr/gpr_types.h"
#include "mca/soh/soh_types.h"
#include "rmgr_types.h"

/*
 * Component functions - all MUST be provided!
 */

/**
 * Query/update a resource
 *
 * @code
 * return_value = orte_rmgr.query();
 * @endcode
 */
typedef int (*orte_rmgr_base_module_query_fn_t)(void);
                                                                                                                                          
/**
 * Create a job. Allocated a jobid and initializes the job segment.
 * 
 * @param app_context   Array of application context values.
 * @param num_context   Number of entries in the app_context array.
 * @param jobid         Returns id allocated to the job.
 *
 * @code
 * orte_jobid_t jobid;
 * 
 * return_value = orte_rmgr.create(app_context,num_context,&jobid);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_create_fn_t)(
    orte_app_context_t** app_context, 
    size_t num_context, 
    orte_jobid_t *jobid);

/**
 * Allocate resources to a job.
 * 
 * @code
 * return_value = orte_rmgr.allocate(orte_jobid_t jobid)
 * @endcode
 */
typedef int (*orte_rmgr_base_module_allocate_fn_t)(orte_jobid_t jobid);

/**
 * Deallocate resources from a job
 *
 * @code
 * return_value = orte_rmgr.deallocate(orte_jobid_t jobid);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_deallocate_fn_t)(orte_jobid_t jobid);

/**
 * Map processes to resources assigned to a job.
 *
 * @code
 * return_value = orte_mgr.map(orte_jobid_t jobid);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_map_fn_t)(orte_jobid_t job);

/**
 * Launch processes that have been mapped.
 *
 * @code
 * return_value = orte_rmgr.launch(orte_jobid_t jobid);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_launch_fn_t)(orte_jobid_t job);

/**
 * Terminate an entire job. 
 *
 * @code
 * return_value = orte_rmgr.terminate_job(orte_jobid_t jobid);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_terminate_job_fn_t)(orte_jobid_t job);

/**
 * Terminate a specific process. 
 *
 * @code
 * return_value = orte_rmgr.terminate_proc(const orte_process_name_t* proc_name);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_terminate_proc_fn_t)(const orte_process_name_t* proc_name);

/*
 * Callback function for resource manager
 */
typedef void (*orte_rmgr_cb_fn_t)(orte_jobid_t jobid, orte_proc_state_t state);

/**
 * Shortcut to spawn an applications. Perform all steps required to 
 * launch the specified application.
 *
 * (1) Create the application context - create a jobid
 * (2) Allocated resources to the job.
 * (3) Map processes to allocated resources
 * (4) Launch the job.
 * (5) Callback function - gets called when job completes (if NULL, then no callback done)
 *
 * @code
 * orte_jobid_t jobid;
 *
 * return_value = orte_rmgr.spawn(app_context, num_context, &jobid, NULL);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_spawn_fn_t)(
    orte_app_context_t** app_context, 
    size_t num_context, 
    orte_jobid_t *jobid,
    orte_rmgr_cb_fn_t cbfn);

/*
 * Init the proc stage gate process
 * A process goes through several stages during its life, each stage being marked by
 * a barrier function that prevents the process from going any further until all
 * processes reach that point. This function initializes the callbacks required
 * to manage that process.
 */
typedef int (*orte_rmgr_base_module_proc_stage_gate_init_fn_t)(orte_jobid_t job);

/*
 * Call the proc stage gate manager
 * As each process achieves a defined barrier (or "stage gate"), it sets its process
 * status (via the SOH) to indicate "at stage gate x". When all process have reached
 * that point, this function is called with a message indicating this has happened.
 * The stage gate manager then takes the appropriate action for that stage gate -
 * usually, broadcasting a message to all processes in the job that allows them
 * to proceed.
 */
typedef void (*orte_rmgr_base_module_proc_stage_gate_mgr_fn_t)(orte_gpr_notify_message_t *notify_msg, void *user_tag);

/**
 * Cleanup resources held by rmgr.
 */

typedef int (*orte_rmgr_base_module_finalize_fn_t)(void);

/*
 * Ver 1.0.0
 */
struct orte_rmgr_base_module_1_0_0_t {
    orte_rmgr_base_module_query_fn_t query;
    orte_rmgr_base_module_create_fn_t create;
    orte_rmgr_base_module_allocate_fn_t allocate;
    orte_rmgr_base_module_deallocate_fn_t deallocate;
    orte_rmgr_base_module_map_fn_t map;
    orte_rmgr_base_module_launch_fn_t launch;
    orte_rmgr_base_module_terminate_job_fn_t terminate_job;
    orte_rmgr_base_module_terminate_proc_fn_t terminate_proc;
    orte_rmgr_base_module_spawn_fn_t spawn;
    orte_rmgr_base_module_proc_stage_gate_init_fn_t stage_gate_init;
    orte_rmgr_base_module_proc_stage_gate_mgr_fn_t stage_gate_mgr;
    orte_rmgr_base_module_finalize_fn_t finalize;
};

typedef struct orte_rmgr_base_module_1_0_0_t orte_rmgr_base_module_1_0_0_t;
typedef orte_rmgr_base_module_1_0_0_t orte_rmgr_base_module_t;

/*
 * RMGR Component
 */

typedef orte_rmgr_base_module_t* (*orte_rmgr_base_component_init_fn_t)(
    int *priority);

 
/*
 * the standard component data structure
 */

struct orte_rmgr_base_component_1_0_0_t {
    mca_base_component_t rmgr_version;
    mca_base_component_data_1_0_0_t rmgr_data;
    orte_rmgr_base_component_init_fn_t rmgr_init;
};
typedef struct orte_rmgr_base_component_1_0_0_t orte_rmgr_base_component_1_0_0_t;
typedef orte_rmgr_base_component_1_0_0_t orte_rmgr_base_component_t;



/**
 * Macro for use in components that are of type rmgr v1.0.0
 */
#define ORTE_RMGR_BASE_VERSION_1_0_0 \
  /* rmgr v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* rmgr v1.0 */ \
  "rmgr", 1, 0, 0

/**
 * Global structure for accessing RAS functions
 */
OMPI_DECLSPEC extern orte_rmgr_base_module_t orte_rmgr;  /* holds selected module's function pointers */

#endif

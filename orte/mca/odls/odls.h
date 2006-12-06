/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
 * @file
 *
 * The OpenRTE Daemon's Local Launch Subsystem
 *
 */

#ifndef ORTE_MCA_ODLS_H
#define ORTE_MCA_ODLS_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"

#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rmaps/rmaps_types.h"

#include "orte/mca/odls/odls_types.h"

/*
 * odls module functions
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    
/**
 * Subscribe to receive the launch data for local processes
 */
typedef int (*orte_odls_base_module_subscribe_launch_data_fn_t)(orte_jobid_t job, orte_gpr_notify_cb_fn_t cbfunc);

/*
 * Construct a notify data object for use in adding local processes
 * In order to reuse daemons, we need a way for the HNP to construct a notify_data object that
 * contains the data needed by the active ODLS component to launch a local process. Since the
 * only one that knows what a particular ODLS component needs is that component, we require an
 * entry point that the HNP can call to get the required notify_data object. This is constructed
 * for *all* nodes - the individual orteds then parse that data to find the specific launch info
 * for procs on their node
 */
typedef int (*orte_odls_base_module_get_add_procs_data_fn_t)(orte_gpr_notify_data_t **data,
                                                             orte_job_map_t *map);

/**
 * Locally launch the provided processes
 */
typedef int (*orte_odls_base_module_launch_local_processes_fn_t)(orte_gpr_notify_data_t *data, char **base_environ);

/**
 * Kill the local processes on this node
 */
typedef int (*orte_odls_base_module_kill_local_processes_fn_t)(orte_jobid_t job, bool set_state);

/**
 * Signal local processes
 */
typedef int (*orte_odls_base_module_signal_local_process_fn_t)(const orte_process_name_t *proc,
                                                              int32_t signal);

/**
 * pls module version 1.3.0
 */
struct orte_odls_base_module_1_3_0_t {
    orte_odls_base_module_subscribe_launch_data_fn_t        subscribe_launch_data;
    orte_odls_base_module_get_add_procs_data_fn_t           get_add_procs_data;
    orte_odls_base_module_launch_local_processes_fn_t       launch_local_procs;
    orte_odls_base_module_kill_local_processes_fn_t         kill_local_procs;
    orte_odls_base_module_signal_local_process_fn_t   		signal_local_procs;
};

/** shorten orte_odls_base_module_1_3_0_t declaration */
typedef struct orte_odls_base_module_1_3_0_t orte_odls_base_module_1_3_0_t;
/** shorten orte_odls_base_module_t declaration */
typedef struct orte_odls_base_module_1_3_0_t orte_odls_base_module_t;

/**
 * odls initialization function
 *
 * Called by the MCA framework to initialize the component.  Invoked
 * exactly once per process.
 *
 * @param priority (OUT) Relative priority or ranking use by MCA to
 *                       select a module.
 */
typedef struct orte_odls_base_module_1_3_0_t*
(*orte_odls_base_component_init_fn_t)(int *priority);

/**
 * Cleanup all resources held by the component
 */
typedef int (*orte_odls_base_component_finalize_fn_t)(void);


/**
 * odls component v1.3.0
 */
struct orte_odls_base_component_1_3_0_t {
    /** component version */
    mca_base_component_t version;
    /** component data */
    mca_base_component_data_1_0_0_t odls_data;
    /** Function called when component is initialized */
    orte_odls_base_component_init_fn_t init;
    /* Function called when component is finalized */
    orte_odls_base_component_finalize_fn_t finalize;
};
/** Convenience typedef */
typedef struct orte_odls_base_component_1_3_0_t orte_odls_base_component_1_3_0_t;
/** Convenience typedef */
typedef orte_odls_base_component_1_3_0_t orte_odls_base_component_t;


/**
 * Macro for use in modules that are of type odls v1.3.0
 */
#define ORTE_ODLS_BASE_VERSION_1_3_0 \
  /* odls v1.3 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* odls v1.3 */ \
  "odls", 1, 3, 0

/* Global structure for accessing ODLS functions
*/
ORTE_DECLSPEC extern orte_odls_base_module_t orte_odls;  /* holds selected module's function pointers */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_ODLS_H */

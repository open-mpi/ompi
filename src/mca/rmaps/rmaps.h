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
 * The Open RTE Resource MAPping Subsystem (RMAPS)
 * 
 * The resource mapping subsystem is responsible for mapping processes
 * to specific nodes/cpus within a given job. In many systems, this
 * functionality will not be supported - the system will map processes
 * wherever it chooses and does not allow the user to specify the
 * mapping. RMAPS components, therefore, provide services for those
 * systems that do permit such mappings.
 * 
 * RMAPS checks the MCA parameters to see if a mapping algorithm has
 * been specified.  If the user selected a mapping algorithm, the
 * indicated RMAPS component will take information from the registry
 * to determine the number of applications/processes to be run, and
 * the identified resources that have been allocated to this job. The
 * selected RMAP component will then assign processes to resources
 * according to its algorithm, with the results stored on the
 * appropriate job segment - the assigned nodename for each process is
 * stored in that respective process' container on the segment.
 * 
 */

#ifndef ORTE_MCA_RMAPS_H
#define ORTE_MCA_RMAPS_H

#include "orte_config.h"
#include "include/orte_constants.h"

#include "mca/mca.h"
#include "mca/ns/ns_types.h"


/*
 * rmaps module functions
 */

/**
 * Mapping function
 */
typedef int (*orte_rmaps_base_module_map_fn_t)(orte_jobid_t job);

/**
 * Cleanup module resources.
 */
typedef int (*orte_rmaps_base_module_finalize_fn_t)(void);

/*
 * rmaps module version 1.0.0
 */
struct orte_rmaps_base_module_1_0_0_t {
    /** Maping function pointer */
    orte_rmaps_base_module_map_fn_t map;
    /** Finalization function pointer */
    orte_rmaps_base_module_finalize_fn_t finalize;
};
/** Convenience typedef */
typedef struct orte_rmaps_base_module_1_0_0_t orte_rmaps_base_module_1_0_0_t;
/** Convenience typedef */
typedef orte_rmaps_base_module_1_0_0_t orte_rmaps_base_module_t;


/*
 * rmaps component
 */

/**
 * Component init / selection
 */
typedef orte_rmaps_base_module_t* (*orte_rmaps_base_component_init_fn_t)(
    int *priority);

 
/**
 * rmaps component version 1.0.0
 */
struct orte_rmaps_base_component_1_0_0_t {
    /** Base MCA structure */
    mca_base_component_t rmaps_version;
    /** Base MCA data */
    mca_base_component_data_1_0_0_t rmaps_data;
    /** Initialization / selection function pointer */
    orte_rmaps_base_component_init_fn_t rmaps_init;
};
/** Convenience typedef */
typedef struct orte_rmaps_base_component_1_0_0_t orte_rmaps_base_component_1_0_0_t;
/** Convenience typedef */
typedef orte_rmaps_base_component_1_0_0_t orte_rmaps_base_component_t;


/**
 * Macro for use in components that are of type rmaps v1.0.0
 */
#define ORTE_RMAPS_BASE_VERSION_1_0_0 \
  /* rmaps v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* rmaps v1.0 */ \
  "rmaps", 1, 0, 0

#endif


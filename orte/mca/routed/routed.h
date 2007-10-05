/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
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
 * Routing table for the RML
 *
 * A flexible routing infrastructure for the RML.  Provides "next hop"
 * service.  Only deals with orte_process_name_ts.
 */


#ifndef ORTE_MCA_ROUTED_ROUTED_H_
#define ORTE_MCA_ROUTED_ROUTED_H_

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/mca.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/gpr/gpr_types.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

BEGIN_C_DECLS


/* ******************************************************************** */


struct orte_buffer_t;
struct orte_process_name_t;
struct orte_rml_module_t;


/* ******************************************************************** */


/**
 * routed component initialization 
 *
 * Create an instance (module) of the given routed component.  Upon
 * returning, the module data structure should be fully populated and
 * all functions should be usable.
 *
 * @return Exactly one module created by the call to the component's
 * initialization function should be returned.  The module structure
 * should be fully populated, and the priority should be set to a
 * reasonable value.
 *
 * @param[out] priority Selection priority for the given component
 *
 * @retval NULL An error occurred and initialization did not occur
 * @retval non-NULL The module was successfully initialized
 */
typedef struct orte_routed_module_t* (*orte_routed_component_init_fn_t)(int  *priority);


/**
 * routed component interface
 *
 * Component interface for the routed framework.  A public instance of
 * this structure, called mca_routed_[component name]_component, must
 * exist in any routed component.
 */
struct orte_routed_component_1_0_0_t {
    /* Base component description */
    mca_base_component_t routed_version;
    /* Base component data block */
    mca_base_component_data_1_0_0_t routed_data;
    /* Component intialization function */
    orte_routed_component_init_fn_t routed_init;
};
/** Convienence typedef */
typedef struct orte_routed_component_1_0_0_t orte_routed_component_t;


/* ******************************************************************** */

/**
 * Finalize the routed module
 *
 * Finalize the routed module, ending cleaning up all resources
 * associated with the module.  After the finalize function is called,
 * all interface functions (and the module structure itself) are not
 * available for use.
 *
 * @note Whether or not the finalize function returns successfully,
 * the module should not be used once this function is called.
 *
 * @retval ORTE_SUCCESS Success
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*orte_routed_module_finalize_fn_t)(void);


/**
 * Update route table with new information
 *
 * Update routing table with a new entry.  If an existing exact match
 * for the entry exists, it will be replaced with the current
 * information.  If the entry is new, it will be inserted behind all
 * entries of similar "mask".  So a wildcard cellid entry will be
 * inserted after any fully-specified entries and any other wildcard
 * cellid entries, but before any wildcard cellid and jobid entries.
 *
 * @retval ORTE_SUCCESS Success
 * @retval ORTE_ERR_NOT_SUPPORTED The updated is not supported.  This 
 *                      is likely due to using partially-specified
 *                      names with a component that does not support
 *                      such functionality
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*orte_routed_module_update_route_fn_t)(orte_process_name_t *target,
                                                    orte_process_name_t *route);


typedef orte_process_name_t (*orte_routed_module_get_route_fn_t)(orte_process_name_t *target);

typedef int (*orte_routed_module_init_routes_fn_t)(orte_jobid_t job, orte_gpr_notify_data_t *ndat);

typedef int (*orte_routed_module_warmup_routes_fn_t)(void);

/* ******************************************************************** */


/**
 * routed module interface
 *
 * Module interface to the routed communication system.  A global
 * instance of this module, orte_routed, provices an interface into the
 * active routed interface.
 */
struct orte_routed_module_t {
    /** Shutdown the communication system and clean up resources */
    orte_routed_module_finalize_fn_t                finalize;

    orte_routed_module_update_route_fn_t            update_route;
    orte_routed_module_get_route_fn_t               get_route;
    orte_routed_module_init_routes_fn_t             init_routes;
    orte_routed_module_warmup_routes_fn_t           warmup_routes;
};
/** Convienence typedef */
typedef struct orte_routed_module_t orte_routed_module_t;

/** Interface for routed communication */
ORTE_DECLSPEC extern orte_routed_module_t orte_routed;


/* ******************************************************************** */


/** Macro for use in components that are of type routed v1.0.0 */
#define ORTE_ROUTED_BASE_VERSION_1_0_0 \
  /* routed v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* routed v1.0 */ \
  "routed", 1, 0, 0


/* ******************************************************************** */


END_C_DECLS

#endif

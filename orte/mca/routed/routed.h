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

#include "orte/types.h"
#include "opal/mca/mca.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/rml/rml_types.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

BEGIN_C_DECLS


/* ******************************************************************** */


struct opal_buffer_t;
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
 * Initialize the routed module
 *
 * Do whatever needs to be done to initialize the selected module
 *
 * @retval ORTE_SUCCESS Success
 * @retval ORTE_ERROR  Error code from whatever was encountered
 */
typedef int (*orte_routed_module_init_fn_t)(void);

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

/**
 * Get the next hop towards the target
 *
 * Obtain the next process on the route to the target. ORTE's routing system
 * works one hop at-a-time, so this function doesn't return the entire path
 * to the target - it only returns the next hop. This could be the target itself,
 * or it could be an intermediate relay. By design, we -never- use application
 * procs as relays, so any relay will be an orted.
 */
typedef orte_process_name_t (*orte_routed_module_get_route_fn_t)(orte_process_name_t *target);

/**
 * Initialize the routing table
 *
 * Initialize the routing table for the specified job. This can be rather complex
 * and depends entirely upon both the selected module AND whether the function
 * is being called by the HNP, an orted, a tool, or an application proc. To
 * understand what is happening, you really need to look at the specific module.
 *
 * Regardless, at the end of the function, the routes to any other process in the
 * specified job -must- be defined (even if it is direct)
 */
typedef int (*orte_routed_module_init_routes_fn_t)(orte_jobid_t job, opal_buffer_t *ndat);

/**
 * Report a route as "lost"
 *
 * Report that an existing connection has been lost, therefore potentially
 * "breaking" a route in the routing table. It is critical that broken
 * connections be reported so that the selected routing module has the
 * option of dealing with it. This could consist of nothing more than
 * removing that route from the routing table, or could - in the case
 * of a "lifeline" connection - result in abort of the process.
 */
typedef int (*orte_routed_module_route_lost_fn_t)(const orte_process_name_t *route);

/**
 * Get wireup data for the specified job
 *
 * Given a jobid and a pointer to a buffer, add whatever routing data
 * this module requires to allow inter-process messaging for the
 * job.
 */
typedef int (*orte_routed_module_get_wireup_info_fn_t)(orte_jobid_t job,
                                                       opal_buffer_t *buf);


/**
 * Handle fault tolerance updates
 *
 * @param[in] state Fault tolerance state update
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int  (*orte_routed_module_ft_event_fn_t)(int state);

/* ******************************************************************** */


/**
 * routed module interface
 *
 * Module interface to the routed communication system.  A global
 * instance of this module, orte_routed, provices an interface into the
 * active routed interface.
 */
struct orte_routed_module_t {
    /** Startup/shutdown the communication system and clean up resources */
    orte_routed_module_init_fn_t                    initialize;
    orte_routed_module_finalize_fn_t                finalize;
    /* API functions */
    orte_routed_module_update_route_fn_t            update_route;
    orte_routed_module_get_route_fn_t               get_route;
    orte_routed_module_init_routes_fn_t             init_routes;
    orte_routed_module_route_lost_fn_t              route_lost;
    orte_routed_module_get_wireup_info_fn_t         get_wireup_info;
    /* FT Notification */
    orte_routed_module_ft_event_fn_t                ft_event;
};
/** Convenience typedef */
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

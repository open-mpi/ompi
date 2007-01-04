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
#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/smr/smr_types.h"
#include "rmgr_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Component functions - all MUST be provided!
 */

/**
 * Setup a job. Allocated a jobid and initializes the job segment.
 *
 * @param app_context   Array of application context values.
 * @param num_context   Number of entries in the app_context array.
 * @param jobid         Returns id allocated to the job.
 *
 * @code
 * orte_jobid_t jobid;
 *
 * return_value = orte_rmgr.setup_job(app_context,num_context,&jobid);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_setup_job_fn_t)(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t *jobid,
    opal_list_t *attrs);

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
 * (5) Callback function - gets called all procs reach specified conditions (if NULL, then no callback done)
 * (6) callback conditions - flag indicating which triggers are to generate callbacks to the specified function
 *
 * @code
 * orte_jobid_t jobid;
 *
 * return_value = orte_rmgr.spawn(app_context, num_context, &jobid, NULL, 0);
 * @endcode
 */
typedef int (*orte_rmgr_base_module_spawn_job_fn_t)(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t *jobid,
    orte_std_cntr_t num_connect,
    orte_process_name_t *connect,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions,
    opal_list_t *attributes);


/**
 * Connect a process to other processes, possibly in other jobs. Note that this
 * function supports WILDCARD process name fields. Thus, a process can request
 * connection to all other processes in another job by providing a single
 * entry in the connect array that has a cellid of ORTE_CELLID_WILDCARD, the
 * desired jobid, and a vpid of ORTE_VPID_WILDCARD.
 */
typedef int (*orte_rmgr_base_module_connect_fn_t)(orte_std_cntr_t num_connect,
                                                  orte_process_name_t *connect);

/**
 * Disconnect a process from one or more other processes. Note that this
 * function supports WILDCARD process name fields. Thus, a process can request
 * to disconnect from all other processes in another job by providing a single
 * entry in the connect array that has a cellid of ORTE_CELLID_WILDCARD, the
 * desired jobid, and a vpid of ORTE_VPID_WILDCARD.
 */
typedef int (*orte_rmgr_base_module_disconnect_fn_t)(orte_std_cntr_t num_disconnect,
                                                     orte_process_name_t *disconnect);


/**
 * Allow module-specific init.
 */

typedef int (*orte_rmgr_base_module_init_fn_t)(void);

/**
    * Cleanup resources held by rmgr.
 */

typedef int (*orte_rmgr_base_module_finalize_fn_t)(void);

/**
    * GENERAL UTILITY FUNCTIONS
 */

/**
 * Find an attribute
 * Given a list of attributes, return a pointer to the specified attribute
 *
 * @param attr_list A pointer to the list of attributes
 * @param key The key indicating the attribute to be returned.
 * @retval ptr A pointer to the orte_gpr_keyval_t containing the attribute.
 * Note that this value is *not* being duplicated, nor is it being
 * removed from the list, so alterations or release of the object will impact the list!
 * @retval NULL The specified attribute was not found - this function does not
 * consider this to be an error, so no ORTE_ERROR_LOG is printed out when this happens.
 */
typedef orte_attribute_t* (*orte_rmgr_base_module_find_attribute_fn_t)(opal_list_t* attr_list, char* key);

/**
 * Add an attribute
 * Given a list of attributes and the data for a new attribute,
 * this function will create the gpr_keyval_t object for that attribute,
 * populate it with the provided data, and append it to the list. If
 * overwrite is set to true AND the value is found on the list, then
 * it will be overwritten with the new value. If overwrite is NOT set
 * and the value is found on the list, it will be left alone - the value
 * will NOT be updated with the one provided.
 *
 * @param attr_list A pointer to the list of attributes
 * @param key The key for the attribute.
 * @param type The data type to be stored in the attribute. A value
 * of ORTE_UNDEF is acceptable to indicate that no data is being stored -
 * the existence of the attribute on the list is all that is required.
 * @param data A pointer to the data to be stored in the attribute. NULL
 * is acceptable IF the data type is ORTE_UNDEF.
 * @param overwrite Indicates if a pre-existing value can be overwritten or not
 * @retval ORTE_SUCCESS Attribute was added to list.
 * @retval ORTE_ERROR An appropriate error code indicating what went wrong.
 */
typedef int (*orte_rmgr_base_module_add_attribute_fn_t)(opal_list_t* attr_list, char* key,
                                                        orte_data_type_t type, void *data,
                                                        bool overwrite);


/**
 * Merge two attribute lists
 * Given two lists of attributes, this function will merge the second list into
 * the first. The boolean defines how to handle matching entries - if set to
 * true (ORTE_RMGR_ATTR_OVERRIDE), entries in the second list will OVERWRITE
 * matching entries in the first list. If set to false (ORTE_RMGR_ATTR_NO_OVERRIDE)
 * matching entries in the second list will be ignored.
 *
 * @param attr_list A pointer to the list of attributes
 * @param key The key for the attribute.
 * @param type The data type to be stored in the attribute. A value
 * of ORTE_UNDEF is acceptable to indicate that no data is being stored -
 * the existence of the attribute on the list is all that is required.
 * @param data A pointer to the data to be stored in the attribute. NULL
 * is acceptable IF the data type is ORTE_UNDEF.
 * @retval ORTE_SUCCESS Attribute was added to list.
 * @retval ORTE_ERROR An appropriate error code indicating what went wrong.
 */
typedef int (*orte_rmgr_base_module_merge_attributes_fn_t)(opal_list_t* target,
                                                           opal_list_t* source,
                                                           bool override);


/**
 * Delete an attribute
 * Given a pointer array of attributes, delete the specified attribute
 *
 * @param attr_list A pointer to the list of attributes
 * @param key The key indicating the attribute to be deleted.
 * @retval ORTE_SUCCESS Attribute was added to list.
 * @retval ORTE_ERROR An appropriate error code indicating what went wrong. Note that
 * an error code of NOT_FOUND will be returned if the specified attribute is
 * not on the provided list - it is up to the caller to decide if this is an actual
 * error. This function will NOT do an ORTE_ERROR_LOG in the case of NOT_FOUND.
 */
typedef int (*orte_rmgr_base_module_delete_attribute_fn_t)(opal_list_t* attr_list, char* key);


/***   APP_CONTEXT FUNCTIONS   ***/
/*
 * Store an array of app_context objects for a given job/pset
 */
typedef int (*orte_rmgr_base_module_store_app_context_fn_t)(orte_jobid_t jobid,
                                                            orte_app_context_t** app_context,
                                                            orte_std_cntr_t num_context);

/*
 * Get an array of app_context objects for a given job/pset
 */
typedef int (*orte_rmgr_base_module_get_app_context_fn_t)(orte_jobid_t jobid,
                                                          orte_app_context_t ***app_context,
                                                          orte_std_cntr_t *num_context);

/*
 * Check the app_context for changing to a working dir or the HOME dir
 */
typedef int (*orte_rmgr_base_module_check_context_cwd_fn_t)(orte_app_context_t *context,
                                                            bool want_chdir);

/* 
 * Check app_context application for existence
 */
typedef int (*orte_rmgr_base_module_check_context_app_fn_t)(orte_app_context_t *context);

/**
    * VPID FUNCTIONS
 */

/**
    * Store the vpid range of a job
 */
typedef int (*orte_rmgr_base_module_set_vpid_range_fn_t)(orte_jobid_t jobid,
                                                         orte_vpid_t start,
                                                         orte_vpid_t range);


/**
    * Retrieve the vpid range of a job
 */
typedef int (*orte_rmgr_base_module_get_vpid_range_fn_t)(orte_jobid_t jobid,
                                                         orte_vpid_t *start,
                                                         orte_vpid_t *range);


/*
 * Ver 2.0
 */
struct orte_rmgr_base_module_2_0_0_t {
    orte_rmgr_base_module_init_fn_t                 module_init;
    orte_rmgr_base_module_setup_job_fn_t            setup_job;
    orte_rmgr_base_module_spawn_job_fn_t            spawn_job;
    orte_rmgr_base_module_connect_fn_t				connect;
    orte_rmgr_base_module_disconnect_fn_t			disconnect;
    orte_rmgr_base_module_finalize_fn_t             finalize;
    /**   SUPPORT FUNCTIONS   ***/
    orte_rmgr_base_module_find_attribute_fn_t       find_attribute;
    orte_rmgr_base_module_add_attribute_fn_t        add_attribute;
    orte_rmgr_base_module_merge_attributes_fn_t     merge_attributes;
    orte_rmgr_base_module_delete_attribute_fn_t     delete_attribute;
    orte_rmgr_base_module_get_app_context_fn_t      get_app_context;
    orte_rmgr_base_module_store_app_context_fn_t    store_app_context;
    orte_rmgr_base_module_check_context_cwd_fn_t    check_context_cwd;
    orte_rmgr_base_module_check_context_app_fn_t    check_context_app;
    orte_rmgr_base_module_set_vpid_range_fn_t       set_vpid_range;
    orte_rmgr_base_module_get_vpid_range_fn_t       get_vpid_range;
};

typedef struct orte_rmgr_base_module_2_0_0_t orte_rmgr_base_module_2_0_0_t;
typedef orte_rmgr_base_module_2_0_0_t orte_rmgr_base_module_t;

/*
 * RMGR Component
 */

typedef orte_rmgr_base_module_t* (*orte_rmgr_base_component_init_fn_t)(
    int *priority);


/*
 * the standard component data structure
 */

struct orte_rmgr_base_component_2_0_0_t {
    mca_base_component_t rmgr_version;
    mca_base_component_data_1_0_0_t rmgr_data;
    orte_rmgr_base_component_init_fn_t rmgr_init;
};
typedef struct orte_rmgr_base_component_2_0_0_t orte_rmgr_base_component_2_0_0_t;
typedef orte_rmgr_base_component_2_0_0_t orte_rmgr_base_component_t;



/**
 * Macro for use in components that are of type rmgr v2.0.0
 */
#define ORTE_RMGR_BASE_VERSION_2_0_0 \
  /* rmgr v2.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* rmgr v2.0 */ \
  "rmgr", 2, 0, 0

/**
 * Global structure for accessing RAS functions
 */
ORTE_DECLSPEC extern orte_rmgr_base_module_t orte_rmgr;  /* holds selected module's function pointers */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open RTE Error and Recovery Manager (ErrMgr)
 *
 * This framework is a composite framework in which multiple components
 * are often active at the same time and may work on a single external call
 * to the interface functions.
 *
 * This framework allows the user to compose a job recovery policy from multiple
 * individual components. Each component will operate on the function call if it
 * has a registered function. If no component registers a function then the base
 * functionality/policy is used.
 *
 * For example, consider the 3 components on the left (C1, C2, C3), and the
 * API function calls across the top:
 *      | Priority | Fn1  | Fn2  | Fn3  | Fn4  |
 * -----+----------+------+------+------+------+
 * base |   ---    | act0 | ---  | ---  | act6 |
 * C1   |    10    | act1 | ---  | act2 | ---  |
 * C2   |    20    | ---  | act3 | ---  | ---  |
 * C3   |    30    | act4 | act5 | ---  | ---  |
 * -----+----------+------+------+------+------+
 * A call to Fn1 will result in:
 *   act4, act1
 * A call to Fn2 will result in:
 *   act5, act3
 * A call to Fn3 will result in:
 *   act2
 * A call to Fn4 will result in:
 *   act6
 *
 * Notice that when the base function is overridden it is not called. The base
 * function is only called when the function has not been overridden by a
 * component.
 *
 */

#ifndef ORTE_MCA_ERRMGR_H
#define ORTE_MCA_ERRMGR_H

/*
 * includes
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/class/opal_object.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/util/opal_sos.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/plm/plm_types.h"

BEGIN_C_DECLS
/* type definition */
typedef uint8_t orte_errmgr_stack_state_t;

/*
 * Macro definitions
 */
/*
 * Thess macros and associated error name array are used to output intelligible error
 * messages.
 */

#define ORTE_ERROR_NAME(n)  opal_strerror(n)
#define ORTE_ERROR_LOG(n)                       \
    if (true == OPAL_SOS_IS_NATIVE(n)) {        \
        orte_errmgr.log(n, __FILE__, __LINE__); \
    } else {                                    \
        OPAL_SOS_LOG(n);                        \
    }

/****   FRAMEWORK API FUNCTIONS   ****/

/**
 * This is not part of any module so it can be used at any time!
 */
typedef void (*orte_errmgr_base_API_log_fn_t)(int error_code, char *filename, int line);

/**
 * Alert - process aborted
 * This function is called by the PLM when a remote process aborts during execution. Actions taken
 * in response to the abnormal termination of a remote application process will vary across
 * the various errmgr components.
 *
 * NOTE: Local process errors should always be reported through the error_detected interface and
 * NOT here.
 *
 * @param *name Pointer to the name of the proc that aborted
 *
 * @retval ORTE_SUCCESS Whatever action that was taken was successful
 * @retval ORTE_ERROR Appropriate error code
 */
typedef int (*orte_errmgr_base_API_update_state_fn_t)(orte_jobid_t job,
                                                      orte_job_state_t jobstate,
                                                      orte_process_name_t *proc_name,
                                                      orte_proc_state_t state,
                                                      pid_t pid,
                                                      orte_exit_code_t exit_code);

/**
 * Predicted process/node failure notification
 * Composite interface. Called in priority order.
 *
 * @param[in] proc_list List of processes (or NULL if none)
 * @param[in] node_list List of nodes (or NULL if none)
 * @param[in] suggested_nodes List of suggested nodes to use on recovery (or NULL if none)
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_errmgr_base_API_predicted_fault_fn_t)(char ***proc_list,
                                                         char ***node_list,
                                                         char ***suggested_nodes);
/**
 * Suggest a node to map a restarting process onto
 *
 * @param[in] proc Process that is being mapped
 * @param[in] oldnode Previous node where this process resided
 * @param[in|out] node_list List of nodes to select from
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_errmgr_base_API_suggest_map_targets_fn_t)(orte_proc_t *proc,
                                                             orte_node_t *oldnode,
                                                             opal_list_t *node_list);


/**
 * Alert - self aborting
 * This function is called when a process is aborting due to some internal error.
 * It will finalize the process
 * itself, and then exit - it takes no other actions. The intent here is to provide
 * a last-ditch exit procedure that attempts to clean up a little.
 */
typedef int (*orte_errmgr_base_API_abort_fn_t)(int error_code, char *fmt, ...)
#   if OPAL_HAVE_ATTRIBUTE_FORMAT_FUNCPTR
__opal_attribute_format__(__printf__, 2, 3)
#   endif
;

/* global structure for accessing ERRMGR FRAMEWORK API's */
typedef struct {
    orte_errmgr_base_API_log_fn_t                   log;
    orte_errmgr_base_API_update_state_fn_t          update_state;
    orte_errmgr_base_API_predicted_fault_fn_t       predicted_fault;
    orte_errmgr_base_API_suggest_map_targets_fn_t   suggest_map_targets;
    orte_errmgr_base_API_abort_fn_t                 abort;
    
} orte_errmgr_API_t;

ORTE_DECLSPEC extern orte_errmgr_API_t orte_errmgr;




/****    INTERNAL MODULE FUNCTIONS    ****/

/**
 * Module initialization function.
 * Public interface. Will be call in each of the active composite components
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_errmgr_base_module_init_fn_t)
     (void);

/**
 * Module finalization function.
 * Public interface. Will be call in each of the active composite components
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_errmgr_base_module_finalize_fn_t)
     (void);

/*
 * Internal Composite Interfaces corresponding to API interfaces
 */
typedef int (*orte_errmgr_base_module_update_state_fn_t)(orte_jobid_t job,
                                                         orte_job_state_t jobstate,
                                                         orte_process_name_t *proc_name,
                                                         orte_proc_state_t state,
                                                         pid_t pid,
                                                         orte_exit_code_t exit_code,
                                                         orte_errmgr_stack_state_t *stack_state);
typedef int (*orte_errmgr_base_module_predicted_fault_fn_t)(char ***proc_list,
                                                            char ***node_list,
                                                            char ***suggested_nodes,
                                                            orte_errmgr_stack_state_t *stack_state);
typedef int (*orte_errmgr_base_module_suggest_map_targets_fn_t)(orte_proc_t *proc,
                                                                orte_node_t *oldnode,
                                                                opal_list_t *node_list,
                                                                orte_errmgr_stack_state_t *stack_state);

/**
 * Handle fault tolerance updates
 *
 * @param[in] state Fault tolerance state update
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int  (*orte_errmgr_base_ft_event_fn_t)(int state);


/*
 * Module Structure
 */
struct orte_errmgr_base_module_2_3_0_t {
    /** Initialization Function */
    orte_errmgr_base_module_init_fn_t                   init;
    /** Finalization Function */
    orte_errmgr_base_module_finalize_fn_t               finalize;

    /* -------------- Internal Composite Interfaces -- */
    /** Actual process failure notification */
    orte_errmgr_base_module_update_state_fn_t           update_state;
    /** Predicted process/node failure notification */
    orte_errmgr_base_module_predicted_fault_fn_t        predicted_fault;
    /** Suggest a node to map a restarting process onto */
    orte_errmgr_base_module_suggest_map_targets_fn_t    suggest_map_targets;

    /** Handle any FT Notifications */
    orte_errmgr_base_ft_event_fn_t                      ft_event;
};

typedef struct orte_errmgr_base_module_2_3_0_t orte_errmgr_base_module_2_3_0_t;
typedef orte_errmgr_base_module_2_3_0_t orte_errmgr_base_module_t;

/*
 * ErrMgr Component
 */
struct orte_errmgr_base_component_3_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;

    /** Verbosity Level */
    int verbose;
    /** Output Handle for opal_output */
    int output_handle;
    /** Default Priority */
    int priority;
};
typedef struct orte_errmgr_base_component_3_0_0_t orte_errmgr_base_component_3_0_0_t;
typedef orte_errmgr_base_component_3_0_0_t orte_errmgr_base_component_t;


/*
 * Macro for use in components that are of type errmgr
 */
#define ORTE_ERRMGR_BASE_VERSION_3_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "errmgr", 3, 0, 0

END_C_DECLS

#endif

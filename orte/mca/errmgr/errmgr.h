/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
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
 * This framework is the logically central clearing house for process/daemon
 * state updates. In particular when a process fails and another process detects
 * it, then that information is reported through this framework. This framework
 * then (depending on the active component) decides how to handle the failure.
 *
 * For example, if a process fails this may activate an automatic recovery
 * of the process from a previous checkpoint, or initial state. Conversely,
 * the active component could decide not to continue the job, and request that
 * it be terminated. The error and recovery policy is determined by individual
 * components within this framework.
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
#include "opal/class/opal_pointer_array.h"
#include "opal/util/output.h"
#include "opal/util/error.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/plm/plm_types.h"

BEGIN_C_DECLS

/*
 * Structure to describe a predicted process fault.
 *
 * This can be expanded in the future to support assurance levels, and
 * additional information that may wish to be conveyed.
 */
struct orte_errmgr_predicted_proc_t {
    /** This is an object, so must have a super */
    opal_list_item_t super;

    /** Process Name */
    orte_process_name_t proc_name;
};
typedef struct orte_errmgr_predicted_proc_t orte_errmgr_predicted_proc_t;
OBJ_CLASS_DECLARATION(orte_errmgr_predicted_proc_t);

/*
 * Structure to describe a predicted node fault.
 *
 * This can be expanded in the future to support assurance levels, and
 * additional information that may wish to be conveyed.
 */
struct orte_errmgr_predicted_node_t {
    /** This is an object, so must have a super */
    opal_list_item_t super;

    /** Node Name */
    char * node_name;
};
typedef struct orte_errmgr_predicted_node_t orte_errmgr_predicted_node_t;
OBJ_CLASS_DECLARATION(orte_errmgr_predicted_node_t);

/*
 * Structure to describe a suggested remapping element for a predicted fault.
 *
 * This can be expanded in the future to support weights , and
 * additional information that may wish to be conveyed.
 */
struct orte_errmgr_predicted_map_t {
    /** This is an object, so must have a super */
    opal_list_item_t super;

    /** Process Name (predicted to fail) */
    orte_process_name_t proc_name;

    /** Node Name (predicted to fail) */
    char * node_name;

    /** Process Name (Map to) */
    orte_process_name_t map_proc_name;

    /** Node Name (Map to) */
    char * map_node_name;

    /** Just off current node */
    bool off_current_node;

    /** Pre-map fixed node assignment */
    char * pre_map_fixed_node;
};
typedef struct orte_errmgr_predicted_map_t orte_errmgr_predicted_map_t;
OBJ_CLASS_DECLARATION(orte_errmgr_predicted_map_t);


/*
 * Macro definitions
 */
/*
 * Thess macros and associated error name array are used to output intelligible error
 * messages.
 */

#define ORTE_ERROR_NAME(n)  opal_strerror(n)
#define ORTE_ERROR_LOG(n)                       \
        orte_errmgr.logfn(n, __FILE__, __LINE__);

/*
 * Framework Interfaces
 */
/**
 * Module initialization function.
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_errmgr_base_module_init_fn_t)(void);

/**
 * Module finalization function.
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_errmgr_base_module_finalize_fn_t)(void);

/**
 * This is not part of any module so it can be used at any time!
 */
typedef void (*orte_errmgr_base_module_log_fn_t)(int error_code, char *filename, int line);

/**
 * Alert - self aborting
 * This function is called when a process is aborting due to some internal error.
 * It will finalize the process
 * itself, and then exit - it takes no other actions. The intent here is to provide
 * a last-ditch exit procedure that attempts to clean up a little.
 */
typedef void (*orte_errmgr_base_module_abort_fn_t)(int error_code, char *fmt, ...)
__opal_attribute_format_funcptr__(__printf__, 2, 3);

/**
 * Alert - abort peers
 *  This function is called when a process wants to abort one or more peer processes.
 *  For example, MPI_Abort(comm) will use this function to terminate peers in the
 *  communicator group before aborting itself.
 */
typedef int (*orte_errmgr_base_module_abort_peers_fn_t)(orte_process_name_t *procs,
                                                        orte_std_cntr_t num_procs,
                                                        int error_code);

/**
 * Predicted process/node failure notification
 *
 * @param[in] proc_list List of processes (or NULL if none)
 * @param[in] node_list List of nodes (or NULL if none)
 * @param[in] suggested_map List of mapping suggestions to use on recovery (or NULL if none)
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_errmgr_base_module_predicted_fault_fn_t)(opal_list_t *proc_list,
                                                            opal_list_t *node_list,
                                                            opal_list_t *suggested_map);

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
typedef int (*orte_errmgr_base_module_suggest_map_targets_fn_t)(orte_proc_t *proc,
                                                                orte_node_t *oldnode,
                                                                opal_list_t *node_list);

/**
 * Handle fault tolerance updates
 *
 * @param[in] state Fault tolerance state update
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int  (*orte_errmgr_base_module_ft_event_fn_t)(int state);

/**
 * Function to perform actions that require the rest of the ORTE layer to be up
 * and running.
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecified error occured
 */
typedef void (*orte_errmgr_base_module_register_migration_warning_fn_t)(struct timeval *tv);

typedef enum {
    ORTE_ERRMGR_CALLBACK_FIRST,
    ORTE_ERRMGR_CALLBACK_LAST,
    ORTE_ERRMGR_CALLBACK_PREPEND,
    ORTE_ERRMGR_CALLBACK_APPEND
} orte_errmgr_error_order_t;

/** 
 * Register a callback function for faults.
 *
 * This callback function will be used anytime (other than during finalize) the
 * runtime detects and handles a critical failure. The runtime will complete all
 * its stabilization before cycling thru all registered callbacks. The order of
 * the callbacks will proceed in the indicated order with which they were registered.
 *
 * The parameter to the callback function will be the orte_process_name_t
 * of the process involved in the error.
 *
 * @param[in] cbfunc The callback function.
 *
 */
typedef struct {
    orte_process_name_t proc;
    int errcode;
} orte_error_t;

typedef int (orte_errmgr_error_callback_fn_t)(opal_pointer_array_t *errors);
typedef int (*orte_errmgr_base_module_register_error_callback_fn_t)(orte_errmgr_error_callback_fn_t *cbfunc,
                                                                    orte_errmgr_error_order_t order);
typedef void (*orte_errmgr_base_module_execute_error_callbacks_fn_t)(opal_pointer_array_t *errors);

/*
 * Module Structure
 */
struct orte_errmgr_base_module_2_3_0_t {
    /** Initialization Function */
    orte_errmgr_base_module_init_fn_t                       init;
    /** Finalization Function */
    orte_errmgr_base_module_finalize_fn_t                   finalize;

    orte_errmgr_base_module_log_fn_t                        logfn;
    orte_errmgr_base_module_abort_fn_t                      abort;
    orte_errmgr_base_module_abort_peers_fn_t                abort_peers;

    /** Predicted process/node failure notification */
    orte_errmgr_base_module_predicted_fault_fn_t             predicted_fault;
    /** Suggest a node to map a restarting process onto */
    orte_errmgr_base_module_suggest_map_targets_fn_t         suggest_map_targets;

    /** Handle any FT Notifications */
    orte_errmgr_base_module_ft_event_fn_t                    ft_event;

    /* Register to be warned of impending migration */
    orte_errmgr_base_module_register_migration_warning_fn_t  register_migration_warning;

    /* Register a callback function */
    orte_errmgr_base_module_register_error_callback_fn_t     register_error_callback;
    orte_errmgr_base_module_execute_error_callbacks_fn_t     execute_error_callbacks;
};
typedef struct orte_errmgr_base_module_2_3_0_t orte_errmgr_base_module_2_3_0_t;
typedef orte_errmgr_base_module_2_3_0_t orte_errmgr_base_module_t;
ORTE_DECLSPEC extern orte_errmgr_base_module_t orte_errmgr;

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

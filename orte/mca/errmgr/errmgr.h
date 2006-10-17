/*
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
/** @file:
 *
 * The Open RTE Error Manager
 *
 */

#ifndef ORTE_MCA_ERRMGR_H
#define ORTE_MCA_ERRMGR_H

/*
 * includes
 */

#include "orte_config.h"
#include "orte/orte_constants.h"


#include "orte/mca/schema/schema.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/ns/ns_types.h"

#include "opal/mca/mca.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Macro definitions
 */
/*
 * Thess macros and associated error name array are used to output intelligible error
 * messages.
 */

#define ORTE_ERROR_NAME(n)  opal_strerror(n)

#define ORTE_ERROR_LOG(n) \
    orte_errmgr.log((n), __FILE__, __LINE__)


/*
 * Component functions - all MUST be provided!
 */

/**
 * Log an error
 * Log an error that occurred in the runtime environment
 *
 * @code
 * orte_errmgr.log("this is an error", __FILE__, __LINE__);
 * @endcode
 */
typedef void (*orte_errmgr_base_module_log_fn_t)(int error_code, char *filename, int line);


/**
 * Alert - process aborted
 * This function is called when a remote process aborts during execution. The function
 * is called via the GPR's trigger notification system. Actions taken in response
 * to the abnormal termination of a remote application process will vary across
 * the various errmgr components.
 
 * NOTE: Local process errors should always be reported through the error_detected interface and
 * NOT here.
 */
typedef int (*orte_errmgr_base_module_proc_aborted_fn_t)(orte_gpr_notify_message_t *msg);

/**
 * Alert - incomplete start of a job
 * This function is called when an attempted launch of a job encounters failure of
 * one or more processes to start. The strategy for dealing
 * with this "incomplete start" situation varies across the various errmgr components.
 *
 * This function is only called by the respective process launcher, which is responsible
 * for detecting incomplete starts. If on a daemon, the function simply updates the
 * process state to indicate failure to launch - this initiates a trigger that goes to
 * the respective HNP for response.
 *
 * NOTE: Errmgr components on non-HNP and non-daemon processes are expressly forbidden
 * from taking any action to this function call. Instead, they are restricted to simply
 * returning.
 */
typedef int (*orte_errmgr_base_module_incomplete_start_fn_t)(orte_gpr_notify_message_t *msg);

/**
 * Alert - internal error detected
 * This function is called when an internal error is detected within a local process.
 * It decides what to do about the error. In the case of application processes, it simply
 * orders the local process to finalize and terminate. The abnormal termination will be
 * detected and dealt with by the daemon/HNP system.
 *
 * HNPs, of course, cannot simply exit - they must first cleanup their running jobs if at
 * all possible. In some cases, this cannot be done - e.g., if the error detected would
 * prevent operation of the registry or has corrupted memory. In these extreme cases,
 * nothing can really be done.
 *
 * Likewise, orteds have responsibility towards their local application processes and
 * must make some attempt to clean them up before exiting.
 *
 * The function pretty prints an error message if possible.  Error message should be
 * specified using the standard \code printf() format.
 */
typedef void (*orte_errmgr_base_module_error_detected_fn_t)(int error_code, char *fmt, ...);

/*
 * Register a job with the error manager
 * When a job is launched, this function is called so the error manager can register
 * subscriptions on the job segment so that the error manager will be notified when
 * problems occur - i.e., when process status entries change to abnormal termination
 * values. Process status entries are changed by the appropriate state monitor
 * and/or the process launcher, depending upon the stage at which the problem occurs.
 *
 * Monitoring of the job begins once the job has reached the "executing" stage. Prior
 * to that time, failure of processes to start are the responsibility of the respective
 * process launcher - which is expected to call the error manager via the "incomplete
 * start" interface to report any problems prior to the job beginning "execution".
 *
 * NOTE: ONLY HNPs are allowed to register for trigger reports. All other components
 * MUST do nothing but return ORTE_SUCCESS.
 */
typedef int (*orte_errmgr_base_module_register_job_fn_t)(orte_jobid_t job);

/**
 * Alert - self aborting
 * This function is called when a process is aborting. It will finalize the process
 * itself, and then exits - it takes no other actions. The intent here is to provide
 * a last-ditch exit procedure that attempts to clean up a little.
 */
typedef void (*orte_errmgr_base_module_abort_fn_t)(void);

/*
 * Request that the system abort processes other than myself
 * The possibility exists that a process will decide that ONLY a small subset of a job
 * must be aborted. This function allows a process to request that the identified
 * processes be aborted. The "request" portion of the function's name is not
 * by accident - this function specifically does NOT perform the abort process
 * itself, but simply requests that it be done.
 *
 * NOTE: Please ensure that you do NOT include your own process name in the
 * array or else you will be ordered to "die" before you complete this function
 * (i.e., you will be held in a blocking receive pending an answer from the
 * HNP, which won't come before you receive your own "die" command). If you need
 * to die too, then call "abort" after completing this function call.
 */
typedef int (*orte_errmgr_base_module_abort_procs_request_fn_t)(orte_process_name_t *procs, orte_std_cntr_t num_procs);
 
/*
 * Ver 1.0.0
 */
struct orte_errmgr_base_module_1_3_0_t {
    orte_errmgr_base_module_log_fn_t                    log;
    orte_errmgr_base_module_proc_aborted_fn_t           proc_aborted;
    orte_errmgr_base_module_incomplete_start_fn_t       incomplete_start;
    orte_errmgr_base_module_error_detected_fn_t         error_detected;
    orte_errmgr_base_module_register_job_fn_t           register_job;
    orte_errmgr_base_module_abort_fn_t                  abort;
    orte_errmgr_base_module_abort_procs_request_fn_t    abort_procs_request;
};

typedef struct orte_errmgr_base_module_1_3_0_t orte_errmgr_base_module_1_3_0_t;
typedef orte_errmgr_base_module_1_3_0_t orte_errmgr_base_module_t;

/*
 * ERRMGR Component
 */

typedef orte_errmgr_base_module_t* (*orte_errmgr_base_component_init_fn_t)(
    bool *allow_multi_user_threads,
    bool *have_hidden_threads,
    int *priority);

typedef int (*orte_errmgr_base_component_finalize_fn_t)(void);

/*
 * the standard component data structure
 */

struct mca_errmgr_base_component_1_3_0_t {
    mca_base_component_t errmgr_version;
    mca_base_component_data_1_0_0_t errmgr_data;

    orte_errmgr_base_component_init_fn_t errmgr_init;
    orte_errmgr_base_component_finalize_fn_t errmgr_finalize;
};
typedef struct mca_errmgr_base_component_1_3_0_t mca_errmgr_base_component_1_3_0_t;
typedef mca_errmgr_base_component_1_3_0_t mca_errmgr_base_component_t;



/*
 * Macro for use in components that are of type errmgr v1.0.0
 */
#define ORTE_ERRMGR_BASE_VERSION_1_3_0 \
  /* errmgr v1.3 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* errmgr v1.3 */ \
  "errmgr", 1, 3, 0

/* Global structure for accessing error manager functions
 */
ORTE_DECLSPEC extern orte_errmgr_base_module_t orte_errmgr;  /* holds selected module's function pointers */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

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
 * The Open RTE Error Manager
 *
 */

#ifndef ORTE_MCA_ERRMGR_H
#define ORTE_MCA_ERRMGR_H

/*
 * includes
 */

#include "orte_config.h"
#include "include/orte_constants.h"
#include "include/orte_schema.h"

#include "mca/mca.h"

/*
 * Macro definitions
 */
#define ORTE_ERROR_LOG(n) \
    orte_errmgr.log((n), __FILE__, __LINE__)


/*
 * Component functions - all MUST be provided!
 */

/**
 * Log an error
 * Log an error that occurred in the runtime environment, and call the "error_detected"
 * interface to see if further action is required.
 * 
 * @code
 * orte_errmgr.log("this is an error", __FILE__, __LINE__);
 * @endcode
 */
typedef void (*orte_errmgr_base_module_log_fn_t)(int error_code, char *filename, int line);


/**
 * Alert - process aborted
 * This function is called when a remote process aborts during execution. Note that local
 * process errors should always be reported through the error_detected interface and
 * NOT here. The function is called when a message is received from the universe daemon
 * indicating that another process in the job failed. For now, this function will
 * simply cause the local process to gracefully finalize and terminate. 
 */
typedef void (*orte_errmgr_base_module_proc_aborted_fn_t)(orte_process_name_t *proc);

/**
 * Alert - incomplete start of a job
 * This function is called when an attempted launch of a job encounters failure of
 * one or more processes to start. The function decides on the strategy for dealing
 * with this "incomplete start" situation - for now, it simply orders the resource
 * manager to terminate the entire job.
 * 
 * This function is only called by the respective process launcher, which is responsible
 * for detecting incomplete starts.
 */
typedef void (*orte_errmgr_base_module_incomplete_start_fn_t)(orte_jobid_t job);

/**
 * Alert - internal error detected
 * This function is called when an internal error is detected within the local process.
 * It decides what to do about the error - for now, it simply orders the local process
 * to finalize and terminate.
 */
typedef void (*orte_errmgr_base_module_error_detected_fn_t)(int error_code);

/*
 * Register a job with the error manager
 * When a job is launched, this function is called so the error manager can register
 * subscriptions on the job segment so that the error manager will be notified when
 * problems occur - i.e., when process status entries change to abnormal termination
 * values. Process status entries are changed by the appropriate state-of-health monitor
 * and/or the process launcher, depending upon the stage at which the problem occurs.
 * 
 * Monitoring of the job begins once the job has reached the "executing" stage. Prior
 * to that time, failure of processes to start are the responsibility of the respective
 * process launcher - which is expected to call the error manager via the "incomplete
 * start" interface to report any problems prior to the job beginning "execution".
 */
typedef int (*orte_errmgr_base_module_register_job_fn_t)(orte_jobid_t job);

/**
 * Alert - self aborting
 * This function is called when a process is aborting. The routine will kill
 * any child processes and terminate the calling process.
 */
typedef void (*orte_errmgr_base_module_abort_fn_t)(void);

/*
 * Ver 1.0.0
 */
struct orte_errmgr_base_module_1_0_0_t {
    orte_errmgr_base_module_log_fn_t log;
    orte_errmgr_base_module_proc_aborted_fn_t proc_aborted;
    orte_errmgr_base_module_incomplete_start_fn_t incomplete_start;
    orte_errmgr_base_module_error_detected_fn_t error_detected;
    orte_errmgr_base_module_register_job_fn_t register_job;
    orte_errmgr_base_module_abort_fn_t abort;
};

typedef struct orte_errmgr_base_module_1_0_0_t orte_errmgr_base_module_1_0_0_t;
typedef orte_errmgr_base_module_1_0_0_t orte_errmgr_base_module_t;

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

struct mca_errmgr_base_component_1_0_0_t {
    mca_base_component_t errmgr_version;
    mca_base_component_data_1_0_0_t errmgr_data;

    orte_errmgr_base_component_init_fn_t errmgr_init;
    orte_errmgr_base_component_finalize_fn_t errmgr_finalize;
};
typedef struct mca_errmgr_base_component_1_0_0_t mca_errmgr_base_component_1_0_0_t;
typedef mca_errmgr_base_component_1_0_0_t mca_errmgr_base_component_t;



/*
 * Macro for use in components that are of type errmgr v1.0.0
 */
#define ORTE_ERRMGR_BASE_VERSION_1_0_0 \
  /* ns v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* errmgr v1.0 */ \
  "errmgr", 1, 0, 0

/* Global structure for accessing error manager functions
 */
OMPI_DECLSPEC extern orte_errmgr_base_module_t orte_errmgr;  /* holds selected module's function pointers */

#endif

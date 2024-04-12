/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/****    PRTE STATE MACHINE    ****/

/* States are treated as events so that the event
 * library can sequence them. Each state consists
 * of an event, a job or process state, a pointer
 * to the respective object, and a callback function
 * to be executed for that state. Events can be defined
 * at different priorities - e.g., SYS priority for
 * events associated with launching jobs, and ERR priority
 * for events associated with abnormal termination of
 * a process.
 *
 * The state machine consists of a list of state objects,
 * each defining a state-cbfunc pair. At startup, a default
 * list is created by the base functions which is then
 * potentially customized by selected components within
 * the various PRTE frameworks. For example, a PLM component
 * may need to insert states in the launch procedure, or may
 * want to redirect a particular state callback to a custom
 * function.
 *
 * For convenience, an ANY state can be defined along with a generic
 * callback function, with the corresponding state object
 * placed at the end of the state machine. Setting the
 * machine to a state that has not been explicitly defined
 * will cause this default action to be executed. Thus, you
 * don't have to explicitly define a state-cbfunc pair
 * for every job or process state.
 */

#ifndef _PRTE_STATE_H_
#define _PRTE_STATE_H_

#include "prte_config.h"

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"
#include "src/pmix/pmix-internal.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/plm/plm_types.h"
#include "src/mca/state/state_types.h"
#include "src/runtime/prte_globals.h"
#include "src/util/error_strings.h"

BEGIN_C_DECLS

/*
 * MCA Framework - put here to access the pmix_output channel
 * in the macros
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_state_base_framework;

#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif

/* For ease in debugging the state machine, it is STRONGLY recommended
 * that the functions be accessed using the following macros
 */

/* Timestamp for state printouts */
#define PRTE_STATE_GET_TIMESTAMP(t)           \
    do {                                      \
        struct timeval tv;                    \
        gettimeofday(&tv, NULL);              \
        t = tv.tv_sec;                        \
        t += (double) tv.tv_usec / 1000000.0; \
    } while (0);

#define PRTE_ACTIVATE_JOB_STATE(j, s)                                                         \
    do {                                                                                      \
        prte_job_t *shadow = (j);                                                             \
        if (prte_state_base_framework.framework_verbose > 0) {                                \
            double timestamp = 0.0;                                                           \
            PRTE_STATE_GET_TIMESTAMP(timestamp);                                              \
            pmix_output_verbose(1, prte_state_base_framework.framework_output,                \
                                "%s [%f] ACTIVATE JOB %s STATE %s AT %s:%d",                  \
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), timestamp,                \
                                (NULL == shadow) ? "NULL" : PRTE_JOBID_PRINT(shadow->nspace), \
                                prte_job_state_to_str((s)), __FILE__, __LINE__);              \
        }                                                                                     \
        prte_state.activate_job_state(shadow, (s));                                           \
    } while (0);

#define PRTE_ACTIVATE_PROC_STATE(p, s)                                               \
    do {                                                                             \
        pmix_proc_t *shadow = (p);                                                   \
        if (prte_state_base_framework.framework_verbose > 0) {                       \
            double timestamp = 0.0;                                                  \
            PRTE_STATE_GET_TIMESTAMP(timestamp);                                     \
            pmix_output_verbose(1, prte_state_base_framework.framework_output,       \
                                "%s [%f] ACTIVATE PROC %s STATE %s AT %s:%d",        \
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), timestamp,       \
                                (NULL == shadow) ? "NULL" : PRTE_NAME_PRINT(shadow), \
                                prte_proc_state_to_str((s)), __FILE__, __LINE__);    \
        }                                                                            \
        prte_state.activate_proc_state(shadow, (s));                                 \
    } while (0);

/* Called when actually arriving (reaching) the state with priority k */
#define PRTE_REACHING_JOB_STATE(j, s)                                                         \
    do {                                                                                      \
        prte_job_t *shadow = (j);                                                             \
        if (prte_state_base_framework.framework_verbose > 0) {                                \
            double timestamp = 0.0;                                                           \
            PRTE_STATE_GET_TIMESTAMP(timestamp);                                              \
            pmix_output_verbose(1, prte_state_base_framework.framework_output,                \
                                "%s [%f] ACTIVATING JOB %s STATE %s",                         \
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), timestamp,                \
                                (NULL == shadow) ? "NULL" : PRTE_JOBID_PRINT(shadow->nspace), \
                                prte_job_state_to_str((s)));                                  \
        }                                                                                     \
    } while (0);

#define PRTE_REACHING_PROC_STATE(p, s)                                               \
    do {                                                                             \
        pmix_proc_t *shadow = (p);                                                   \
        if (prte_state_base_framework.framework_verbose > 0) {                       \
            double timestamp = 0.0;                                                  \
            PRTE_STATE_GET_TIMESTAMP(timestamp);                                     \
            pmix_output_verbose(1, prte_state_base_framework.framework_output,       \
                                "%s [%f] ACTIVATING PROC %s STATE %s",               \
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), timestamp,       \
                                (NULL == shadow) ? "NULL" : PRTE_NAME_PRINT(shadow), \
                                prte_proc_state_to_str((s)));                        \
        }                                                                            \
    } while (0);

/**
 * Module initialization function.
 *
 * @retval PRTE_SUCCESS The operation completed successfully
 * @retval PRTE_ERROR   An unspecifed error occurred
 */
typedef int (*prte_state_base_module_init_fn_t)(void);

/**
 * Module finalization function.
 *
 * @retval PRTE_SUCCESS The operation completed successfully
 * @retval PRTE_ERROR   An unspecifed error occurred
 */
typedef int (*prte_state_base_module_finalize_fn_t)(void);

/****    JOB STATE APIs    ****/
/* Job states are accessed via prte_job_t objects as they are only
 * used in PRTE tools and not application processes. APIs are provided
 * for assembling and editing the state machine, as well as activating
 * a specific job state
 *
 * Note the inherent assumption in this design that any customization
 * of the state machine will at least start with the base states - i.e.,
 * that one would start with the default machine and edit it to add,
 * remove, or modify callbacks as required. Alternatively, one could
 * just clear the list entirely and assemble a fully custom state
 * machine - both models are supported.
 */

/* Activate a state in the job state machine.
 *
 * Creates and activates an event with the callback corresponding to the
 * specified job state. If the specified state is not found:
 *
 * 1. if a state machine entry for PRTE_JOB_STATE_ERROR was given, and
 *    the state is an error state (i.e., PRTE_JOB_STATE_ERROR <= state),
 *    then the callback for the ERROR state will be used
 *
 * 2. if a state machine entry for PRTE_JOB_STATE_ANY was given, and
 *    the state is not an error state (i.e., state < PRTE_JOB_STATE_ERROR),
 *    then the callback for the ANY state will be used
 *
 * 3. if neither of the above is true, then the call will be ignored.
 */
typedef void (*prte_state_base_module_activate_job_state_fn_t)(prte_job_t *jdata,
                                                               prte_job_state_t state);

/* Add a state to the job state machine.
 *
 */
typedef int (*prte_state_base_module_add_job_state_fn_t)(prte_job_state_t state,
                                                         prte_state_cbfunc_t cbfunc);

/* Set the callback function for a state in the job state machine.
 *
 */
typedef int (*prte_state_base_module_set_job_state_callback_fn_t)(prte_job_state_t state,
                                                                  prte_state_cbfunc_t cbfunc);

/* Remove a state from the job state machine.
 *
 */
typedef int (*prte_state_base_module_remove_job_state_fn_t)(prte_job_state_t state);

/****    Proc STATE APIs  ****/
/* Proc states are accessed via pmix_proc_t as the state machine
 * must be available to both application processes and PRTE tools. APIs are
 * providedfor assembling and editing the state machine, as well as activating
 * a specific proc state
 *
 * Note the inherent assumption in this design that any customization
 * of the state machine will at least start with the base states - i.e.,
 * that one would start with the default machine and edit it to add,
 * remove, or modify callbacks as required. Alternatively, one could
 * just clear the list entirely and assemble a fully custom state
 * machine - both models are supported.
 */

/* Activate a proc state.
 *
 * Creates and activates an event with the callback corresponding to the
 * specified proc state. If the specified state is not found:
 *
 * 1. if a state machine entry for PRTE_PROC_STATE_ERROR was given, and
 *    the state is an error state (i.e., PRTE_PROC_STATE_ERROR <= state),
 *    then the callback for the ERROR state will be used
 *
 * 2. if a state machine entry for PRTE_PROC_STATE_ANY was given, and
 *    the state is not an error state (i.e., state < PRTE_PROC_STATE_ERROR),
 *    then the callback for the ANY state will be used
 *
 * 3. if neither of the above is true, then the call will be ignored.
 */
typedef void (*prte_state_base_module_activate_proc_state_fn_t)(pmix_proc_t *proc,
                                                                prte_proc_state_t state);

/* Add a state to the proc state machine.
 *
 */
typedef int (*prte_state_base_module_add_proc_state_fn_t)(prte_proc_state_t state,
                                                          prte_state_cbfunc_t cbfunc);

/* Set the callback function for a state in the proc state machine.
 *
 */
typedef int (*prte_state_base_module_set_proc_state_callback_fn_t)(prte_proc_state_t state,
                                                                   prte_state_cbfunc_t cbfunc);

/* Remove a state from the proc state machine.
 *
 */
typedef int (*prte_state_base_module_remove_proc_state_fn_t)(prte_proc_state_t state);

/*
 * Module Structure
 */
struct prte_state_base_module_1_0_0_t {
    /** Initialization Function */
    prte_state_base_module_init_fn_t init;
    /** Finalization Function */
    prte_state_base_module_finalize_fn_t finalize;
    /* Job state APIs */
    prte_state_base_module_activate_job_state_fn_t activate_job_state;
    prte_state_base_module_add_job_state_fn_t add_job_state;
    prte_state_base_module_set_job_state_callback_fn_t set_job_state_callback;
    prte_state_base_module_remove_job_state_fn_t remove_job_state;
    /* Proc state APIs */
    prte_state_base_module_activate_proc_state_fn_t activate_proc_state;
    prte_state_base_module_add_proc_state_fn_t add_proc_state;
    prte_state_base_module_set_proc_state_callback_fn_t set_proc_state_callback;
    prte_state_base_module_remove_proc_state_fn_t remove_proc_state;
};
typedef struct prte_state_base_module_1_0_0_t prte_state_base_module_1_0_0_t;
typedef prte_state_base_module_1_0_0_t prte_state_base_module_t;
PRTE_EXPORT extern prte_state_base_module_t prte_state;

/*
 * State Component
 */
typedef pmix_mca_base_component_t prte_state_base_component_t;

/*
 * Macro for use in components that are of type state
 */
#define PRTE_STATE_BASE_VERSION_1_0_0 PRTE_MCA_BASE_VERSION_3_0_0("state", 1, 0, 0)

END_C_DECLS
#endif

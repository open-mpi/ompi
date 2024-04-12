/*
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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

#ifndef _PRTE_STATE_TYPES_H_
#define _PRTE_STATE_TYPES_H_

#include "prte_config.h"

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"

#include "src/mca/plm/plm_types.h"
#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

typedef void (*prte_state_cbfunc_t)(int fd, short args, void *cb);

typedef struct {
    pmix_list_item_t super;
    prte_job_state_t job_state;
    prte_proc_state_t proc_state;
    prte_state_cbfunc_t cbfunc;
} prte_state_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_state_t);

/* caddy for passing job and proc data to state event handlers */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    prte_job_t *jdata;
    prte_job_state_t job_state;
    pmix_proc_t name;
    prte_proc_state_t proc_state;
} prte_state_caddy_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_state_caddy_t);

END_C_DECLS
#endif

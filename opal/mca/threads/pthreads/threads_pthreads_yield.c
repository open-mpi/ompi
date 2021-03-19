/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <time.h>
#ifdef HAVE_SCHED_H
#    include <sched.h>
#endif

#include "opal/constants.h"
#include "opal/mca/threads/pthreads/threads_pthreads.h"
#include "opal/mca/threads/thread.h"

static void opal_thread_pthreads_yield_sched_yield(void);
static void opal_thread_pthreads_yield_nanosleep(void);

typedef enum {
    OPAL_PTHREADS_YIELD_SCHED_YIELD = 0,
    OPAL_PTHREADS_YIELD_NANOSLEEP
} opal_threads_pthreads_yield_strategy_t;

static mca_base_var_enum_value_t yield_strategy_values[] = {{OPAL_PTHREADS_YIELD_SCHED_YIELD,
                                                             "sched_yield"},
                                                            {OPAL_PTHREADS_YIELD_NANOSLEEP,
                                                             "nanosleep"},
                                                            {0, NULL}};

/* Number of nanoseconds to nanosleep, if enabled */
static uint64_t yield_nsleep_nanosecs;
/* The time to nanosleep, if enabled */
static struct timespec yield_nsleep_time = {.tv_sec = 0, .tv_nsec = 1};
static opal_threads_pthreads_yield_strategy_t yield_strategy = OPAL_PTHREADS_YIELD_SCHED_YIELD;

opal_threads_pthreads_yield_fn_t *opal_threads_pthreads_yield_fn
    = &opal_thread_pthreads_yield_sched_yield;

int opal_threads_pthreads_yield_init(const mca_base_component_t *component)
{
    mca_base_var_enum_t *yield_strategy_enumerator;
    mca_base_var_enum_create("pthread_yield_strategies", yield_strategy_values,
                             &yield_strategy_enumerator);

    (void) mca_base_component_var_register(component, "yield_strategy",
                                           "Pthread yield strategy to use", MCA_BASE_VAR_TYPE_INT,
                                           yield_strategy_enumerator, 0, 0, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &yield_strategy);
    switch (yield_strategy) {
    case OPAL_PTHREADS_YIELD_NANOSLEEP:
        opal_threads_pthreads_yield_fn = &opal_thread_pthreads_yield_nanosleep;
        break;
    default:
        /* use initial value */
        break;
    }

    OBJ_RELEASE(yield_strategy_enumerator);

    yield_nsleep_nanosecs = (yield_nsleep_time.tv_sec * 1E9) + yield_nsleep_time.tv_nsec;
    (void) mca_base_component_var_register(
        component, "nanosleep_time",
        "Number of nanoseconds to sleep when using nanosleep as the pthread yield strategy",
        MCA_BASE_VAR_TYPE_UINT64_T, NULL, 0, 0, OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL,
        &yield_nsleep_nanosecs);
    yield_nsleep_time.tv_sec = yield_nsleep_nanosecs / 1E9;
    yield_nsleep_time.tv_nsec = yield_nsleep_nanosecs - (uint64_t)(yield_nsleep_time.tv_sec * 1E9);

    return OPAL_SUCCESS;
}

void opal_thread_pthreads_yield_sched_yield(void)
{
#ifdef HAVE_SCHED_H
    sched_yield();
#endif
}

void opal_thread_pthreads_yield_nanosleep(void)
{
    nanosleep(&yield_nsleep_time, NULL);
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- *//*
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
 * Copyright (c) 2006-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Progress engine for Open MPI
 */

#ifndef OPAL_RUNTIME_OPAL_PROGRESS_H
#define OPAL_RUNTIME_OPAL_PROGRESS_H

BEGIN_C_DECLS

#include "opal_config.h"
#include "opal/threads/mutex.h"
#include "opal/runtime/opal_params.h"
#include "opal/mca/timer/base/base.h"

/**
 * Initialize the progress engine
 *
 * Initialize the progress engine, including constructing the
 * proper locks and allocating space for the progress registration
 * functions.  At this point, any function in the progress engine
 * interface may be called.
 */
OPAL_DECLSPEC int opal_progress_init(void);


/**
 * Shut down the progress engine
 *
 * Shut down the progress engine.  This includes deregistering all
 * registered callbacks and freeing all resources.  After finalize
 * returns, no calls into the progress interface are allowed.
 */
OPAL_DECLSPEC int opal_progress_finalize(void);


/**
 * Progress all pending events
 *
 * Progress all pending events.  All registered event handlers will be
 * called every call into opal_progress().  The event library will be
 * called if opal_progress_event_users is greater than 0 (adjustments
 * can be made by calling opal_progress_event_users_add() and
 * opal_progress_event_users_delete()) or the time since the last call
 * into the event library is greater than the progress tick rate (by
 * default, 10ms).
 */
OPAL_DECLSPEC void opal_progress(void);


/**
 * Control how the event library is called
 *
 * Adjust the flags argument used to call opal_event_loop() from
 * opal_progress().  The default argument is OPAL_EVLOOP_ONELOOP,
 * meaning that the call to opal_event_loop() will block pending
 * events, but may block for a period of time.
 *
 * @param flags     One of the valid vlags argument to
 *                  opal_event_loop().
 * @return          Previous value of flags used to call
 *                  opal_event_loop().
 */
OPAL_DECLSPEC int opal_progress_set_event_flag(int flags);


/**
 * Increase the number of users of the event library
 *
 * Increase the number of users of the event library.  This count is
 * used by opal_progress to determine if opal_event_loop() should be
 * called every call to opal_progress() or only after a time has
 * elapsed since the last call (by default, 10ms).  The count defaults
 * to 0, meaning that opal_progress_event_users_increment() must be
 * called at least once for the event loop to be called on every entry
 * to opal_progress().
 *
 */
OPAL_DECLSPEC void opal_progress_event_users_increment(void);


/**
 * Decrease the number of users of the event library
 *
 * Decrease the number of users of the event library.  This count is
 * used by opal_progress to determine if opal_event_loop() should be
 * called every call to opal_progress() or only after a time has
 * elapsed since the last call (by default, 10ms).
 */
OPAL_DECLSPEC void opal_progress_event_users_decrement(void);


/**
 * Set whether opal_progress() should yield when idle
 *
 * Set whether opal_progress() should yield the processor (either by
 * sched_yield() or SwitchToThread()) if no events were progressed
 * during the progress loop.  The return value of the callback
 * functions is used to determine whether or not yielding is required.
 * By default, the event loop will yield when the progress function is
 * idle.
 *
 * @param   yieldopt  Whether to yield when idle.
 * @return         Previous value of the yield_when_idle option.
 */
OPAL_DECLSPEC bool opal_progress_set_yield_when_idle(bool yieldopt);


/**
 * Set time between calls into the event library
 *
 * Set time between calls into the event library when there are no
 * users of the event library (set by
 * opal_progress_event_users_increment() and
 * opal_progress_event_users_decrement()).
 *
 * @param   polltime  Time (in microseconds) between calls to the event
 *                    library
 */
OPAL_DECLSPEC void opal_progress_set_event_poll_rate(int microseconds);


/**
 * Progress callback function typedef
 *
 * Prototype for the a progress function callback.  Progress function
 * callbacks can be registered with opal_progress_register() and
 * deregistered with opal_progress_deregister().  It should be noted
 * that either registering or deregistering a function callback is an
 * extraordinarily expensive operation and should not be used for
 * potentially short callback lifetimes.
 *
 * @return         Number of events progressed during the callback
 */
typedef int (*opal_progress_callback_t)(void);


/**
 * Register an event to be progressed
 *
 * Register an event to be progressed during calls to opal_progress().
 * Please read the note in opal_progress_callback_t.
 */
OPAL_DECLSPEC int opal_progress_register(opal_progress_callback_t cb);

OPAL_DECLSPEC int opal_progress_register_lp (opal_progress_callback_t cb);


/**
 * Deregister previously registered event
 *
 * Deregister an event to be progressed during calls to opal_progress().
 * Please read the note in opal_progress_callback_t.
 */
OPAL_DECLSPEC int opal_progress_unregister(opal_progress_callback_t cb);


OPAL_DECLSPEC extern int opal_progress_spin_count;

/* do we want to call sched_yield() if nothing happened */
OPAL_DECLSPEC extern bool opal_progress_yield_when_idle;

/**
 * Progress until flag is true or poll iterations completed
 */
static inline bool opal_progress_spin(volatile bool* complete)
{
    int32_t c;

    for (c = 0; c < opal_progress_spin_count; c++) {
        if (true == *complete) {
             return true;
        }
        opal_progress();
    }

    return false;
}

/**
 * typedef of callback functions which are called when hang-up is detected
 *
 * A typical usage is to output hang-up situation information for debugging.
 *
 * @param file   File stream to output hang-up situation information into.
 * @param prefix Desired prefix for each line of hang-up situation information.
 * @param cbdata Callback data passed with the function pointer.
 */
typedef void (*opal_progress_hangup_callback_fn_t)(FILE *file, char *prefix,
                                                   void *cbdata);

/**
 * How many seconds we wait in opal_progress loop (OPAL_PROGRESS_BLOCK_WHILE)
 * for the timeout-based hang-up detection feature
 *
 * The absolute value is a time in seconds to wait a completion of an operation.
 * If zero, the timeout-based hang-up detection feature is disabled.
 * If positive, the process prints a message and aborts on the timeout.
 * If negative, the process prints a message and continues on the timeout.
 */
OPAL_DECLSPEC extern int opal_progress_timeout;

/**
 * Handle a hang-up situation.
 */
OPAL_DECLSPEC
void opal_progress_handle_hangup(opal_progress_hangup_callback_fn_t cbfunc,
                                 void *cbdata);

/**
 * Call the opal_progress function in while-loop
 *
 * @param while_condition_ Expression used as the condition of the while-loop.
 * @param detect_hangup_   Whether to detect hang-up if opal_progress_timeout
 *                         is nonzero. (bool)
 * @param hangup_cbfunc_   Function called when when hang-up is detected.
 *                         (opal_progress_hangup_callback_fn_t)
 * @param hangup_cbdata_   Data passed as an argument of hangup_cbfunc_.
 *                         (void *)
 * @param progress_block_  Content of the while-loop. The opal_progress
 *                         function should be called in this block.
 *                         (C block enclosed with '{' and '}')
 */
#define OPAL_PROGRESS_BLOCK_WHILE(while_condition_, detect_hangup_,          \
                                  hangup_cbfunc_, hangup_cbdata_,            \
                                  progress_block_)                           \
    if (OPAL_UNLIKELY((detect_hangup_) && opal_progress_timeout != 0)) {     \
        bool once_detected_ = false;                                         \
        opal_timer_t limit_ = opal_timer_base_get_usec() +                   \
                             abs(opal_progress_timeout) * 1000000;           \
        while (while_condition_) {                                           \
            progress_block_;                                                 \
            if (OPAL_UNLIKELY(! once_detected_ &&                            \
                              opal_timer_base_get_usec() >= limit_)) {       \
                opal_progress_handle_hangup(hangup_cbfunc_, hangup_cbdata_); \
                once_detected_ = true;                                       \
            }                                                                \
        }                                                                    \
    } else {                                                                 \
        while (while_condition_) progress_block_                             \
    }

/**
 * Call the opal_progress function in while-loop
 *
 * See comment of OPAL_PROGRESS_BLOCK_WHILE.
 */
#define OPAL_PROGRESS_WHILE(while_condition_, detect_hangup_,                \
                            hangup_cbfunc_, hangup_cbdata_)                  \
    OPAL_PROGRESS_BLOCK_WHILE(while_condition_, detect_hangup_,              \
                              hangup_cbfunc_, hangup_cbdata_,                \
                              { opal_progress(); })

END_C_DECLS

#endif


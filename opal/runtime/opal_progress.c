/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2020 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/timer/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_params.h"
#include "opal/runtime/opal_progress.h"
#include "opal/util/event.h"
#include "opal/util/output.h"

#define OPAL_PROGRESS_USE_TIMERS       (OPAL_TIMER_CYCLE_SUPPORTED || OPAL_TIMER_USEC_SUPPORTED)
#define OPAL_PROGRESS_ONLY_USEC_NATIVE (OPAL_TIMER_USEC_NATIVE && !OPAL_TIMER_CYCLE_NATIVE)

#if OPAL_ENABLE_DEBUG
bool opal_progress_debug = false;
#endif

/*
 * default parameters
 */
static int opal_progress_event_flag = OPAL_EVLOOP_ONCE | OPAL_EVLOOP_NONBLOCK;
int opal_progress_spin_count = 10000;

#if OPAL_ENABLE_PROGRESS_THREADS == 1
/* MCA OPAL parameter */
extern bool opal_async_progress;

/* Track whether the async progress thread was successfully spawned.
 * This boolean is set at init, and read at finalize to join the
 * progress thread */
bool opal_async_progress_thread_spawned = false;

/* async progress thread info & args */
typedef struct thread_args_s {
    /* number of events reported.
     * This is updated by the async progress thread, to be read and resetted
     * by the application threads */
    opal_atomic_int64_t nb_events_reported;

    /* should continue running ? */
    volatile bool running;
} thread_args_t;

/* async progress thread routine */
static void *opal_progress_async_thread_engine(opal_object_t *obj);

static opal_thread_t opal_progress_async_thread;
static thread_args_t thread_arg = {
    .nb_events_reported = 0,
    .running = false
};
#endif

/*
 * Local variables
 */
static opal_atomic_lock_t progress_lock;

/* callbacks to progress
 *
 * Callbacks are stored in a 2x2 table indexed by
 *   [thread-safety class][priority]
 *
 * "SAFE" callbacks were explicitly registered via
 * opal_progress_register_thread_safe(..., true) by a component
 * maintainer who audited that the callback may be invoked concurrently
 * with opal_progress() running on another thread (i.e. from the async
 * progress thread). Anything registered via the original
 * opal_progress_register()/_lp() API -- i.e. unaudited -- is treated
 * as "UNSAFE" by default (fail closed).
 *
 * Only the OPAL_THREAD_SAFE row is ever progressed by the async
 * progress thread. The OPAL_THREAD_UNSAFE row is progressed
 * synchronously by whichever application thread calls opal_progress(),
 * even while the async thread is running, since nothing else will ever
 * call them. */
typedef enum {
    OPAL_THREAD_SAFE = 0,
    OPAL_THREAD_UNSAFE = 1,
    OPAL_THREAD_SAFETY_MAX
} opal_progress_thread_safety_t;

typedef enum {
    OPAL_PRIORITY_NORMAL = 0,
    OPAL_PRIORITY_LOW = 1,
    OPAL_PRIORITY_MAX
} opal_progress_priority_t;

typedef struct {
    volatile opal_progress_callback_t *cbs;
    size_t len;
    size_t size;
} opal_progress_cb_array_t;

static opal_progress_cb_array_t callbacks[OPAL_THREAD_SAFETY_MAX][OPAL_PRIORITY_MAX];

/* do we want to yield() if nothing happened */
bool opal_progress_yield_when_idle = false;
#if OPAL_PROGRESS_USE_TIMERS
static opal_timer_t event_progress_last_time = 0;
static opal_timer_t event_progress_delta = 0;
#else
/* current count down until we tick the event library */
static opal_atomic_int32_t event_progress_counter = 0;
/* reset value for counter when it hits 0 */
static int32_t event_progress_delta = 0;
#endif
/* users of the event library from MPI cause the tick rate to
   be every time */
static opal_atomic_int32_t num_event_users = 0;

#if OPAL_ENABLE_DEBUG
static int debug_output = -1;
#endif

/**
 * Fake callback used for threading purposes when one thread
 * progresses callbacks while another unregisters some. The root
 * of the problem is that we allow modifications of the callback
 * array directly from the callbacks themselves. Now if
 * writing a pointer is atomic, we should not have any more
 * problems.
 */
static int fake_cb(void)
{
    return 0;
}

static int _opal_progress_unregister(opal_progress_callback_t cb,
                                     volatile opal_progress_callback_t *callback_array,
                                     size_t *callback_array_len);

static void opal_progress_finalize(void)
{
    /* free memory associated with the callbacks */
    opal_atomic_lock(&progress_lock);

    for (int t = 0; t < OPAL_THREAD_SAFETY_MAX; ++t) {
        for (int p = 0; p < OPAL_PRIORITY_MAX; ++p) {
            callbacks[t][p].len = 0;
            callbacks[t][p].size = 0;
            free((void *) callbacks[t][p].cbs);
            callbacks[t][p].cbs = NULL;
        }
    }

    opal_atomic_unlock(&progress_lock);
}

void opal_progress_shutdown_async_progress_thread(void)
{
#if OPAL_ENABLE_PROGRESS_THREADS == 1
    if (opal_async_progress_thread_spawned) {
        /* shutdown the async thread */
        thread_arg.running = false;
        int err = opal_thread_join(&opal_progress_async_thread, NULL);
        if (OPAL_SUCCESS == err) {
            opal_set_using_threads(false);
            opal_async_progress_thread_spawned = false;
        } else {
            OPAL_OUTPUT(
                (debug_output, "progress: Failed to join async progress thread: err=%d", err));
        }
    }
#endif
}

/* init the progress engine - called from orte_init */
int opal_progress_init(void)
{
    int err = OPAL_SUCCESS;

    /* reentrant issues */
    opal_atomic_lock_init(&progress_lock, OPAL_ATOMIC_LOCK_UNLOCKED);

    /* set the event tick rate */
    opal_progress_set_event_poll_rate(10000);

#if OPAL_ENABLE_DEBUG
    if (opal_progress_debug) {
        debug_output = opal_output_open(NULL);
    }
#endif

    for (int t = 0; t < OPAL_THREAD_SAFETY_MAX; ++t) {
        for (int p = 0; p < OPAL_PRIORITY_MAX; ++p) {
            callbacks[t][p].size = 8;
            callbacks[t][p].len = 0;
            callbacks[t][p].cbs = malloc(callbacks[t][p].size * sizeof(callbacks[t][p].cbs[0]));

            if (NULL == callbacks[t][p].cbs) {
                /* roll back everything allocated until now */
                for (int tt = 0; tt <= t; ++tt) {
                    for (int pp = 0; pp < (tt == t ? p : OPAL_PRIORITY_MAX); ++pp) {
                        free((void *) callbacks[tt][pp].cbs);
                        callbacks[tt][pp].cbs = NULL;
                        callbacks[tt][pp].size = 0;
                        callbacks[tt][pp].len = 0;
                    }
                }
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            for (size_t i = 0; i < callbacks[t][p].size; ++i) {
                callbacks[t][p].cbs[i] = fake_cb;
            }
        }
    }

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    if (opal_async_progress && !opal_async_progress_thread_spawned) {
        /* prepare the thread */
        thread_arg.nb_events_reported = 0;
        thread_arg.running = true;
        OBJ_CONSTRUCT(&opal_progress_async_thread, opal_thread_t);
        opal_progress_async_thread.t_run = opal_progress_async_thread_engine;
        opal_progress_async_thread.t_arg = &thread_arg;

        /* optimistic setting for asynchronism here, but we know that these might
        still be changed by other *init() routines :( */
        opal_progress_set_yield_when_idle(true);
        opal_progress_set_event_flag(opal_progress_event_flag | OPAL_EVLOOP_NONBLOCK);

        err = opal_thread_start(&opal_progress_async_thread);
        if (OPAL_SUCCESS == err) {
            opal_set_using_threads(true);
            opal_async_progress_thread_spawned = true;
        } else {
            thread_arg.running = false;
            OPAL_OUTPUT(
                (debug_output, "progress: Failed to start async progress thread: err=%d", err));
        }
    }
#endif

    OPAL_OUTPUT(
        (debug_output, "progress: initialized event flag to: %x", opal_progress_event_flag));
    OPAL_OUTPUT((debug_output, "progress: initialized yield_when_idle to: %s",
                 opal_progress_yield_when_idle ? "true" : "false"));
    OPAL_OUTPUT((debug_output, "progress: initialized num users to: %d", num_event_users));
    OPAL_OUTPUT(
        (debug_output, "progress: initialized poll rate to: %ld", (long) event_progress_delta));

    opal_finalize_register_cleanup(opal_progress_finalize);

    return err;
}

static int opal_progress_events(void)
{
    static opal_atomic_int32_t lock = 0;
    int events = 0;

    if (opal_progress_event_flag != 0 && !OPAL_THREAD_SWAP_32(&lock, 1)) {
#if OPAL_PROGRESS_USE_TIMERS
#    if OPAL_PROGRESS_ONLY_USEC_NATIVE
        opal_timer_t now = opal_timer_base_get_usec();
#    else
        opal_timer_t now = opal_timer_base_get_cycles();
#    endif /* OPAL_PROGRESS_ONLY_USEC_NATIVE */
        /* trip the event library if we've reached our tick rate and we are
           enabled */
        if (now - event_progress_last_time > event_progress_delta) {
            event_progress_last_time = (num_event_users > 0) ? now - event_progress_delta : now;

            events += opal_event_loop(opal_sync_event_base, opal_progress_event_flag);
        }

#else /* OPAL_PROGRESS_USE_TIMERS */
        /* trip the event library if we've reached our tick rate and we are
           enabled */
        if (OPAL_THREAD_ADD_FETCH32(&event_progress_counter, -1) <= 0) {
            event_progress_counter = (num_event_users > 0) ? 0 : event_progress_delta;
            events += opal_event_loop(opal_sync_event_base, opal_progress_event_flag);
        }
#endif /* OPAL_PROGRESS_USE_TIMERS */
        lock = 0;
    }

    return events;
}

/*
 * Invoke every callback registered in callbacks[safety][prio] and return
 * the number of events they reported.
 */
static inline int _opal_progress_callbacks_run(opal_progress_thread_safety_t safety,
                                               opal_progress_priority_t prio)
{
    const opal_progress_cb_array_t *arr = &callbacks[safety][prio];
    int events = 0;

    for (size_t i = 0; i < arr->len; ++i) {
        events += (arr->cbs[i])();
    }

    return events;
}

/*
 * Progress the event library and any functions that have registered to
 * be called.  We don't propagate errors from the progress functions,
 * so no action is taken if they return failures.  The functions are
 * expected to return the number of events progressed, to determine
 * whether or not we should yield the CPU during MPI progress.
 * This is only loosely tracked, as an error return can cause the number
 * of progressed events to appear lower than it actually is.  We don't
 * care, as the cost of that happening is far outweighed by the cost
 * of the if checks (they were resulting in bad pipe stalling behavior)
 *
 * This variant progresses EVERY registered callback, safe and unsafe
 * alike, and is used only when no async progress thread is running --
 * i.e. there is only ever one thread in here, so the safe/unsafe split
 * is irrelevant and we fall back to the historical single-array
 * behavior.
 */
static int _opal_progress_full(void)
{
    static uint32_t num_calls_full = 0;
    int events = 0;

    /* progress all registered callbacks, regardless of thread-safety class */
    events += _opal_progress_callbacks_run(OPAL_THREAD_SAFE, OPAL_PRIORITY_NORMAL);
    events += _opal_progress_callbacks_run(OPAL_THREAD_UNSAFE, OPAL_PRIORITY_NORMAL);

    /* Run low priority callbacks and events once every <N> calls to opal_progress().
     * Even though "num_calls_full" can be modified by multiple threads, we do not use
     * atomic operations here, for performance reasons. In case of a race, the
     * number of calls may be inaccurate, but since it will eventually be incremented,
     * it's not a problem.
     * No async progress thread here, so N = 8, as before.
     */
    if (((num_calls_full++) & 0x7) == 0) {
        events += _opal_progress_callbacks_run(OPAL_THREAD_SAFE, OPAL_PRIORITY_LOW);
        events += _opal_progress_callbacks_run(OPAL_THREAD_UNSAFE, OPAL_PRIORITY_LOW);

        opal_progress_events();
    } else if (num_event_users > 0) {
        opal_progress_events();
    }

    if (opal_progress_yield_when_idle && events <= 0) {
        /* If there is nothing to do - yield the processor - otherwise
         * we could consume the processor for the entire time slice. If
         * the processor is oversubscribed - this will result in a best-case
         * latency equivalent to the time-slice.
         * With some thread implementations, yielding might be required
         * to ensure correct scheduling of all communicating threads.
         */
        opal_thread_yield();
    }

    return events;
}

#if OPAL_ENABLE_PROGRESS_THREADS == 1
/*
 * Progress ONLY the callbacks explicitly declared thread-safe. This is
 * the sole function invoked by the async progress thread, and therefore
 * the sole caller of opal_progress_events() (the libevent tick) once
 * the async thread is running: opal_sync_event_base is not safe to
 * drive from two threads concurrently, so _opal_progress_main_unsafe()
 * below must never touch it while this thread is alive.
 */
static int _opal_progress_async_safe(void)
{
    static uint32_t num_calls_safe = 0;
    int events = 0;

    events += _opal_progress_callbacks_run(OPAL_THREAD_SAFE, OPAL_PRIORITY_NORMAL);

    /* N = 256 for the async thread tick rate (George's recommendation);
     * adapt later if it takes too many resources. */
    if (((num_calls_safe++) & 0xFF) == 0) {
        events += _opal_progress_callbacks_run(OPAL_THREAD_SAFE, OPAL_PRIORITY_LOW);

        opal_progress_events();
    } else if (num_event_users > 0) {
        opal_progress_events();
    }

    return events;
}

/*
 * Progress ONLY the callbacks NOT declared thread-safe (either
 * explicitly registered unsafe, or registered via the legacy
 * opal_progress_register()/_lp() API and therefore unaudited). Called
 * from opal_progress() by whichever application thread invokes it,
 * even while the async progress thread is running, since these
 * callbacks are never touched by that thread and nothing else will
 * ever progress them.
 *
 * Deliberately does NOT call opal_progress_events(): while the async
 * thread is running it is the sole, exclusive owner of the event
 * library tick (see _opal_progress_async_safe() above).
 */
static int _opal_progress_main_unsafe(void)
{
    static uint32_t num_calls_unsafe = 0;
    int events = 0;

    events += _opal_progress_callbacks_run(OPAL_THREAD_UNSAFE, OPAL_PRIORITY_NORMAL);

    if (((num_calls_unsafe++) & 0x7) == 0) {
        events += _opal_progress_callbacks_run(OPAL_THREAD_UNSAFE, OPAL_PRIORITY_LOW);
    }

    return events;
}

/*
 * OPAL async progress thread to execute safe callbacks
 */
static void *opal_progress_async_thread_engine(opal_object_t *obj)
{
    opal_thread_t *current_thread = (opal_thread_t *) obj;
    thread_args_t *p_thread_arg = (thread_args_t *) current_thread->t_arg;

    while (p_thread_arg->running) {
        const int64_t new_events = _opal_progress_async_safe();
        if (new_events > 0) {
            opal_atomic_add_fetch_64(&p_thread_arg->nb_events_reported, new_events);
        }
    }

    return OPAL_THREAD_CANCELLED;
}
#endif

int opal_progress(void)
{
#if OPAL_ENABLE_PROGRESS_THREADS == 1
    if (opal_async_progress_thread_spawned) {
        /* async progress thread alongside may has processed new events,
         * atomically read and reset nb_events_reported to zero.
         */
        const int64_t new_events = opal_atomic_swap_64(&thread_arg.nb_events_reported, 0);

        /* Callbacks registered as NOT thread-safe are never touched by
         * the async thread -- progress them synchronously here, on
         * whichever thread called opal_progress(), or they would never
         * make progress at all. */
        const int unsafe_events = _opal_progress_main_unsafe();

        /* if no new event at all (async-reported or unsafe-local), then
         * application thread may yield here */
        if (opal_progress_yield_when_idle && new_events <= 0 && unsafe_events <= 0) {
            opal_thread_yield();
        }
        return (int) new_events + unsafe_events;
    } else {
#endif

    /* no async progress thread, call the normal progress routine like before */
    return _opal_progress_full();

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    }
#endif
}

int opal_progress_set_event_flag(int flag)
{
    int tmp = opal_progress_event_flag;
    opal_progress_event_flag = flag;

    OPAL_OUTPUT((debug_output, "progress: set_event_flag setting to %d", flag));

    return tmp;
}

void opal_progress_event_users_increment(void)
{
#if OPAL_ENABLE_DEBUG
    int32_t val;
    val = opal_atomic_add_fetch_32(&num_event_users, 1);

    OPAL_OUTPUT((debug_output, "progress: event_users_increment setting count to %d", val));
#else
    (void) opal_atomic_add_fetch_32(&num_event_users, 1);
#endif

#if OPAL_PROGRESS_USE_TIMERS
    /* force an update next round (we'll be past the delta) */
    event_progress_last_time -= event_progress_delta;
#else
    /* always reset the tick rate - can't hurt */
    event_progress_counter = 0;
#endif
}

void opal_progress_event_users_decrement(void)
{
#if OPAL_ENABLE_DEBUG || !OPAL_PROGRESS_USE_TIMERS
    int32_t val;
    val = opal_atomic_sub_fetch_32(&num_event_users, 1);

    OPAL_OUTPUT((debug_output, "progress: event_users_decrement setting count to %d", val));
#else
    (void) opal_atomic_sub_fetch_32(&num_event_users, 1);
#endif

#if !OPAL_PROGRESS_USE_TIMERS
    /* start now in delaying if it's easy */
    if (val >= 0) {
        event_progress_counter = event_progress_delta;
    }
#endif
}

bool opal_progress_set_yield_when_idle(bool yieldopt)
{
    bool tmp = opal_progress_yield_when_idle;
    opal_progress_yield_when_idle = (yieldopt) ? 1 : 0;

    OPAL_OUTPUT((debug_output, "progress: progress_set_yield_when_idle to %s",
                 opal_progress_yield_when_idle ? "true" : "false"));

    return tmp;
}

void opal_progress_set_event_poll_rate(int polltime)
{
    OPAL_OUTPUT((debug_output, "progress: progress_set_event_poll_rate(%d)", polltime));

#if OPAL_PROGRESS_USE_TIMERS
    event_progress_delta = 0;
#    if OPAL_PROGRESS_ONLY_USEC_NATIVE
    event_progress_last_time = opal_timer_base_get_usec();
#    else
    event_progress_last_time = opal_timer_base_get_cycles();
#    endif
#else
    event_progress_counter = event_progress_delta = 0;
#endif

    if (polltime == 0) {
#if OPAL_PROGRESS_USE_TIMERS
        /* user specified as never tick - tick once per minute */
        event_progress_delta = 60 * 1000000;
#else
        /* user specified as never tick - don't count often */
        event_progress_delta = INT_MAX;
#endif
    } else {
#if OPAL_PROGRESS_USE_TIMERS
        event_progress_delta = polltime;
#else
        /* subtract one so that we can do post-fix subtraction
           in the inner loop and go faster */
        event_progress_delta = polltime - 1;
#endif
    }

#if OPAL_PROGRESS_USE_TIMERS && !OPAL_PROGRESS_ONLY_USEC_NATIVE
    /*  going to use cycles for counter.  Adjust specified usec into cycles */
    event_progress_delta = event_progress_delta * opal_timer_base_get_freq() / 1000000;
#endif
}

static int opal_progress_find_cb(opal_progress_callback_t cb,
                                 volatile opal_progress_callback_t *cbs, size_t cbs_len)
{
    for (size_t i = 0; i < cbs_len; ++i) {
        if (cbs[i] == cb) {
            return (int) i;
        }
    }

    return OPAL_ERR_NOT_FOUND;
}

static int _opal_progress_register(opal_progress_callback_t cb,
                                   volatile opal_progress_callback_t **cbs, size_t *cbs_size,
                                   size_t *cbs_len)
{
    int ret = OPAL_SUCCESS;

    if (OPAL_ERR_NOT_FOUND != opal_progress_find_cb(cb, *cbs, *cbs_len)) {
        return OPAL_SUCCESS;
    }

    /* see if we need to allocate more space */
    if (*cbs_len + 1 > *cbs_size) {
        opal_progress_callback_t *tmp, *old;

        tmp = (opal_progress_callback_t *) malloc(sizeof(tmp[0]) * 2 * *cbs_size);
        if (tmp == NULL) {
            return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        }

        if (*cbs) {
            /* copy old callbacks */
            memcpy(tmp, (void *) *cbs, sizeof(tmp[0]) * *cbs_size);
        }

        for (size_t i = *cbs_len; i < 2 * *cbs_size; ++i) {
            tmp[i] = fake_cb;
        }

        opal_atomic_wmb();

        /* swap out callback array */
        old = (opal_progress_callback_t *) opal_atomic_swap_ptr((opal_atomic_intptr_t *) cbs,
                                                                (intptr_t) tmp);

        opal_atomic_wmb();

        free(old);
        *cbs_size *= 2;
    }

    cbs[0][*cbs_len] = cb;
    ++*cbs_len;

    opal_atomic_wmb();

    return ret;
}

/*
 * Register cb in callbacks[safety][prio], first removing it from any
 * other slot of the table so a callback lives in exactly one slot.
 */
static int _opal_progress_register_in(opal_progress_callback_t cb,
                                      opal_progress_thread_safety_t safety,
                                      opal_progress_priority_t prio)
{
    int ret;

    opal_atomic_lock(&progress_lock);

    for (int t = 0; t < OPAL_THREAD_SAFETY_MAX; ++t) {
        for (int p = 0; p < OPAL_PRIORITY_MAX; ++p) {
            if (t == (int) safety && p == (int) prio) {
                continue;
            }
            (void) _opal_progress_unregister(cb, callbacks[t][p].cbs, &callbacks[t][p].len);
        }
    }

    ret = _opal_progress_register(cb, &callbacks[safety][prio].cbs,
                                  &callbacks[safety][prio].size,
                                  &callbacks[safety][prio].len);

    opal_atomic_unlock(&progress_lock);

    return ret;
}

int opal_progress_register_thread_safe(opal_progress_callback_t cb, bool is_thread_safe)
{
    return _opal_progress_register_in(cb, is_thread_safe ? OPAL_THREAD_SAFE : OPAL_THREAD_UNSAFE,
                                      OPAL_PRIORITY_NORMAL);
}

int opal_progress_register_thread_safe_lp(opal_progress_callback_t cb, bool is_thread_safe)
{
    return _opal_progress_register_in(cb, is_thread_safe ? OPAL_THREAD_SAFE : OPAL_THREAD_UNSAFE,
                                      OPAL_PRIORITY_LOW);
}

int opal_progress_register(opal_progress_callback_t cb)
{
    /* By default, callbacks are treated as NOT thread-safe
     * until a maintainer explicitly audits it and switches
     * the component to call opal_progress_register_thread_safe().
     * */
    return opal_progress_register_thread_safe(cb, false);
}

int opal_progress_register_lp(opal_progress_callback_t cb)
{
    /* same reason as opal_progress_register() above */
    return opal_progress_register_thread_safe_lp(cb, false);
}

static int _opal_progress_unregister(opal_progress_callback_t cb,
                                     volatile opal_progress_callback_t *callback_array,
                                     size_t *callback_array_len)
{
    size_t cb_len = *callback_array_len;
    int ret = opal_progress_find_cb(cb, callback_array, cb_len);
    if (OPAL_ERR_NOT_FOUND == ret) {
        return ret;
    }

    /* If we found the function we're unregistering: If callbacks_len
       is 0, we're not going to do anything interesting anyway, so
       skip.  If callbacks_len is 1, it will soon be 0, so no need to
       do any repacking. */
    for (size_t i = (size_t) ret; i < cb_len - 1; ++i) {
        /* copy callbacks atomically since another thread may be in
         * opal_progress(). */
        (void) opal_atomic_swap_ptr((opal_atomic_intptr_t *) (callback_array + i),
                                    (intptr_t) callback_array[i + 1]);
    }

    --cb_len;
    *callback_array_len = cb_len;
    callback_array[cb_len] = fake_cb;

    return OPAL_SUCCESS;
}

int opal_progress_unregister(opal_progress_callback_t cb)
{
    int ret = OPAL_ERR_NOT_FOUND;

    opal_atomic_lock(&progress_lock);

    /* a callback will never be in more than one slot of the table */
    for (int t = 0; t < OPAL_THREAD_SAFETY_MAX && OPAL_SUCCESS != ret; ++t) {
        for (int p = 0; p < OPAL_PRIORITY_MAX && OPAL_SUCCESS != ret; ++p) {
            ret = _opal_progress_unregister(cb, callbacks[t][p].cbs, &callbacks[t][p].len);
        }
    }

    opal_atomic_unlock(&progress_lock);
    return ret;
}

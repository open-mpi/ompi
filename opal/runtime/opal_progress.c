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

#include "opal_config.h"

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#include "opal/runtime/opal_progress.h"
#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/constants.h"
#include "opal/mca/timer/base/base.h"

#define OPAL_PROGRESS_USE_TIMERS (OPAL_TIMER_CYCLE_SUPPORTED || OPAL_TIMER_USEC_SUPPORTED)

/* 
 * default parameters 
 */
static int opal_progress_event_flag = OPAL_EVLOOP_ONELOOP;
#if OPAL_PROGRESS_USE_TIMERS
static const opal_timer_t opal_progress_default_tick_rate = 10000; /* 10ms */
#else
static const int opal_progress_default_tick_rate = 10000; /* 10k calls to opal_progress */
#endif

volatile int32_t opal_progress_thread_count = 0;
int opal_progress_spin_count = 10000;
                                                                                                          


/*
 * Local variables
 */
#if OMPI_HAVE_THREAD_SUPPORT
static opal_atomic_lock_t progress_lock;
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

/* callbacks to progress */
static opal_progress_callback_t *callbacks = NULL;
static size_t callbacks_len = 0;
static size_t callbacks_size = 0;

/* do we want to call sched_yield() if nothing happened */
static int call_yield = 1;

#if OPAL_PROGRESS_USE_TIMERS
static opal_timer_t event_progress_last_time = 0;
static opal_timer_t event_progress_delta = 0;
#else
/* current count down until we tick the event library */
static int32_t event_progress_counter = 0;
/* reset value for counter when it hits 0 */
static int32_t event_progress_delta = 0;
#endif
/* users of the event library from MPI cause the tick rate to 
   be every time */
static int32_t event_num_mpi_users = 0;


/* init the progress engine - called from orte_init */
int
opal_progress_init(void)
{
    /* reentrant issues */
#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_init(&progress_lock, OPAL_ATOMIC_UNLOCKED);
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

    /* always call sched yield when in the rte only... */
    call_yield = 1;

#if OPAL_PROGRESS_USE_TIMERS
    event_progress_delta = 0;
#if OPAL_TIMER_USEC_NATIVE
    event_progress_last_time = opal_timer_base_get_usec();
#else
    event_progress_last_time = opal_timer_base_get_cycles();
#endif
#else
    event_progress_counter = event_progress_delta = 0;
#endif

    return OPAL_SUCCESS;
}


int
opal_progress_mpi_init(void)
{
    event_num_mpi_users = 0;

    return OPAL_SUCCESS;
}

/* turn on MPI optimizations */
int
opal_progress_mpi_enable(void)
{
    int param, value;

    /* call sched yield when oversubscribed. */
    param = mca_base_param_find("mpi", NULL, "yield_when_idle");
    mca_base_param_lookup_int(param, &value);

    if (value < 0) {
        /* this should never happen set to 1 if it somehow does */
        call_yield = 1;
    } else {
        call_yield = value;
    }

    /* set the event tick rate */
    param = mca_base_param_find("mpi", NULL, "event_tick_rate");
    mca_base_param_lookup_int(param, &value);

    if (value < 0) {
        /* user didn't specify - default tick rate */
        event_progress_delta = opal_progress_default_tick_rate;
    } else if (value == 0) {
#if OPAL_PROGRESS_USE_TIMERS
        /* user specified as never tick - tick once per minute */
        event_progress_delta = 60 * 1000000;
#else
        /* user specified as never tick - don't count often */
        event_progress_delta = INT_MAX;
#endif
    } else {
#if OPAL_PROGRESS_USE_TIMERS
        event_progress_delta = value;
#else
        /* subtract one so that we can do post-fix subtraction
           in the inner loop and go faster */
        event_progress_delta = value - 1;
#endif
    }
#if OPAL_PROGRESS_USE_TIMERS && !OPAL_TIMER_USEC_NATIVE
    /*  going to use cycles for counter.  Adjust specified usec into cycles */
    event_progress_delta = event_progress_delta * opal_timer_base_get_freq() / 1000000;
#endif

#if OPAL_PROGRESS_USE_TIMERS
#if OPAL_TIMER_USEC_NATIVE
    event_progress_last_time = opal_timer_base_get_usec();
#else
    event_progress_last_time = opal_timer_base_get_cycles();
#endif
#else
    /* it's possible that an init function bumped up our tick rate.
     * If so, set the event_progress counter to 0.  Otherwise, set it to
     * the reset value */
    event_progress_counter = (event_num_mpi_users > 0) ? 
        0 : event_progress_delta;
#endif

    return OPAL_SUCCESS;
}


int
opal_progress_mpi_disable(void)
{
    /* always call sched yield from here on... */
    call_yield = 1;

    /* always tick the event library */
    event_progress_delta = 0;
#if !OPAL_PROGRESS_USE_TIMERS
    event_progress_counter = 0;
#endif

    return OPAL_SUCCESS;
}


int
opal_progress_finalize(void)
{
    /* don't need to free the progess lock */

    /* free memory associated with the callbacks */
#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    if (NULL != callbacks) {
        free(callbacks);
        callbacks = NULL;
    }
    callbacks_len = 0;
    callbacks_size = 0;

#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_unlock(&progress_lock);
#endif

    return OPAL_SUCCESS;
}



void
opal_progress_events(int flag)
{
    opal_progress_event_flag = flag;
}


/*
 * Progress the event library and any functions that have registered to 
 * be called.  We don't propogate errors from the progress functions,
 * so no action is taken if they return failures.  The functions are
 * expected to return the number of events progressed, to determine
 * whether or not we should call sched_yield() during MPI progress.
 * This is only losely tracked, as an error return can cause the number
 * of progressed events to appear lower than it actually is.  We don't
 * care, as the cost of that happening is far outweighed by the cost
 * of the if checks (they were resulting in bad pipe stalling behavior)
 */
void
opal_progress(void)
{
    size_t i;
    int events = 0;

    if( opal_progress_event_flag != 0 ) {
#if (OMPI_ENABLE_PROGRESS_THREADS == 0) && OPAL_HAVE_WORKING_EVENTOPS
#if OPAL_PROGRESS_USE_TIMERS
#if OPAL_TIMER_USEC_NATIVE
    opal_timer_t now = opal_timer_base_get_usec();
#else
    opal_timer_t now = opal_timer_base_get_cycles();
#endif  /* OPAL_TIMER_USEC_NATIVE */
    /* trip the event library if we've reached our tick rate and we are
       enabled */
        if (now - event_progress_last_time > event_progress_delta ) {
#if OMPI_HAVE_THREAD_SUPPORT
            if (opal_atomic_trylock(&progress_lock)) {
#endif  /* OMPI_HAVE_THREAD_SUPPORT */
                event_progress_last_time = (event_num_mpi_users > 0) ? 
                    now - event_progress_delta : now;

                events += opal_event_loop(opal_progress_event_flag);
#if OMPI_HAVE_THREAD_SUPPORT
                opal_atomic_unlock(&progress_lock);
            }
#endif  /* OMPI_HAVE_THREAD_SUPPORT */
        }

#else /* OPAL_PROGRESS_USE_TIMERS */
    /* trip the event library if we've reached our tick rate and we are
       enabled */
        if (OPAL_THREAD_ADD32(&event_progress_counter, -1) <= 0 ) {
#if OMPI_HAVE_THREAD_SUPPORT
            if (opal_atomic_trylock(&progress_lock)) {
#endif  /* OMPI_HAVE_THREAD_SUPPORT */
                event_progress_counter = 
                    (event_num_mpi_users > 0) ? 0 : event_progress_delta;
                events += opal_event_loop(opal_progress_event_flag);
#if OMPI_HAVE_THREAD_SUPPORT
                opal_atomic_unlock(&progress_lock);
            }
#endif  /* OMPI_HAVE_THREAD_SUPPORT */
        }
#endif /* OPAL_PROGRESS_USE_TIMERS */

#endif /* OMPI_ENABLE_PROGRESS_THREADS == 0 && OPAL_HAVE_WORKING_EVENTOPS */
    }

    /* progress all registered callbacks */
    for (i = 0 ; i < callbacks_len ; ++i) {
        events += (callbacks[i])();
    }

#if defined(__WINDOWS__) || defined(HAVE_SCHED_YIELD)
    if (call_yield && events <= 0) {
        /* If there is nothing to do - yield the processor - otherwise
         * we could consume the processor for the entire time slice. If
         * the processor is oversubscribed - this will result in a best-case
         * latency equivalent to the time-slice.
         */
#if defined(__WINDOWS__)
        SwitchToThread();
#else
        sched_yield();
#endif  /* defined(__WINDOWS__) */
    }
#endif  /* defined(__WINDOWS__) || defined(HAVE_SCHED_YIELD) */
}


int
opal_progress_register(opal_progress_callback_t cb)
{
    int ret = OPAL_SUCCESS;

#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    /* see if we need to allocate more space */
    if (callbacks_len + 1 > callbacks_size) {
        opal_progress_callback_t *tmp;
        tmp = (opal_progress_callback_t*)realloc(callbacks, sizeof(opal_progress_callback_t) * (callbacks_size + 4));
        if (tmp == NULL) {
            ret = OPAL_ERR_TEMP_OUT_OF_RESOURCE;
            goto cleanup;
        }

        callbacks = tmp;
        callbacks_size += 4;
    }

    callbacks[callbacks_len++] = cb;

 cleanup:

#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_unlock(&progress_lock);
#endif

    return ret;
}

int
opal_progress_unregister(opal_progress_callback_t cb)
{
    size_t i;
    int ret = OPAL_ERR_NOT_FOUND;

#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    for (i = 0 ; i < callbacks_len ; ++i) {
        if (cb == callbacks[i]) {
            callbacks[i] = NULL;
            ret = OPAL_SUCCESS;
            break;
        }
    }
    
    /* If we found the function we're unregistering: If callbacks_len
       is 0, we're not goig to do anything interesting anyway, so
       skip.  If callbacks_len is 1, it will soon be 0, so no need to
       do any repacking.  size_t can be unsigned, so 0 - 1 is bad for
       a loop condition :). */
    if (OPAL_SUCCESS == ret) {
        if (callbacks_len > 1 ) {
            /* now tightly pack the array */
            for ( ; i < callbacks_len - 1 ; ++i) {
                callbacks[i] = callbacks[i + 1];
            }
        }
        callbacks[callbacks_len - 1] = NULL;
        callbacks_len--;
    }

#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_unlock(&progress_lock);
#endif

    return ret;
}


int
opal_progress_event_increment()
{
    int32_t val;
    val = opal_atomic_add_32(&event_num_mpi_users, 1);

#if OPAL_PROGRESS_USE_TIMERS
    /* force an update next round (we'll be past the delta) */
    event_progress_last_time -= event_progress_delta;
#else
    /* always reset the tick rate - can't hurt */
    event_progress_counter = 0;
#endif

    return OPAL_SUCCESS;
}


int
opal_progress_event_decrement()
{
    int32_t val;
   val = opal_atomic_sub_32(&event_num_mpi_users, 1);

#if !OPAL_PROGRESS_USE_TIMERS
   /* start now in delaying if it's easy */
   if (val >= 0) {
       event_progress_counter = event_progress_delta;
   }
#endif

   return OPAL_SUCCESS;
}

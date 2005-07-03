/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#include "runtime/ompi_progress.h"
#include "event/event.h"
#include "include/constants.h"
#include "mca/base/mca_base_param.h"

/* 
 * default parameters 
 */
static int ompi_progress_event_flag = OMPI_EVLOOP_ONCE;
static const int ompi_progress_default_tick_rate = 100;

/*
 * Local variables
 */
#if OMPI_HAVE_THREAD_SUPPORT
static opal_atomic_lock_t progress_lock;
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

/* callbacks to progress */
static ompi_progress_callback_t *callbacks = NULL;
static size_t callbacks_len = 0;
static size_t callbacks_size = 0;

/* do we want to call sched_yield() if nothing happened */
static int call_yield = 1;

/* current count down until we tick the event library */
static long event_progress_counter = 0;
/* reset value for counter when it hits 0 */
static long event_progress_counter_reset = 0;
/* users of the event library from MPI cause the tick rate to 
   be every time */
static int32_t event_num_mpi_users = 0;


/* init the progress engine - called from orte_init */
int
ompi_progress_init(void)
{
    /* reentrant issues */
#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_init(&progress_lock, OPAL_ATOMIC_UNLOCKED);
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

    /* always call sched yield when in the rte only... */
    call_yield = 1;

    event_progress_counter = event_progress_counter_reset = 0;

    return OMPI_SUCCESS;
}


int
ompi_progress_mpi_init(void)
{
    event_num_mpi_users = 0;

    return OMPI_SUCCESS;
}

/* turn on MPI optimizations */
int
ompi_progress_mpi_enable(void)
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
        event_progress_counter_reset = ompi_progress_default_tick_rate;
    } else if (value == 0) {
        /* user specified as never tick - don't count often */
        event_progress_counter_reset = INT_MAX;
    } else {
        /* subtract one so that we can do post-fix subtraction
           in the inner loop and go faster */
        event_progress_counter_reset = value - 1;
    }

    /* it's possible that an init function bumped up our tick rate.
     * If so, set the event_progress counter to 0.  Otherwise, set it to
     * the reset value */
    event_progress_counter = (event_num_mpi_users > 0) ? 
        0 : event_progress_counter_reset;

    return OMPI_SUCCESS;
}


int
ompi_progress_mpi_disable(void)
{
    /* always call sched yield from here on... */
    call_yield = 1;

    /* always tick the event library */
    event_progress_counter = event_progress_counter_reset = 0;

    return OMPI_SUCCESS;
}


int
ompi_progress_finalize(void)
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

    return OMPI_SUCCESS;
}



void
ompi_progress_events(int flag)
{
    ompi_progress_event_flag = flag;
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
 * of the if checks (they were resulting in bad pipe stalling behabior)
 */
void
ompi_progress(void)
{
    size_t i;
    int events = 0;

#if OMPI_HAVE_THREAD_SUPPORT
    /* NOTE: BWB - XXX - FIXME: Currently, there are some progress functions
       (the event library, for one) that are not reentrant.  It is not known
       if the PTL progress functions are all reentrant or not.  The I/O
       code should all be reentrant.  Because of the uncertainty, we are
       preventing more than one thread of execution from progressing the
       via ompi_progress (which is usually only called when there are
       no PROGRESS_THREADS running).  This should be made more fine-grained
       at some point in the future. */
    if (! opal_atomic_trylock(&progress_lock)) {
        /* someone is already progressing - return */
        return;
    }
#endif

#if OMPI_ENABLE_PROGRESS_THREADS == 0
    /* trip the event library if we've reached our tick rate and we are
       enabled */
    if (event_progress_counter-- <= 0 && ompi_progress_event_flag != 0) {
        event_progress_counter = 
            (event_num_mpi_users > 0) ? 1 : event_progress_counter_reset;
        events += ompi_event_loop(ompi_progress_event_flag);
    }
#endif

    /* progress all registered callbacks */
    for (i = 0 ; i < callbacks_len ; ++i) {
#if OMPI_ENABLE_DEBUG    
        if (NULL != callbacks[i]) {
#endif
            events += (callbacks[i])();
#if OMPI_ENABLE_DEBUG
        } else {
            ompi_output(0, "WARNING: ompi_progess attempted to call NULL"
                        " function at line %d, index %d.", __LINE__, i);
        }  
#endif
    }

#if OMPI_HAVE_THREAD_SUPPORT
    /* release the lock before yielding, for obvious reasons */
    opal_atomic_unlock(&progress_lock);
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

    if (call_yield && events <= 0) {
        /* If there is nothing to do - yield the processor - otherwise
         * we could consume the processor for the entire time slice. If
         * the processor is oversubscribed - this will result in a best-case
         * latency equivalent to the time-slice.
         */
#ifndef WIN32
        /* TODO: Find the windows equivalent for this */
        sched_yield();
#endif
    }
}


int
ompi_progress_register(ompi_progress_callback_t cb)
{
    int ret = OMPI_SUCCESS;

#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    /* see if we need to allocate more space */
    if (callbacks_len + 1 > callbacks_size) {
        ompi_progress_callback_t *tmp;
        tmp = realloc(callbacks, sizeof(ompi_progress_callback_t) * (callbacks_size + 4));
        if (tmp == NULL) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
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
ompi_progress_unregister(ompi_progress_callback_t cb)
{
    size_t i;
    int ret = OMPI_ERR_NOT_FOUND;

#if OMPI_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    for (i = 0 ; i < callbacks_len ; ++i) {
        if (cb == callbacks[i]) {
            callbacks[i] = NULL;
            ret = OMPI_SUCCESS;
            break;
        }
    }
    
    /* If we found the function we're unregistering: If callbacks_len
       is 0, we're not goig to do anything interesting anyway, so
       skip.  If callbacks_len is 1, it will soon be 0, so no need to
       do any repacking.  size_t can be unsigned, so 0 - 1 is bad for
       a loop condition :). */
    if (OMPI_SUCCESS == ret) {
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
ompi_progress_event_increment()
{
    int32_t val;
    val = opal_atomic_add_32(&event_num_mpi_users, 1);
    /* always reset the tick rate - can't hurt */
    event_progress_counter = 0;

    return OMPI_SUCCESS;
}


int
ompi_progress_event_decrement()
{
    int32_t val;
   val = opal_atomic_sub_32(&event_num_mpi_users, 1);
   if (val >= 0) {
       event_progress_counter = event_progress_counter_reset;
   }

   return OMPI_SUCCESS;
}

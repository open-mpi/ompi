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

#include "event/event.h"
#include "mca/pml/pml.h"
#include "runtime/ompi_progress.h"
#include "include/constants.h"

static int ompi_progress_event_flag = OMPI_EVLOOP_ONCE;

#if OMPI_HAVE_THREAD_SUPPORT
static ompi_lock_t progress_lock;
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

static ompi_progress_callback_t *callbacks = NULL;
static size_t callbacks_len = 0;
static size_t callbacks_size = 0;

static int call_yield = 1;

static long event_progress_counter = 0;
static long event_progress_counter_reset = 0;


int
ompi_progress_init(void)
{
    /* reentrant issues */
#if OMPI_HAVE_THREAD_SUPPORT
    ompi_atomic_init(&progress_lock, OMPI_ATOMIC_UNLOCKED);
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

    /* always call sched yield when in the rte only... */
    call_yield = 1;

    event_progress_counter = event_progress_counter_reset = 0;

    return OMPI_SUCCESS;
}


int
ompi_progress_mpi_init(void)
{
    int param, value;

    /* call sched yield when oversubscribed.  Should really set 
     *  the default to something based on the RTE input
     */
    param = mca_base_param_find("mpi", NULL, "yield_when_idle");
    mca_base_param_lookup_int(param, &call_yield);

    /* set the event tick rate */
    param = mca_base_param_find("mpi", NULL, "event_tick_rate");
    mca_base_param_lookup_int(param, &value);

    if (value <= 0) {
        event_progress_counter_reset = INT_MAX;
    } else {
        /* subtract one so that we can do post-fix subtraction
           in the inner loop and go faster */
        event_progress_counter_reset = value - 1;
    }

    event_progress_counter = event_progress_counter_reset;

    return OMPI_SUCCESS;
}


int
ompi_progress_mpi_finalize(void)
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
    ompi_atomic_lock(&progress_lock);
#endif

    free(callbacks);
    callbacks = NULL;
    callbacks_len = 0;
    callbacks_size = 0;

#if OMPI_HAVE_THREAD_SUPPORT
    ompi_atomic_unlock(&progress_lock);
#endif

    return OMPI_SUCCESS;
}



void
ompi_progress_events(int flag)
{
    ompi_progress_event_flag = flag;
}


void
ompi_progress(void)
{
    size_t i;
    int ret, events = 0;

#if OMPI_HAVE_THREAD_SUPPORT
    /* NOTE: BWB - XXX - FIXME: Currently, there are some progress functions
       (the event library, for one) that are not reentrant.  It is not known
       if the PTL progress functions are all reentrant or not.  The I/O
       code should all be reentrant.  Because of the uncertainty, we are
       preventing more than one thread of execution from progressing the
       via ompi_progress (which is usually only called when there are
       no PROGRESS_THREADS running).  This should be made more fine-grained
       at some point in the future. */
    if (! ompi_atomic_trylock(&progress_lock)) {
        /* someone is already progressing - return */
        return;
    }
#endif

#if OMPI_ENABLE_PROGRESS_THREADS == 0
    /* trip the event library if we've reached our tick rate and we are
       enabled */
    if (event_progress_counter-- <= 0 && ompi_progress_event_flag != 0) {
        event_progress_counter = event_progress_counter_reset;
        ret = ompi_event_loop(ompi_progress_event_flag);
        if (ret > 0) {
            events += ret;
        }
    }
#endif

    /* progress all registered callbacks */
    for (i = 0 ; i < callbacks_len ; ++i) {
        if (NULL != callbacks[i]) {
            ret = (callbacks[i])();
            if (ret > 0) {
                events += ret;
            }
        }
    }

#if OMPI_HAVE_THREAD_SUPPORT
    /* release the lock before yielding, for obvious reasons */
    ompi_atomic_unlock(&progress_lock);
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

    if(call_yield && events == 0) {
        /*
         * TSW - BWB - XXX - FIXME: this has a non-zero impact on
         * performance.  Evaluate reasonable defaults.
         * 
         * If there is nothing to do - yield the processor - otherwise
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
    ompi_atomic_lock(&progress_lock);
#endif

    /* see if we need to allocate more space */
    if (callbacks_len + 1 > callbacks_size) {
        ompi_progress_callback_t *tmp;
        tmp = realloc(callbacks, callbacks_size + 4);
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
    ompi_atomic_unlock(&progress_lock);
#endif

    return ret;
}

int
ompi_progress_unregister(ompi_progress_callback_t cb)
{
    size_t i;
    int ret = OMPI_ERR_NOT_FOUND;;

#if OMPI_HAVE_THREAD_SUPPORT
    ompi_atomic_lock(&progress_lock);
#endif

    for (i = 0 ; i < callbacks_len ; ++i) {
        if (cb == callbacks[i]) {
            callbacks[i] = NULL;
            ret = OMPI_SUCCESS;
            goto cleanup;
        }
    }

 cleanup:

#if OMPI_HAVE_THREAD_SUPPORT
    ompi_atomic_unlock(&progress_lock);
#endif

    return ret;
}

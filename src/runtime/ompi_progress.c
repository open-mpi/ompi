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
static ompi_lock_t progress_lock;
static ompi_progress_callback_t *callbacks = NULL;
static size_t callbacks_len = 0;
static size_t callbacks_size = 0;

int
ompi_progress_init(void)
{
    ompi_atomic_init(&progress_lock, OMPI_ATOMIC_UNLOCKED);
    return OMPI_SUCCESS;
}

void ompi_progress_events(int flag)
{
    ompi_progress_event_flag = flag;
}


void ompi_progress(void)
{
    size_t i;

    /* progress any outstanding communications */
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
    if (ompi_progress_event_flag != 0) {
        ret = ompi_event_loop(ompi_progress_event_flag);
        if (ret > 0) {
            events += ret;
        }
    }
#endif

    for (i = 0 ; i < callbacks_len ; ++i) {
        if (NULL != callbacks[i]) {
            ret = (callbacks[i])();
            if (ret > 0) {
                events += ret;
            }
        }
    }

#if 0 /* replaced by callback code */
    ret = mca_pml.pml_progress();
    if (ret > 0) {
        events += ret;
    }

    /* Progress IO requests, if there are any */
    ret = mca_io_base_request_progress();
    if (ret > 0) {
        events += ret;
    }
#endif

#if OMPI_HAVE_THREAD_SUPPORT
    /* release the lock before yielding, for obvious reasons */
    ompi_atomic_unlock(&progress_lock);
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

#if 1
    /* TSW - disable this until can validate that it doesn't impact SMP
     * performance
    */
    /*
     * if there is nothing to do - yield the processor - otherwise
     * we could consume the processor for the entire time slice. If
     * the processor is oversubscribed - this will result in a best-case
     * latency equivalent to the time-slice.
    */
    if(events == 0) {
#ifndef WIN32
        /* TODO: Find the windows equivalent for this */
        sched_yield();
#endif
    }
#endif
}


/*
 * NOTE: This function is not in any way thread-safe.  Do not allow
 * multiple calls to ompi_progress_register and/or ompi_progress
 * concurrently.  This will be fixed in the near future.
 */
int
ompi_progress_register(ompi_progress_callback_t cb)
{
    /* see if we need to allocate more space */
    if (callbacks_len + 1 > callbacks_size) {
        ompi_progress_callback_t *tmp;
        tmp = realloc(callbacks, callbacks_size + 4);
        if (tmp == NULL) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

        callbacks = tmp;
        callbacks_size += 4;
    }

    callbacks[callbacks_len++] = cb;

    return OMPI_SUCCESS;
}

int
ompi_progress_unregister(ompi_progress_callback_t cb)
{
    size_t i;

    for (i = 0 ; i < callbacks_len ; ++i) {
        if (cb == callbacks[i]) {
            callbacks[i] = NULL;
            return OMPI_SUCCESS;
        }
    }

    return OMPI_ERR_NOT_FOUND;
}

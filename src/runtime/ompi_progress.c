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

#include "ompi_config.h"
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/io/io.h"
#include "runtime/ompi_progress.h"


static int ompi_progress_event_flag = OMPI_EVLOOP_ONCE;
int ompi_progress_pending_io_reqs = 0;


void ompi_progress_events(int flag)
{
    ompi_progress_event_flag = flag;
}


void ompi_progress(void)
{
    /* progress any outstanding communications */
    int ret, events = 0;
#if OMPI_HAVE_THREADS == 0
    if (ompi_progress_event_flag != 0) {
        ret = ompi_event_loop(ompi_progress_event_flag);
        if (ret > 0) {
            events += ret;
        }
    }
#endif
    ret = mca_pml.pml_progress();
    if (ret > 0) {
        events += ret;
    }

    /* Progress IO requests, if there are any */

    if (ompi_progress_pending_io_reqs > 0) {
        ret = mca_io_base_progress();
        if (ret > 0) {
            events += ret;
            ompi_progress_pending_io_reqs -= ret;
        }
    }

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


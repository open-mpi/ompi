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
#include <sched.h>
#include "event/event.h"
#include "mca/pml/pml.h"
#include "runtime/ompi_progress.h"

                                                                                                                
static int ompi_progress_event_flag = OMPI_EVLOOP_ONCE;
                                                                                                               
void ompi_progress_events(int flag)
{
    if(flag != 0 || ompi_progress_event_flag == OMPI_EVLOOP_ONCE) {
        ompi_progress_event_flag = flag;
    }
}
                                                                                                               
void ompi_progress(void)
{
    /* progress any outstanding communications */
    int events = 0;
#if OMPI_HAVE_THREADS == 0
    if(ompi_progress_event_flag != 0)
       events += ompi_event_loop(ompi_progress_event_flag);
#endif
    events += mca_pml.pml_progress();

#if 0
    /* TSW - disable this until can validate that it doesn't impact SMP
     * performance
    */
    /*
     * if there is nothing to do - yield the processor - otherwise
     * we could consume the processor for the entire time slice. If
     * the processor is oversubscribed - this will result in a best-case
     * latency equivalent to the time-slice.
    */
    if(events == 0)
        sched_yield();
#endif
}


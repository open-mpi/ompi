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
#include "event/event.h"
#include "mca/pml/pml.h"
#include "runtime/ompi_progress.h"


static int ompi_progress_event_flag = OMPI_EVLOOP_ONCE;

void ompi_progress_events(int flag)
{
    ompi_progress_event_flag = flag;
}

void ompi_progress(void)
{
    if(ompi_progress_event_flag != 0)
       ompi_event_loop(ompi_progress_event_flag);
    mca_pml.pml_progress();
}


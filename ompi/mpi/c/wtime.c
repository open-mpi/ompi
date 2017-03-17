/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>
#ifdef HAVE_TIME_H
#include <time.h>
#endif  /* HAVE_TIME_H */

#include MCA_timer_IMPLEMENTATION_HEADER
#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/mpiruntime.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Wtime = PMPI_Wtime
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif


double MPI_Wtime(void)
{
    double wtime;

    /*
     * See https://github.com/open-mpi/ompi/issues/3003 to find out
     * what's happening here.
     */
#if 0
#if OPAL_TIMER_USEC_NATIVE
    /* We may or may not have native usec precision on Windows, so put
       this #if before the #ifdef checking for Windows. */
    wtime = ((double) opal_timer_base_get_usec()) / 1000000.0;
#elif defined(__WINDOWS__)
    wtime = ((double) opal_timer_base_get_cycles()) /
        ((double) opal_timer_base_get_freq());
#endif
#else
#if defined(__linux__) && OPAL_HAVE_CLOCK_GETTIME
    struct timespec tp = {.tv_sec = 0, .tv_nsec = 0};
    (void) clock_gettime(CLOCK_MONOTONIC, &tp);
    wtime = tp.tv_sec;
    wtime += tp.tv_nsec/1.0e+9;
#else
    /* Fall back to gettimeofday() if we have nothing else */
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif
#endif

    OPAL_CR_NOOP_PROGRESS();

    return wtime;
}

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
 * Copyright (c) 2007-2014 Cisco Systems, Inc.  All rights reserved.
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
#endif

#include MCA_timer_IMPLEMENTATION_HEADER
#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/mpiruntime.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Wtick = PMPI_Wtick
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif


double MPI_Wtick(void)
{
    OPAL_CR_NOOP_PROGRESS();

    /*
     * See https://github.com/open-mpi/ompi/issues/3003
     * to get an idea what's going on here.
     */
#if 0
#if OPAL_TIMER_USEC_NATIVE
    /* We may or may not have native usec precision on Windows, so put
       this #if before the #ifdef checking for Windows. */
    return 0.000001;
#elif defined(__WINDOWS__)
    if( (opal_timer_t)0 == opal_timer_base_get_freq() ) {
        opal_output( 0, "No timer frequency\n" );
    }
    return (double)opal_timer_base_get_freq();
#endif
#else
#if defined(__linux__) && OPAL_HAVE_CLOCK_GETTIME
    struct timespec spec;
    double wtick = 0.0;
    if (0 == clock_getres(CLOCK_MONOTONIC, &spec)){
        wtick =  spec.tv_sec + spec.tv_nsec * 1.0e-09;
    } else {
        /* guess */
        wtick = 1.0e-09;
    }
    return wtick;
#else
    /* Otherwise, we already return usec precision. */
    return 0.000001;
#endif
#endif
}

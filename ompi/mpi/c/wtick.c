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
 * Copyright (c) 2007-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/mpiruntime.h"

#include "opal/util/clock_gettime.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Wtick = PMPI_Wtick
#endif
#define MPI_Wtick PMPI_Wtick
#endif

double MPI_Wtick(void)
{
    // We intentionally don't use the OPAL timer framework here.  See
    // https://github.com/open-mpi/ompi/issues/3003 for more details.
    struct timespec spec;
    double wtick = 0.0;
    if (0 == opal_clock_getres(&spec)){
        wtick =  spec.tv_sec + spec.tv_nsec * 1.0e-09;
    } else {
        /* guess */
        wtick = 1.0e-09;
    }
    return wtick;
}

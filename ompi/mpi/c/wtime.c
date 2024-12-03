/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
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

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/ompi_spc.h"

#include "opal/util/clock_gettime.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Wtime = PMPI_Wtime
#endif
#define MPI_Wtime PMPI_Wtime
#endif
/**
 * Use this as a base time set early during MPI initialization to improve the range
 * and accuracy of the user visible timer.
 * More info: https://github.com/mpi-forum/mpi-issues/issues/77#issuecomment-369663119
 */
extern struct timespec ompi_wtime_time_origin;

double MPI_Wtime(void)
{
    double wtime;

    SPC_RECORD(OMPI_SPC_WTIME, 1);

    // We intentionally don't use the OPAL timer framework here.  See
    // https://github.com/open-mpi/ompi/issues/3003 for more details.
    struct timespec tp;
    (void) opal_clock_gettime(&tp);
    wtime  = (double)(tp.tv_nsec - ompi_wtime_time_origin.tv_nsec)/1.0e+9;
    wtime += (tp.tv_sec - ompi_wtime_time_origin.tv_sec);

    return wtime;
}

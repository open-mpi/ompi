/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

/* The OMPI_GENERATE_F77_BINDINGS work only for the most common F77 bindings, the
 * one that does not return any value. There are 4 exceptions MPI_Wtick, MPI_Wtime,
 * MPI_Aint_add, and MPI_Aint_diff. For these 4 we can insert the bindings
 * manually.
 */
#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_WTICK = ompi_wtick_f
#pragma weak pmpi_wtick = ompi_wtick_f
#pragma weak pmpi_wtick_ = ompi_wtick_f
#pragma weak pmpi_wtick__ = ompi_wtick_f

#pragma weak PMPI_Wtick_f = ompi_wtick_f
#pragma weak PMPI_Wtick_f08 = ompi_wtick_f
#else
double PMPI_WTICK(void) { return pompi_wtick_f(); }
double pmpi_wtick(void) { return pompi_wtick_f(); }
double pmpi_wtick_(void) { return pompi_wtick_f(); }
double pmpi_wtick__(void) { return pompi_wtick_f(); }
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTICK = ompi_wtick_f
#pragma weak mpi_wtick = ompi_wtick_f
#pragma weak mpi_wtick_ = ompi_wtick_f
#pragma weak mpi_wtick__ = ompi_wtick_f

#pragma weak MPI_Wtick_f = ompi_wtick_f
#pragma weak MPI_Wtick_f08 = ompi_wtick_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
double MPI_WTICK(void) { return ompi_wtick_f(); }
double mpi_wtick(void) { return ompi_wtick_f(); }
double mpi_wtick_(void) { return ompi_wtick_f(); }
double mpi_wtick__(void) { return ompi_wtick_f(); }
#else
#define ompi_wtick_f pompi_wtick_f
#endif
#endif


double ompi_wtick_f(void)
{
    return PMPI_Wtick();
}

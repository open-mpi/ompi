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
#pragma weak PMPI_AINT_ADD = ompi_aint_add_f
#pragma weak pmpi_aint_add = ompi_aint_add_f
#pragma weak pmpi_aint_add_ = ompi_aint_add_f
#pragma weak pmpi_aint_add__ = ompi_aint_add_f

#pragma weak PMPI_Aint_add_f = ompi_aint_add_f
#pragma weak PMPI_Aint_add_f08 = ompi_aint_add_f
#else
MPI_Aint PMPI_AINT_ADD(MPI_Aint *base, MPI_Aint *diff) { return pompi_aint_add_f(base, diff); }
MPI_Aint pmpi_aint_add(MPI_Aint *base, MPI_Aint *diff) { return pompi_aint_add_f(base, diff); }
MPI_Aint pmpi_aint_add_(MPI_Aint *base, MPI_Aint *diff) { return pompi_aint_add_f(base, diff); }
MPI_Aint pmpi_aint_add__(MPI_Aint *base, MPI_Aint *diff) { return pompi_aint_add_f(base, diff); }
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_AINT_ADD = ompi_aint_add_f
#pragma weak mpi_aint_add = ompi_aint_add_f
#pragma weak mpi_aint_add_ = ompi_aint_add_f
#pragma weak mpi_aint_add__ = ompi_aint_add_f

#pragma weak MPI_Aint_add_f = ompi_aint_add_f
#pragma weak MPI_Aint_add_f08 = ompi_aint_add_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
MPI_Aint MPI_AINT_ADD(MPI_Aint *base, MPI_Aint *diff) { return ompi_aint_add_f(base, diff); }
MPI_Aint mpi_aint_add(MPI_Aint *base, MPI_Aint *diff) { return ompi_aint_add_f(base, diff); }
MPI_Aint mpi_aint_add_(MPI_Aint *base, MPI_Aint *diff) { return ompi_aint_add_f(base, diff); }
MPI_Aint mpi_aint_add__(MPI_Aint *base, MPI_Aint *diff) { return ompi_aint_add_f(base, diff); }
#else
#define ompi_aint_add_f pompi_aint_add_f
#endif
#endif

MPI_Aint ompi_aint_add_f(MPI_Aint *base, MPI_Aint *diff)
{
    return MPI_Aint_add (*base, *diff);
}

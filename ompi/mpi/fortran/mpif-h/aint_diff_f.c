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
#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_AINT_DIFF = ompi_aint_diff_f
#pragma weak pmpi_aint_diff = ompi_aint_diff_f
#pragma weak pmpi_aint_diff_ = ompi_aint_diff_f
#pragma weak pmpi_aint_diff__ = ompi_aint_diff_f

#pragma weak PMPI_Aint_diff_f = ompi_aint_diff_f
#pragma weak PMPI_Aint_diff_f08 = ompi_aint_diff_f
#elif OMPI_PROFILE_LAYER
MPI_Aint PMPI_AINT_DIFF(MPI_Aint *addr1, MPI_Aint *addr2) { return pompi_aint_diff_f(addr1, addr2); }
MPI_Aint pmpi_aint_diff(MPI_Aint *addr1, MPI_Aint *addr2) { return pompi_aint_diff_f(addr1, addr2); }
MPI_Aint pmpi_aint_diff_(MPI_Aint *addr1, MPI_Aint *addr2) { return pompi_aint_diff_f(addr1, addr2); }
MPI_Aint pmpi_aint_diff__(MPI_Aint *addr1, MPI_Aint *addr2) { return pompi_aint_diff_f(addr1, addr2); }
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_AINT_DIFF = ompi_aint_diff_f
#pragma weak mpi_aint_diff = ompi_aint_diff_f
#pragma weak mpi_aint_diff_ = ompi_aint_diff_f
#pragma weak mpi_aint_diff__ = ompi_aint_diff_f

#pragma weak MPI_Aint_diff_f = ompi_aint_diff_f
#pragma weak MPI_Aint_diff_f08 = ompi_aint_diff_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
MPI_Aint MPI_AINT_DIFF(MPI_Aint *addr1, MPI_Aint *addr2) { return ompi_aint_diff_f(addr1, addr2); }
MPI_Aint mpi_aint_diff(MPI_Aint *addr1, MPI_Aint *addr2) { return ompi_aint_diff_f(addr1, addr2); }
MPI_Aint mpi_aint_diff_(MPI_Aint *addr1, MPI_Aint *addr2) { return ompi_aint_diff_f(addr1, addr2); }
MPI_Aint mpi_aint_diff__(MPI_Aint *addr1, MPI_Aint *addr2) { return ompi_aint_diff_f(addr1, addr2); }
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

MPI_Aint ompi_aint_diff_f(MPI_Aint *addr1, MPI_Aint *addr2)
{
    return MPI_Aint_diff (*addr1, *addr2);
}

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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_FREE_MEM = ompi_free_mem_f
#pragma weak pmpi_free_mem = ompi_free_mem_f
#pragma weak pmpi_free_mem_ = ompi_free_mem_f
#pragma weak pmpi_free_mem__ = ompi_free_mem_f

#pragma weak PMPI_Free_mem_f = ompi_free_mem_f
#pragma weak PMPI_Free_mem_f08 = ompi_free_mem_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FREE_MEM,
                           pmpi_free_mem,
                           pmpi_free_mem_,
                           pmpi_free_mem__,
                           pompi_free_mem_f,
                           (char *base, MPI_Fint *ierr),
                           (base, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FREE_MEM = ompi_free_mem_f
#pragma weak mpi_free_mem = ompi_free_mem_f
#pragma weak mpi_free_mem_ = ompi_free_mem_f
#pragma weak mpi_free_mem__ = ompi_free_mem_f

#pragma weak MPI_Free_mem_f = ompi_free_mem_f
#pragma weak MPI_Free_mem_f08 = ompi_free_mem_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FREE_MEM,
                           mpi_free_mem,
                           mpi_free_mem_,
                           mpi_free_mem__,
                           ompi_free_mem_f,
                           (char *base, MPI_Fint *ierr),
                           (base, ierr) )
#else
#define ompi_free_mem_f pompi_free_mem_f
#endif
#endif


void ompi_free_mem_f(char *base, MPI_Fint *ierr)
{
    int c_ierr = PMPI_Free_mem(base);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

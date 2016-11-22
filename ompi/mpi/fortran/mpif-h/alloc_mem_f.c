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
 * Copyright (c) 2014-2015 Research Organization for Information Science
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
#pragma weak PMPI_ALLOC_MEM = ompi_alloc_mem_f
#pragma weak pmpi_alloc_mem = ompi_alloc_mem_f
#pragma weak pmpi_alloc_mem_ = ompi_alloc_mem_f
#pragma weak pmpi_alloc_mem__ = ompi_alloc_mem_f

#pragma weak PMPI_Alloc_mem_f = ompi_alloc_mem_f
#pragma weak PMPI_Alloc_mem_f08 = ompi_alloc_mem_f

#pragma weak PMPI_ALLOC_MEM_CPTR = ompi_alloc_mem_f
#pragma weak pmpi_alloc_mem_cptr = ompi_alloc_mem_f
#pragma weak pmpi_alloc_mem_cptr_ = ompi_alloc_mem_f
#pragma weak pmpi_alloc_mem_cptr__ = ompi_alloc_mem_f

#pragma weak PMPI_Alloc_mem_cptr_f = ompi_alloc_mem_f
#pragma weak PMPI_Alloc_mem_cptr_f08 = ompi_alloc_mem_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLOC_MEM,
                           pmpi_alloc_mem,
                           pmpi_alloc_mem_,
                           pmpi_alloc_mem__,
                           pompi_alloc_mem_f,
                           (MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr),
                           (size, info, baseptr, ierr) )

OMPI_GENERATE_F77_BINDINGS (PMPI_ALLOC_MEM_CPTR,
                           pmpi_alloc_mem_cptr,
                           pmpi_alloc_mem_cptr_,
                           pmpi_alloc_mem_cptr__,
                           pompi_alloc_mem_cptr_f,
                           (MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr),
                           (size, info, baseptr, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLOC_MEM = ompi_alloc_mem_f
#pragma weak mpi_alloc_mem = ompi_alloc_mem_f
#pragma weak mpi_alloc_mem_ = ompi_alloc_mem_f
#pragma weak mpi_alloc_mem__ = ompi_alloc_mem_f

#pragma weak MPI_Alloc_mem_f = ompi_alloc_mem_f
#pragma weak MPI_Alloc_mem_f08 = ompi_alloc_mem_f

#pragma weak MPI_ALLOC_MEM_CPTR = ompi_alloc_mem_f
#pragma weak mpi_alloc_mem_cptr = ompi_alloc_mem_f
#pragma weak mpi_alloc_mem_cptr_ = ompi_alloc_mem_f
#pragma weak mpi_alloc_mem_cptr__ = ompi_alloc_mem_f

#pragma weak MPI_Alloc_mem_cptr_f = ompi_alloc_mem_f
#pragma weak MPI_Alloc_mem_cptr_f08 = ompi_alloc_mem_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ALLOC_MEM,
                           mpi_alloc_mem,
                           mpi_alloc_mem_,
                           mpi_alloc_mem__,
                           ompi_alloc_mem_f,
                           (MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr),
                           (size, info, baseptr, ierr) )

OMPI_GENERATE_F77_BINDINGS (MPI_ALLOC_MEM_CPTR,
                           mpi_alloc_mem_cptr,
                           mpi_alloc_mem_cptr_,
                           mpi_alloc_mem_cptr__,
                           ompi_alloc_mem_cptr_f,
                           (MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr),
                           (size, info, baseptr, ierr) )
#else
#define ompi_alloc_mem_f pompi_alloc_mem_f
#define ompi_alloc_mem_cptr_f pompi_alloc_mem_cptr_f
#endif
#endif


void ompi_alloc_mem_f(MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Info c_info = PMPI_Info_f2c(*info);

    ierr_c = PMPI_Alloc_mem(*size, c_info, baseptr);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
}

/*
 * Note that MPI-3 mandates a second form of the
 * MPI_Alloc_mem interface -- one that has a "_cptr" suffix.
 */
void ompi_alloc_mem_cptr_f(MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr)
{
    ompi_alloc_mem_f(size, info, baseptr, ierr);
}

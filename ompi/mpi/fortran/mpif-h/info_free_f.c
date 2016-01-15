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
#pragma weak PMPI_INFO_FREE = ompi_info_free_f
#pragma weak pmpi_info_free = ompi_info_free_f
#pragma weak pmpi_info_free_ = ompi_info_free_f
#pragma weak pmpi_info_free__ = ompi_info_free_f

#pragma weak PMPI_Info_free_f = ompi_info_free_f
#pragma weak PMPI_Info_free_f08 = ompi_info_free_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_FREE,
                           pmpi_info_free,
                           pmpi_info_free_,
                           pmpi_info_free__,
                           pompi_info_free_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_FREE = ompi_info_free_f
#pragma weak mpi_info_free = ompi_info_free_f
#pragma weak mpi_info_free_ = ompi_info_free_f
#pragma weak mpi_info_free__ = ompi_info_free_f

#pragma weak MPI_Info_free_f = ompi_info_free_f
#pragma weak MPI_Info_free_f08 = ompi_info_free_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_FREE,
                           mpi_info_free,
                           mpi_info_free_,
                           mpi_info_free__,
                           ompi_info_free_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#else
#define ompi_info_free_f pompi_info_free_f
#endif
#endif


void ompi_info_free_f(MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Info c_info;

    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Info_free(&c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *info = PMPI_Info_c2f(c_info);
    }
}

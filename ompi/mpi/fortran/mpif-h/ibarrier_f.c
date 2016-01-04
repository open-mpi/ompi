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
#pragma weak PMPI_IBARRIER = ompi_ibarrier_f
#pragma weak pmpi_ibarrier = ompi_ibarrier_f
#pragma weak pmpi_ibarrier_ = ompi_ibarrier_f
#pragma weak pmpi_ibarrier__ = ompi_ibarrier_f

#pragma weak PMPI_Ibarrier_f = ompi_ibarrier_f
#pragma weak PMPI_Ibarrier_f08 = ompi_ibarrier_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_IBARRIER,
                            pmpi_ibarrier,
                            pmpi_ibarrier_,
                            pmpi_ibarrier__,
                            pompi_ibarrier_f,
                            (MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IBARRIER = ompi_ibarrier_f
#pragma weak mpi_ibarrier = ompi_ibarrier_f
#pragma weak mpi_ibarrier_ = ompi_ibarrier_f
#pragma weak mpi_ibarrier__ = ompi_ibarrier_f

#pragma weak MPI_Ibarrier_f = ompi_ibarrier_f
#pragma weak MPI_Ibarrier_f08 = ompi_ibarrier_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_IBARRIER,
                            mpi_ibarrier,
                            mpi_ibarrier_,
                            mpi_ibarrier__,
                            ompi_ibarrier_f,
                            (MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, request, ierr) )
#else
#define ompi_ibarrier_f pompi_ibarrier_f
#endif
#endif


void ompi_ibarrier_f(MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;
    MPI_Request c_req;

    c_comm = PMPI_Comm_f2c(*comm);

    ierr_c = PMPI_Ibarrier(c_comm, &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS == ierr_c) *request = PMPI_Request_c2f(c_req);
}

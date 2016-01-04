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
#pragma weak PMPI_GREQUEST_COMPLETE = ompi_grequest_complete_f
#pragma weak pmpi_grequest_complete = ompi_grequest_complete_f
#pragma weak pmpi_grequest_complete_ = ompi_grequest_complete_f
#pragma weak pmpi_grequest_complete__ = ompi_grequest_complete_f

#pragma weak PMPI_Grequest_complete_f = ompi_grequest_complete_f
#pragma weak PMPI_Grequest_complete_f08 = ompi_grequest_complete_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GREQUEST_COMPLETE,
                           pmpi_grequest_complete,
                           pmpi_grequest_complete_,
                           pmpi_grequest_complete__,
                           pompi_grequest_complete_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GREQUEST_COMPLETE = ompi_grequest_complete_f
#pragma weak mpi_grequest_complete = ompi_grequest_complete_f
#pragma weak mpi_grequest_complete_ = ompi_grequest_complete_f
#pragma weak mpi_grequest_complete__ = ompi_grequest_complete_f

#pragma weak MPI_Grequest_complete_f = ompi_grequest_complete_f
#pragma weak MPI_Grequest_complete_f08 = ompi_grequest_complete_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GREQUEST_COMPLETE,
                           mpi_grequest_complete,
                           mpi_grequest_complete_,
                           mpi_grequest_complete__,
                           ompi_grequest_complete_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#else
#define ompi_grequest_complete_f pompi_grequest_complete_f
#endif
#endif


void ompi_grequest_complete_f(MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request c_req = PMPI_Request_f2c(*request);

    c_ierr = PMPI_Grequest_complete(c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

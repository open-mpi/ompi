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
#pragma weak PMPI_CANCEL = ompi_cancel_f
#pragma weak pmpi_cancel = ompi_cancel_f
#pragma weak pmpi_cancel_ = ompi_cancel_f
#pragma weak pmpi_cancel__ = ompi_cancel_f

#pragma weak PMPI_Cancel_f = ompi_cancel_f
#pragma weak PMPI_Cancel_f08 = ompi_cancel_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_CANCEL,
                           pmpi_cancel,
                           pmpi_cancel_,
                           pmpi_cancel__,
                           pompi_cancel_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CANCEL = ompi_cancel_f
#pragma weak mpi_cancel = ompi_cancel_f
#pragma weak mpi_cancel_ = ompi_cancel_f
#pragma weak mpi_cancel__ = ompi_cancel_f

#pragma weak MPI_Cancel_f = ompi_cancel_f
#pragma weak MPI_Cancel_f08 = ompi_cancel_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_CANCEL,
                           mpi_cancel,
                           mpi_cancel_,
                           mpi_cancel__,
                           ompi_cancel_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OMPI_BUILD_MPI_PROFILING && ! OPAL_HAVE_WEAK_SYMBOLS
#define ompi_cancel_f pompi_cancel_f
#endif
#endif


void ompi_cancel_f(MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request c_req = PMPI_Request_f2c(*request);

    c_ierr = PMPI_Cancel(&c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

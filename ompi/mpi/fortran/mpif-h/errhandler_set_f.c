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
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/errhandler/errhandler.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_SET = ompi_errhandler_set_f
#pragma weak pmpi_errhandler_set = ompi_errhandler_set_f
#pragma weak pmpi_errhandler_set_ = ompi_errhandler_set_f
#pragma weak pmpi_errhandler_set__ = ompi_errhandler_set_f

#pragma weak PMPI_Errhandler_set_f = ompi_errhandler_set_f
#pragma weak PMPI_Errhandler_set_f08 = ompi_errhandler_set_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_SET,
                           pmpi_errhandler_set,
                           pmpi_errhandler_set_,
                           pmpi_errhandler_set__,
                           pompi_errhandler_set_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_SET = ompi_errhandler_set_f
#pragma weak mpi_errhandler_set = ompi_errhandler_set_f
#pragma weak mpi_errhandler_set_ = ompi_errhandler_set_f
#pragma weak mpi_errhandler_set__ = ompi_errhandler_set_f

#pragma weak MPI_Errhandler_set_f = ompi_errhandler_set_f
#pragma weak MPI_Errhandler_set_f08 = ompi_errhandler_set_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_SET,
                           mpi_errhandler_set,
                           mpi_errhandler_set_,
                           mpi_errhandler_set__,
                           ompi_errhandler_set_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_errhandler_set_f(MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Errhandler c_errhandler;

    c_comm = MPI_Comm_f2c(*comm);
    c_errhandler = MPI_Errhandler_f2c(*errhandler);

    c_ierr = MPI_Errhandler_set(c_comm, c_errhandler);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

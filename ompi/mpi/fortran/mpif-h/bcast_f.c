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
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_BCAST = ompi_bcast_f
#pragma weak pmpi_bcast = ompi_bcast_f
#pragma weak pmpi_bcast_ = ompi_bcast_f
#pragma weak pmpi_bcast__ = ompi_bcast_f

#pragma weak PMPI_Bcast_f = ompi_bcast_f
#pragma weak PMPI_Bcast_f08 = ompi_bcast_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_BCAST,
                           pmpi_bcast,
                           pmpi_bcast_,
                           pmpi_bcast__,
                           pompi_bcast_f,
                           (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (buffer, count, datatype, root, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BCAST = ompi_bcast_f
#pragma weak mpi_bcast = ompi_bcast_f
#pragma weak mpi_bcast_ = ompi_bcast_f
#pragma weak mpi_bcast__ = ompi_bcast_f

#pragma weak MPI_Bcast_f = ompi_bcast_f
#pragma weak MPI_Bcast_f08 = ompi_bcast_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_BCAST,
                           mpi_bcast,
                           mpi_bcast_,
                           mpi_bcast__,
                           ompi_bcast_f,
                           (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (buffer, count, datatype, root, comm, ierr) )
#else
#define ompi_bcast_f pompi_bcast_f
#endif
#endif


void ompi_bcast_f(char *buffer, MPI_Fint *count, MPI_Fint *datatype,
		 MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;

    c_comm = PMPI_Comm_f2c(*comm);
    c_type = PMPI_Type_f2c(*datatype);

    c_ierr = PMPI_Bcast(OMPI_F2C_BOTTOM(buffer),
                       OMPI_FINT_2_INT(*count),
                       c_type,
                       OMPI_FINT_2_INT(*root),
                       c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

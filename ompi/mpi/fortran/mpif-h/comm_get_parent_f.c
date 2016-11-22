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
#pragma weak PMPI_COMM_GET_PARENT = ompi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent = ompi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent_ = ompi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent__ = ompi_comm_get_parent_f

#pragma weak PMPI_Comm_get_parent_f = ompi_comm_get_parent_f
#pragma weak PMPI_Comm_get_parent_f08 = ompi_comm_get_parent_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_GET_PARENT,
                           pmpi_comm_get_parent,
                           pmpi_comm_get_parent_,
                           pmpi_comm_get_parent__,
                           pompi_comm_get_parent_f,
                           (MPI_Fint *parent, MPI_Fint *ierr),
                           (parent, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_PARENT = ompi_comm_get_parent_f
#pragma weak mpi_comm_get_parent = ompi_comm_get_parent_f
#pragma weak mpi_comm_get_parent_ = ompi_comm_get_parent_f
#pragma weak mpi_comm_get_parent__ = ompi_comm_get_parent_f

#pragma weak MPI_Comm_get_parent_f = ompi_comm_get_parent_f
#pragma weak MPI_Comm_get_parent_f08 = ompi_comm_get_parent_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_GET_PARENT,
                           mpi_comm_get_parent,
                           mpi_comm_get_parent_,
                           mpi_comm_get_parent__,
                           ompi_comm_get_parent_f,
                           (MPI_Fint *parent, MPI_Fint *ierr),
                           (parent, ierr) )
#else
#define ompi_comm_get_parent_f pompi_comm_get_parent_f
#endif
#endif


void ompi_comm_get_parent_f(MPI_Fint *parent, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_parent;

    c_ierr = PMPI_Comm_get_parent(&c_parent);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *parent = PMPI_Comm_c2f(c_parent);
    }
}

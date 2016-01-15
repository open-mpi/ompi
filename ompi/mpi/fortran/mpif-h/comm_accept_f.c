/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#include "ompi/mpi/fortran/base/strings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_COMM_ACCEPT = ompi_comm_accept_f
#pragma weak pmpi_comm_accept = ompi_comm_accept_f
#pragma weak pmpi_comm_accept_ = ompi_comm_accept_f
#pragma weak pmpi_comm_accept__ = ompi_comm_accept_f

#pragma weak PMPI_Comm_accept_f = ompi_comm_accept_f
#pragma weak PMPI_Comm_accept_f08 = ompi_comm_accept_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_ACCEPT,
                           pmpi_comm_accept,
                           pmpi_comm_accept_,
                           pmpi_comm_accept__,
                           pompi_comm_accept_f,
                           (char *port_name, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr, int port_name_len),
                           (port_name, info, root, comm, newcomm, ierr, port_name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_ACCEPT = ompi_comm_accept_f
#pragma weak mpi_comm_accept = ompi_comm_accept_f
#pragma weak mpi_comm_accept_ = ompi_comm_accept_f
#pragma weak mpi_comm_accept__ = ompi_comm_accept_f

#pragma weak MPI_Comm_accept_f = ompi_comm_accept_f
#pragma weak MPI_Comm_accept_f08 = ompi_comm_accept_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_ACCEPT,
                           mpi_comm_accept,
                           mpi_comm_accept_,
                           mpi_comm_accept__,
                           ompi_comm_accept_f,
                           (char *port_name, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr, int port_name_len),
                           (port_name, info, root, comm, newcomm, ierr, port_name_len) )
#else
#define ompi_comm_accept_f pompi_comm_accept_f
#endif
#endif


void ompi_comm_accept_f(char *port_name, MPI_Fint *info, MPI_Fint *root,
		       MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr,
		       int port_name_len)
{
    int c_ierr;
    MPI_Comm c_comm, c_new_comm;
    MPI_Info c_info;
    char *c_port_name;

    c_comm = PMPI_Comm_f2c(*comm);
    c_info = PMPI_Info_f2c(*info);
    ompi_fortran_string_f2c(port_name, port_name_len, &c_port_name);


    c_ierr = PMPI_Comm_accept(c_port_name, c_info,
                             OMPI_FINT_2_INT(*root),
                             c_comm, &c_new_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newcomm = PMPI_Comm_c2f(c_new_comm);
    }
    free ( c_port_name );
}

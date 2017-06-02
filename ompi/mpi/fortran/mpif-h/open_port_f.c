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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/fortran_base_strings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_OPEN_PORT = ompi_open_port_f
#pragma weak pmpi_open_port = ompi_open_port_f
#pragma weak pmpi_open_port_ = ompi_open_port_f
#pragma weak pmpi_open_port__ = ompi_open_port_f

#pragma weak PMPI_Open_port_f = ompi_open_port_f
#pragma weak PMPI_Open_port_f08 = ompi_open_port_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_OPEN_PORT,
                           pmpi_open_port,
                           pmpi_open_port_,
                           pmpi_open_port__,
                           pompi_open_port_f,
                           (MPI_Fint *info, char *port_name, MPI_Fint *ierr, int port_name_len),
                           (info, port_name, ierr, port_name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OPEN_PORT = ompi_open_port_f
#pragma weak mpi_open_port = ompi_open_port_f
#pragma weak mpi_open_port_ = ompi_open_port_f
#pragma weak mpi_open_port__ = ompi_open_port_f

#pragma weak MPI_Open_port_f = ompi_open_port_f
#pragma weak MPI_Open_port_f08 = ompi_open_port_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_OPEN_PORT,
                           mpi_open_port,
                           mpi_open_port_,
                           mpi_open_port__,
                           ompi_open_port_f,
                           (MPI_Fint *info, char *port_name, MPI_Fint *ierr, int port_name_len),
                           (info, port_name, ierr, port_name_len) )
#else
#define ompi_open_port_f pompi_open_port_f
#endif
#endif


void ompi_open_port_f(MPI_Fint *info, char *port_name, MPI_Fint *ierr, int port_name_len)
{
    int c_ierr;
    MPI_Info c_info;
    char c_port_name[MPI_MAX_PORT_NAME];

    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Open_port(c_info, c_port_name);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if ( MPI_SUCCESS == c_ierr ) {
	ompi_fortran_string_c2f(c_port_name, port_name, port_name_len );
    }
}

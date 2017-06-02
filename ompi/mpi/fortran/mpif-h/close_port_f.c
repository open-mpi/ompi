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
#pragma weak PMPI_CLOSE_PORT = ompi_close_port_f
#pragma weak pmpi_close_port = ompi_close_port_f
#pragma weak pmpi_close_port_ = ompi_close_port_f
#pragma weak pmpi_close_port__ = ompi_close_port_f

#pragma weak PMPI_Close_port_f = ompi_close_port_f
#pragma weak PMPI_Close_port_f08 = ompi_close_port_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_CLOSE_PORT,
                           pmpi_close_port,
                           pmpi_close_port_,
                           pmpi_close_port__,
                           pompi_close_port_f,
                           (char *port_name, MPI_Fint *ierr, int port_name_len),
                           (port_name, ierr, port_name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CLOSE_PORT = ompi_close_port_f
#pragma weak mpi_close_port = ompi_close_port_f
#pragma weak mpi_close_port_ = ompi_close_port_f
#pragma weak mpi_close_port__ = ompi_close_port_f

#pragma weak MPI_Close_port_f = ompi_close_port_f
#pragma weak MPI_Close_port_f08 = ompi_close_port_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_CLOSE_PORT,
                           mpi_close_port,
                           mpi_close_port_,
                           mpi_close_port__,
                           ompi_close_port_f,
                           (char *port_name, MPI_Fint *ierr, int port_name_len),
                           (port_name, ierr, port_name_len) )
#else
#define ompi_close_port_f pompi_close_port_f
#endif
#endif


void ompi_close_port_f(char *port_name, MPI_Fint *ierr, int port_name_len)
{
    int c_ierr;
    char *c_port_name;

    ompi_fortran_string_f2c(port_name, port_name_len, &c_port_name);
    c_ierr = PMPI_Close_port(c_port_name);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free ( c_port_name);
}

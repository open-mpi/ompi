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
 * Copyright (c) 2025      Triad National Security, LLC.  All rights reserved.
 *
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
#pragma weak PMPI_COMM_ATTACH_BUFFER = ompi_comm_attach_buffer_f
#pragma weak pmpi_comm_attach_buffer = ompi_comm_attach_buffer_f
#pragma weak pmpi_comm_attach_buffer_ = ompi_comm_attach_buffer_f
#pragma weak pmpi_comm_attach_buffer__ = ompi_comm_attach_buffer_f

#pragma weak PMPI_Comm_attach_buffer_f = ompi_comm_attach_buffer_f
#pragma weak PMPI_Comm_attach_buffer_f08 = ompi_comm_attach_buffer_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_ATTACH_BUFFER,
                           pmpi_comm_attach_buffer,
                           pmpi_comm_attach_buffer_,
                           pmpi_comm_attach_buffer__,
                           pompi_comm_attach_buffer_f,
                           (MPI_Fint *comm, char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, buffer, size, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_ATTACH_BUFFER = ompi_comm_attach_buffer_f
#pragma weak mpi_comm_attach_buffer = ompi_comm_attach_buffer_f
#pragma weak mpi_comm_attach_buffer_ = ompi_comm_attach_buffer_f
#pragma weak mpi_comm_attach_buffer__ = ompi_comm_attach_buffer_f

#pragma weak MPI_Comm_attach_buffer_f = ompi_comm_attach_buffer_f
#pragma weak MPI_Comm_attach_buffer_f08 = ompi_comm_attach_buffer_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_ATTACH_BUFFER,
                           mpi_comm_attach_buffer,
                           mpi_comm_attach_buffer_,
                           mpi_comm_attach_buffer__,
                           ompi_comm_attach_buffer_f,
                           (MPI_Fint *comm, char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, buffer, size, ierr) )
#else
#define ompi_comm_attach_buffer_f pompi_comm_attach_buffer_f
#endif
#endif


void ompi_comm_attach_buffer_f(MPI_Fint *comm, char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{
   MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
   int c_ierr = PMPI_Comm_attach_buffer(c_comm, 
                                        OMPI_F2C_BUFFER_AUTOMATIC(buffer), 
                                        OMPI_FINT_2_INT(*size));
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

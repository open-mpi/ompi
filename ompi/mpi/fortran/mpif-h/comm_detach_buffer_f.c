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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019-2025 Triad National Security, LLC.  All rights reserved.
 *
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
#pragma weak PMPI_COMM_DETACH_BUFFER = ompi_comm_detach_buffer_f
#pragma weak pmpi_comm_detach_buffer = ompi_comm_detach_buffer_f
#pragma weak pmpi_comm_detach_buffer_ = ompi_comm_detach_buffer_f
#pragma weak pmpi_comm_detach_buffer__ = ompi_comm_detach_buffer_f

#pragma weak PMPI_Comm_detach_buffer_ = ompi_comm_detach_buffer_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_DETACH_BUFFER,
                           pmpi_comm_detach_buffer,
                           pmpi_comm_detach_buffer_,
                           pmpi_comm_detach_buffer__,
                           pompi_comm_detach_buffer_f,
                           (MPI_Fint *comm, char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, buffer, size, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_DETACH_BUFFER = ompi_comm_detach_buffer_f
#pragma weak mpi_comm_detach_buffer = ompi_comm_detach_buffer_f
#pragma weak mpi_comm_detach_buffer_ = ompi_comm_detach_buffer_f
#pragma weak mpi_comm_detach_buffer__ = ompi_comm_detach_buffer_f

#pragma weak MPI_Comm_detach_buffer_ = ompi_comm_detach_buffer_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_DETACH_BUFFER,
                           mpi_comm_detach_buffer,
                           mpi_comm_detach_buffer_,
                           mpi_comm_detach_buffer__,
                           ompi_comm_detach_buffer_f,
                           (MPI_Fint *comm, char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, buffer, size, ierr) )
#else
#define ompi_comm_detach_buffer_f pompi_comm_detach_buffer_f
#endif
#endif

/*
 * MPI-3.1 section 3.6, page 45, states that the mpif.h and mpi module
 * interfaces for MPI_BUFFER_DETACH ignore the buffer argument.
 * Therefore, for the mpif.h and mpi module interfaces, we use a dummy
 * variable and leave the value handed in alone.
 *
 * The mpi_f08 implementation for MPI_BUFFER_DETACH therefore is a
 * separate routine in the use-mpi-f08 directory (it's not built in
 * the mpif-h directory because of all the different combinations of
 * supporting weak symbols (or not), building the profiling layer (or
 * not), etc.).
 */
void ompi_comm_detach_buffer_f(MPI_Fint *comm, char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{
    int c_ierr;
    void *dummy;
    OMPI_SINGLE_NAME_DECL(size);
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    c_ierr = PMPI_Comm_detach_buffer(c_comm, &dummy, OMPI_SINGLE_NAME_CONVERT(size));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}

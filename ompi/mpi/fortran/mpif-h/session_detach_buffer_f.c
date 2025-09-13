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
#pragma weak PMPI_SESSION_DETACH_BUFFER = ompi_session_detach_buffer_f
#pragma weak pmpi_session_detach_buffer = ompi_session_detach_buffer_f
#pragma weak pmpi_session_detach_buffer_ = ompi_session_detach_buffer_f
#pragma weak pmpi_session_detach_buffer__ = ompi_session_detach_buffer_f

#pragma weak PMPI_Session_detach_buffer_ = ompi_session_detach_buffer_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SESSION_DETACH_BUFFER,
                           pmpi_session_detach_buffer,
                           pmpi_session_detach_buffer_,
                           pmpi_session_detach_buffer__,
                           pompi_session_detach_buffer_f,
                           (MPI_Fint *session, char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (session, buffer, size, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SESSION_DETACH_BUFFER = ompi_session_detach_buffer_f
#pragma weak mpi_session_detach_buffer = ompi_session_detach_buffer_f
#pragma weak mpi_session_detach_buffer_ = ompi_session_detach_buffer_f
#pragma weak mpi_session_detach_buffer__ = ompi_session_detach_buffer_f

#pragma weak MPI_Session_detach_buffer_ = ompi_session_detach_buffer_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SESSION_DETACH_BUFFER,
                           mpi_session_detach_buffer,
                           mpi_session_detach_buffer_,
                           mpi_session_detach_buffer__,
                           ompi_session_detach_buffer_f,
                           (MPI_Fint *session, char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (session, buffer, size, ierr) )
#else
#define ompi_session_detach_buffer_f pompi_session_detach_buffer_f
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
void ompi_session_detach_buffer_f(MPI_Fint *session, char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{
    int c_ierr;
    void *dummy;
    OMPI_SINGLE_NAME_DECL(size);
    MPI_Session c_session = PMPI_Session_f2c(*session);

    c_ierr = PMPI_Session_detach_buffer(c_session, &dummy, OMPI_SINGLE_NAME_CONVERT(size));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}

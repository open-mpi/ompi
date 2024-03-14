/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Triad National Security, LLC.  All rights reserved.
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
#pragma weak PMPI_SESSION_FINALIZE = ompi_session_finalize_f
#pragma weak pmpi_session_finalize = ompi_session_finalize_f
#pragma weak pmpi_session_finalize_ = ompi_session_finalize_f
#pragma weak pmpi_session_finalize__ = ompi_session_finalize_f

#pragma weak PMPI_Session_finalize_f = ompi_session_finalize_f
#pragma weak PMPI_Session_finalize_f08 = ompi_session_finalize_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SESSION_FINALIZE,
                            pmpi_session_finalize,
                            pmpi_session_finalize_,
                            pmpi_session_finalize__,
                            pompi_session_finalize_f,
                            (MPI_Fint *session, MPI_Fint *ierr),
                            (session, ierr) )
#endif
#endif


#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SESSION_FINALIZE = ompi_session_finalize_f
#pragma weak mpi_session_finalize = ompi_session_finalize_f
#pragma weak mpi_session_finalize_ = ompi_session_finalize_f
#pragma weak mpi_session_finalize__ = ompi_session_finalize_f

#pragma weak MPI_Session_finalize_f = ompi_session_finalize_f
#pragma weak MPI_Session_finalize_f08 = ompi_session_finalize_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SESSION_FINALIZE,
                            mpi_session_finalize,
                            mpi_session_finalize_,
                            mpi_session_finalize__,
                            ompi_session_finalize_f,
                            (MPI_Fint *session, MPI_Fint *ierr),
                            (session, ierr) )
#else
#define ompi_session_finalize_f pompi_session_finalize_f
#endif
#endif

void ompi_session_finalize_f(MPI_Fint *session, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Session c_session;

    c_session = PMPI_Session_f2c(*session);

    c_ierr = PMPI_Session_finalize(&c_session);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    /* This value comes from the MPI_SESSION_NULL value in mpif.h.  Do not
       change without consulting mpif.h! */

    if (MPI_SUCCESS == c_ierr) {
        *session = 0;
    }
}

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
 * Copyright (c) 2015-2023 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "ompi/mpi/fortran/base/fortran_base_strings.h"
#include "ompi/constants.h"


#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_SESSION_GET_NTH_PSET = ompi_session_get_nth_pset_f
#pragma weak pmpi_session_get_nth_pset = ompi_session_get_nth_pset_f
#pragma weak pmpi_session_get_nth_pset_ = ompi_session_get_nth_pset_f
#pragma weak pmpi_session_get_nth_pset__ = ompi_session_get_nth_pset_f

#pragma weak PMPI_Session_get_nth_pset_f = ompi_session_get_nth_pset_f
#pragma weak PMPI_Session_get_nth_pset_f08 = ompi_session_get_nth_pset_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SESSION_GET_NTH_PSET,
                            pmpi_session_get_nth_pset,
                            pmpi_session_get_nth_pset_,
                            pmpi_session_get_nth_pset__,
                            pompi_session_get_nth_pset_f,
                            (MPI_Fint *session, MPI_Fint *info, MPI_Fint *n, MPI_Fint *pset_len, char *pset_name, MPI_Fint *ierr),
                            (session, info, n, pset_len, pset_name, ierr))
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SESSION_GET_NTH_PSET = ompi_session_get_nth_pset_f
#pragma weak mpi_session_get_nth_pset = ompi_session_get_nth_pset_f
#pragma weak mpi_session_get_nth_pset_ = ompi_session_get_nth_pset_f
#pragma weak mpi_session_get_nth_pset__ = ompi_session_get_nth_pset_f

#pragma weak MPI_Session_get_nth_pset_f = ompi_session_get_nth_pset_f
#pragma weak MPI_Session_get_nth_pset_f08 = ompi_session_get_nth_pset_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SESSION_GET_NTH_PSET,
                            mpi_session_get_nth_pset,
                            mpi_session_get_nth_pset_,
                            mpi_session_get_nth_pset__,
                            ompi_session_get_nth_pset_f,
                            (MPI_Fint *session, MPI_Fint *info, MPI_Fint *n, MPI_Fint *pset_len, char *pset_name, MPI_Fint *ierr),
                            (session, info, n, pset_len, pset_name, ierr))
#else
#define ompi_session_get_nth_pset_f pompi_session_get_nth_pset_f
#endif
#endif

void ompi_session_get_nth_pset_f(MPI_Fint *session, MPI_Fint *info, MPI_Fint *n, MPI_Fint *pset_len, char *pset_name, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Session c_session;
    char c_name[MPI_MAX_PSET_NAME_LEN];
    OMPI_SINGLE_NAME_DECL(pset_len);

    c_session = PMPI_Session_f2c(*session);

    if (0 == *pset_len) {
        c_ierr = PMPI_Session_get_nth_pset(c_session, MPI_INFO_NULL, *n,
                                           OMPI_SINGLE_NAME_CONVERT(pset_len),
                                           c_name);
        if (MPI_SUCCESS == c_ierr) {
            OMPI_SINGLE_INT_2_FINT(pset_len);
        }

    } else {
        c_ierr = PMPI_Session_get_nth_pset(c_session, MPI_INFO_NULL, *n,
                                           OMPI_SINGLE_NAME_CONVERT(pset_len),
                                           c_name);
        if (MPI_SUCCESS == c_ierr) {
            ompi_fortran_string_c2f(c_name, pset_name, *pset_len);
        }
    }

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

}

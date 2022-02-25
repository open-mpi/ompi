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
#include "ompi/mpi/fortran/base/fortran_base_strings.h"
#include "ompi/constants.h"
#include "ompi/instance/instance.h"


#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_SESSION_GET_PSET_INFO = ompi_session_get_pset_info_f
#pragma weak pmpi_session_get_pset_info = ompi_session_get_pset_info_f
#pragma weak pmpi_session_get_pset_info_ = ompi_session_get_pset_info_f
#pragma weak pmpi_session_get_pset_info__ = ompi_session_get_pset_info_f

#pragma weak PMPI_Session_get_pset_info_f = ompi_session_get_pset_info_f
#pragma weak PMPI_Session_get_pset_info_f08 = ompi_session_get_pset_info_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SESSION_GET_PSET_INFO,
                            pmpi_session_get_pset_info,
                            pmpi_session_get_pset_info_,
                            pmpi_session_get_pset_info__,
                            pompi_session_get_pset_info_f,
                            (MPI_Fint *session, char *pset_name, MPI_Fint *info, MPI_Fint *ierr, int name_len),
                            (session, pset_name, info, ierr, name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SESSION_GET_PSET_INFO = ompi_session_get_pset_info_f
#pragma weak mpi_session_get_pset_info = ompi_session_get_pset_info_f
#pragma weak mpi_session_get_pset_info_ = ompi_session_get_pset_info_f
#pragma weak mpi_session_get_pset_info__ = ompi_session_get_pset_info_f

#pragma weak MPI_Session_get_pset_info_f = ompi_session_get_pset_info_f
#pragma weak MPI_Session_get_pset_info_f08 = ompi_session_get_pset_info_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SESSION_GET_PSET_INFO,
                            mpi_session_get_pset_info,
                            mpi_session_get_pset_info_,
                            mpi_session_get_pset_info__,
                            ompi_session_get_pset_info_f,
                            (MPI_Fint *session, char *pset_name, MPI_Fint *info, MPI_Fint *ierr, int name_len),
                            (session, pset_name, info, ierr, name_len) )
#else
#define ompi_session_get_pset_info_f pompi_session_get_pset_info_f
#endif
#endif

void ompi_session_get_pset_info_f(MPI_Fint *session,char *pset_name, MPI_Fint *info, MPI_Fint *ierr, int name_len)
{
    int c_ierr, ret;
    MPI_Session c_session;
    char *c_name = NULL;
    MPI_Info c_info;

    c_session = PMPI_Session_f2c(*session);

    /* Convert the fortran string */

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(pset_name, name_len,
                                                       &c_name))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE((ompi_instance_t *)c_session, ret,
                                        "MPI_SESSION_GET_PSET_INFO");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = PMPI_Session_get_pset_info(c_session, c_name, &c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (NULL != c_name) {
        free (c_name);
    }

    if (MPI_SUCCESS == c_ierr) {
        *info = PMPI_Info_c2f(c_info);
    }
}



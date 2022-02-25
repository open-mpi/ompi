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
#pragma weak PMPI_GROUP_FROM_SESSION_PSET = ompi_group_from_session_pset_f
#pragma weak pmpi_group_from_session_pset = ompi_group_from_session_pset_f
#pragma weak pmpi_group_from_session_pset_ = ompi_group_from_session_pset_f
#pragma weak pmpi_group_from_session_pset__ = ompi_group_from_session_pset_f

#pragma weak PMPI_Group_from_session_pset_f = ompi_group_from_session_pset_f
#pragma weak PMPI_Group_from_session_pset_f08 = ompi_group_from_session_pset_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_FROM_SESSION_PSET,
                            pmpi_group_from_session_pset,
                            pmpi_group_from_session_pset_,
                            pmpi_group_from_session_pset__,
                            pompi_group_from_session_pset_f,
                            (MPI_Fint *session, char *pset_name, MPI_Fint *newgroup, MPI_Fint *ierr, int name_len),
                            (session, pset_name, newgroup, ierr, name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_FROM_SESSION_PSET = ompi_group_from_session_pset_f
#pragma weak mpi_group_from_session_pset = ompi_group_from_session_pset_f
#pragma weak mpi_group_from_session_pset_ = ompi_group_from_session_pset_f
#pragma weak mpi_group_from_session_pset__ = ompi_group_from_session_pset_f

#pragma weak MPI_Group_from_session_pset_f = ompi_group_from_session_pset_f
#pragma weak MPI_Group_from_session_pset_f08 = ompi_group_from_session_pset_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_FROM_SESSION_PSET,
                            mpi_group_from_session_pset,
                            mpi_group_from_session_pset_,
                            mpi_group_from_session_pset__,
                            ompi_group_from_session_pset_f,
                            (MPI_Fint *session, char *pset_name, MPI_Fint *newgroup, MPI_Fint *ierr, int name_len),
                            (session, pset_name, newgroup, ierr, name_len) )
#else
#define ompi_group_from_session_pset_f pompi_group_from_session_pset_f
#endif
#endif

void ompi_group_from_session_pset_f(MPI_Fint *session,char *pset_name, MPI_Fint *newgroup, MPI_Fint *ierr, int name_len)
{
    int c_ierr, ret;
    MPI_Session c_session;
    char *c_name;
    MPI_Group c_newgroup;

    c_session = PMPI_Session_f2c(*session);

    /* Convert the fortran string */

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(pset_name, name_len,
                                                       &c_name))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE((ompi_instance_t *)c_session, ret,
                                        "MPI_GROUP_FROM_SESSION_PSET");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = PMPI_Group_from_session_pset(c_session, c_name, &c_newgroup);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newgroup = PMPI_Group_c2f (c_newgroup);
    }

    /* Free the C name */

    free(c_name);
}



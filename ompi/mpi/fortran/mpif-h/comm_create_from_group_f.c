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
 * Copyright (c) 2019-2021 Triad National Security, LLC.  All rights reserved.
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
#include "ompi/group/group.h"


#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_COMM_CREATE_FROM_GROUP = ompi_comm_create_from_group_f
#pragma weak pmpi_comm_create_from_group = ompi_comm_create_from_group_f
#pragma weak pmpi_comm_create_from_group_ = ompi_comm_create_from_group_f
#pragma weak pmpi_comm_create_from_group__ = ompi_comm_create_from_group_f

#pragma weak PMPI_Comm_create_from_group_f = ompi_comm_create_from_group_f
#pragma weak PMPI_Comm_create_from_group_f08 = ompi_comm_create_from_group_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_CREATE_FROM_GROUP,
                            pmpi_comm_create_from_group,
                            pmpi_comm_create_from_group_,
                            pmpi_comm_create_from_group__,
                            pompi_comm_create_from_group_f,
                            (MPI_Fint *group, char *stringtag, MPI_Fint *info, MPI_Fint *errhandler, MPI_Fint *newcomm, MPI_Fint *ierr, int name_len),
                            (group, stringtag, info, errhandler, newcomm, ierr, name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CREATE_FROM_GROUP = ompi_comm_create_from_group_f
#pragma weak mpi_comm_create_from_group = ompi_comm_create_from_group_f
#pragma weak mpi_comm_create_from_group_ = ompi_comm_create_from_group_f
#pragma weak mpi_comm_create_from_group__ = ompi_comm_create_from_group_f

#pragma weak MPI_Comm_create_from_group_f = ompi_comm_create_from_group_f
#pragma weak MPI_Comm_create_from_group_f08 = ompi_comm_create_from_group_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_CREATE_FROM_GROUP,
                            mpi_comm_create_from_group,
                            mpi_comm_create_from_group_,
                            mpi_comm_create_from_group__,
                            ompi_comm_create_from_group_f,
                            (MPI_Fint *group, char *stringtag, MPI_Fint *info, MPI_Fint *errhandler, MPI_Fint *newcomm, MPI_Fint *ierr, int name_len),
                            (group, stringtag, info, errhandler, newcomm, ierr, name_len) )
#else
#define ompi_comm_create_from_group_f pompi_comm_create_from_group_f
#endif
#endif

void ompi_comm_create_from_group_f(MPI_Fint *group, char *stringtag, MPI_Fint *info, MPI_Fint *errhandler, MPI_Fint *newcomm, MPI_Fint *ierr, int name_len)
{
    int c_ierr, ret;
    MPI_Group c_group;
    char *c_tag;
    MPI_Comm c_comm;
    MPI_Info c_info;
    MPI_Errhandler c_err;

    c_group = PMPI_Group_f2c(*group);
    c_info = PMPI_Info_f2c(*info);
    c_err = PMPI_Errhandler_f2c(*errhandler);

    /* Convert the fortran string */

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(stringtag, name_len,
                                                       &c_tag))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(ompi_group_get_instance(c_group), ret, "MPI_COMM_CREATE_FROM_GROUP");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = PMPI_Comm_create_from_group(c_group, c_tag, c_info, c_err, &c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newcomm = PMPI_Comm_c2f (c_comm);
    }

    /* Free the C tag */

    free(c_tag);
}


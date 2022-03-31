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
#include "ompi/group/group.h"


#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_INTERCOMM_CREATE_FROM_GROUPS = ompi_intercomm_create_from_groups_f
#pragma weak pmpi_intercomm_create_from_groups = ompi_intercomm_create_from_groups_f
#pragma weak pmpi_intercomm_create_from_groups_ = ompi_intercomm_create_from_groups_f
#pragma weak pmpi_intercomm_create_from_groups__ = ompi_intercomm_create_from_groups_f

#pragma weak PMPI_Intercomm_create_from_groups_f = ompi_intercomm_create_from_groups_f
#pragma weak PMPI_Intercomm_create_from_groups_f08 = ompi_intercomm_create_from_groups_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INTERCOMM_CREATE_FROM_GROUPS,
                            pmpi_intercomm_create_from_groups,
                            pmpi_intercomm_create_from_groups_,
                            pmpi_intercomm_create_from_groups__,
                            pompi_intercomm_create_from_groups_f,
                            (MPI_Fint *local_group, MPI_Fint *local_leader, MPI_Fint *remote_group,
                             MPI_Fint *remote_leader, char *stringtag, MPI_Fint *info, MPI_Fint *errhandler,
                             MPI_Fint *internewcomm, MPI_Fint *ierr, int name_len),
                            (local_group, local_leader, remote_group,
                             remote_leader, stringtag, info, errhandler, internewcomm, ierr, name_len) )

#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INTERCOMM_CREATE_FROM_GROUPS = ompi_intercomm_create_from_groups_f
#pragma weak mpi_intercomm_create_from_groups = ompi_intercomm_create_from_groups_f
#pragma weak mpi_intercomm_create_from_groups_ = ompi_intercomm_create_from_groups_f
#pragma weak mpi_intercomm_create_from_groups__ = ompi_intercomm_create_from_groups_f

#pragma weak MPI_Intercomm_create_from_groups_f = ompi_intercomm_create_from_groups_f
#pragma weak MPI_Intercomm_create_from_groups_f08 = ompi_intercomm_create_from_groups_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INTERCOMM_CREATE_FROM_GROUPS,
                            mpi_intercomm_create_from_groups,
                            mpi_intercomm_create_from_groups_,
                            mpi_intercomm_create_from_groups__,
                            ompi_intercomm_create_from_groups_f,
                            (MPI_Fint *local_group, MPI_Fint *local_leader, MPI_Fint *remote_group,
                             MPI_Fint *remote_leader, char *stringtag, MPI_Fint *info, MPI_Fint *errhandler,
                             MPI_Fint *internewcomm, MPI_Fint *ierr, int name_len),
                            (local_group, local_leader, remote_group,
                             remote_leader, stringtag, info, errhandler, internewcomm, ierr, name_len) )
#else
#define ompi_intercomm_create_from_groups_f pompi_intercomm_create_from_groups_f
#endif
#endif

void ompi_intercomm_create_from_groups_f(MPI_Fint *local_group, MPI_Fint *local_leader, MPI_Fint *remote_group,
                                         MPI_Fint *remote_leader, char *stringtag, MPI_Fint *info, MPI_Fint *errhandler, 
                                         MPI_Fint *internewcomm, MPI_Fint *ierr, int name_len)
{
    int c_ierr, ret;
    MPI_Group c_lgroup, c_rgroup;
    char *c_tag;
    MPI_Comm c_intercomm;
    MPI_Info c_info;
    MPI_Errhandler c_err;

    c_lgroup = PMPI_Group_f2c(*local_group);
    c_rgroup = PMPI_Group_f2c(*remote_group);
    c_info = PMPI_Info_f2c(*info);
    c_err = PMPI_Errhandler_f2c(*errhandler);

    /* Convert the fortran string */

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(stringtag, name_len,
                                                       &c_tag))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(ompi_group_get_instance(c_lgroup), ret, "MPI_INTERCOMM_CREATE_FROM_GROUPS");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = PMPI_Intercomm_create_from_groups(c_lgroup,  OMPI_FINT_2_INT(*local_leader), 
                                               c_rgroup,  OMPI_FINT_2_INT(*remote_leader), 
                                               c_tag, c_info, c_err, &c_intercomm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *internewcomm = PMPI_Comm_c2f (c_intercomm);
    }

    /* Free the C tag */

    free(c_tag);
}

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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/group/group.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_GROUP_INCL = ompi_group_incl_f
#pragma weak pmpi_group_incl = ompi_group_incl_f
#pragma weak pmpi_group_incl_ = ompi_group_incl_f
#pragma weak pmpi_group_incl__ = ompi_group_incl_f

#pragma weak PMPI_Group_incl_f = ompi_group_incl_f
#pragma weak PMPI_Group_incl_f08 = ompi_group_incl_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_INCL,
                           pmpi_group_incl,
                           pmpi_group_incl_,
                           pmpi_group_incl__,
                           pompi_group_incl_f,
                           (MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranks, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group, n, ranks, newgroup, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_INCL = ompi_group_incl_f
#pragma weak mpi_group_incl = ompi_group_incl_f
#pragma weak mpi_group_incl_ = ompi_group_incl_f
#pragma weak mpi_group_incl__ = ompi_group_incl_f

#pragma weak MPI_Group_incl_f = ompi_group_incl_f
#pragma weak MPI_Group_incl_f08 = ompi_group_incl_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_INCL,
                           mpi_group_incl,
                           mpi_group_incl_,
                           mpi_group_incl__,
                           ompi_group_incl_f,
                           (MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranks, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group, n, ranks, newgroup, ierr) )
#else
#define ompi_group_incl_f pompi_group_incl_f
#endif
#endif


void ompi_group_incl_f(MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranks, MPI_Fint *newgroup, MPI_Fint *ierr)
{
    /* local variables */
    int c_ierr;
    ompi_group_t *c_group, *c_newgroup;
    OMPI_ARRAY_NAME_DECL(ranks);

    /* make the fortran to c representation conversion */
    c_group = PMPI_Group_f2c(*group);

    OMPI_ARRAY_FINT_2_INT(ranks, *n);
    c_ierr = PMPI_Group_incl(c_group,
                            OMPI_FINT_2_INT(*n),
                            OMPI_ARRAY_NAME_CONVERT(ranks),
                            &c_newgroup);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    /* translate the results from c to fortran */
    if (MPI_SUCCESS == c_ierr) {
        *newgroup = c_newgroup->grp_f_to_c_index;
    }
}

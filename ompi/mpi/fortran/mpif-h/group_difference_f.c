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
#pragma weak PMPI_GROUP_DIFFERENCE = ompi_group_difference_f
#pragma weak pmpi_group_difference = ompi_group_difference_f
#pragma weak pmpi_group_difference_ = ompi_group_difference_f
#pragma weak pmpi_group_difference__ = ompi_group_difference_f

#pragma weak PMPI_Group_difference_f = ompi_group_difference_f
#pragma weak PMPI_Group_difference_f08 = ompi_group_difference_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_DIFFERENCE,
                           pmpi_group_difference,
                           pmpi_group_difference_,
                           pmpi_group_difference__,
                           pompi_group_difference_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_DIFFERENCE = ompi_group_difference_f
#pragma weak mpi_group_difference = ompi_group_difference_f
#pragma weak mpi_group_difference_ = ompi_group_difference_f
#pragma weak mpi_group_difference__ = ompi_group_difference_f

#pragma weak MPI_Group_difference_f = ompi_group_difference_f
#pragma weak MPI_Group_difference_f08 = ompi_group_difference_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_DIFFERENCE,
                           mpi_group_difference,
                           mpi_group_difference_,
                           mpi_group_difference__,
                           ompi_group_difference_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#else
#define ompi_group_difference_f pompi_group_difference_f
#endif
#endif


void ompi_group_difference_f(MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr)
{
  int c_ierr;
  ompi_group_t *c_group1, *c_group2, *c_newgroup;

  /* Make the fortran to c representation conversion */
  c_group1 = PMPI_Group_f2c(*group1);
  c_group2 = PMPI_Group_f2c(*group2);

  c_ierr = OMPI_FORTRAN_FPTR(MPI_Group_difference)(c_group1, c_group2, &c_newgroup);
  if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

  /* translate the results from c to fortran */
  if (MPI_SUCCESS == c_ierr) {
    *newgroup = c_newgroup->grp_f_to_c_index;
  }
}

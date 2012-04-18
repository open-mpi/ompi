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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/group/group.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_INTERSECTION = ompi_group_intersection_f
#pragma weak pmpi_group_intersection = ompi_group_intersection_f
#pragma weak pmpi_group_intersection_ = ompi_group_intersection_f
#pragma weak pmpi_group_intersection__ = ompi_group_intersection_f

#pragma weak PMPI_Group_intersection_f = ompi_group_intersection_f
#pragma weak PMPI_Group_intersection_f08 = ompi_group_intersection_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_INTERSECTION,
                           pmpi_group_intersection,
                           pmpi_group_intersection_,
                           pmpi_group_intersection__,
                           pompi_group_intersection_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_INTERSECTION = ompi_group_intersection_f
#pragma weak mpi_group_intersection = ompi_group_intersection_f
#pragma weak mpi_group_intersection_ = ompi_group_intersection_f
#pragma weak mpi_group_intersection__ = ompi_group_intersection_f

#pragma weak MPI_Group_intersection_f = ompi_group_intersection_f
#pragma weak MPI_Group_intersection_f08 = ompi_group_intersection_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_INTERSECTION,
                           mpi_group_intersection,
                           mpi_group_intersection_,
                           mpi_group_intersection__,
                           ompi_group_intersection_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_group_intersection_f(MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr)
{
  int c_ierr;
  ompi_group_t *c_group1, *c_group2, *c_newgroup;

  /* Make the fortran to c representation conversion */
  c_group1 = MPI_Group_f2c(*group1);
  c_group2 = MPI_Group_f2c(*group2);
  
  c_ierr = MPI_Group_intersection(c_group1, c_group2, &c_newgroup);
  if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

  /* translate the results from c to fortran */
  if (MPI_SUCCESS == c_ierr) {
      *newgroup = c_newgroup->grp_f_to_c_index;
  }
}

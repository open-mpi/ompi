/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "group/group.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_DIFFERENCE = mpi_group_difference_f
#pragma weak pmpi_group_difference = mpi_group_difference_f
#pragma weak pmpi_group_difference_ = mpi_group_difference_f
#pragma weak pmpi_group_difference__ = mpi_group_difference_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_DIFFERENCE,
                           pmpi_group_difference,
                           pmpi_group_difference_,
                           pmpi_group_difference__,
                           pmpi_group_difference_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_DIFFERENCE = mpi_group_difference_f
#pragma weak mpi_group_difference = mpi_group_difference_f
#pragma weak mpi_group_difference_ = mpi_group_difference_f
#pragma weak mpi_group_difference__ = mpi_group_difference_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_DIFFERENCE,
                           mpi_group_difference,
                           mpi_group_difference_,
                           mpi_group_difference__,
                           mpi_group_difference_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_group_difference_f(MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr)
{
  ompi_group_t *c_group1, *c_group2, *c_newgroup;

  /* Make the fortran to c representation conversion */
  c_group1 = MPI_Group_f2c(*group1);
  c_group2 = MPI_Group_f2c(*group2);
  
  *ierr = OMPI_INT_2_FINT(MPI_Group_difference(c_group1, c_group2, 
					       &c_newgroup));

  /* translate the results from c to fortran */
  if (MPI_SUCCESS == *ierr) {
    *newgroup = c_newgroup->grp_f_to_c_index; 
  }
}

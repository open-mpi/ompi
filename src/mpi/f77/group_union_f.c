/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "group/group.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_UNION = mpi_group_union_f
#pragma weak pmpi_group_union = mpi_group_union_f
#pragma weak pmpi_group_union_ = mpi_group_union_f
#pragma weak pmpi_group_union__ = mpi_group_union_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_UNION,
                           pmpi_group_union,
                           pmpi_group_union_,
                           pmpi_group_union__,
                           pmpi_group_union_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_UNION = mpi_group_union_f
#pragma weak mpi_group_union = mpi_group_union_f
#pragma weak mpi_group_union_ = mpi_group_union_f
#pragma weak mpi_group_union__ = mpi_group_union_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_UNION,
                           mpi_group_union,
                           mpi_group_union_,
                           mpi_group_union__,
                           mpi_group_union_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_group_union_f(MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr)
{
  ompi_group_t *c_group1, *c_group2, *c_newgroup;

  /* Make the fortran to c representation conversion */
  c_group1 = MPI_Group_f2c(*group1);
  c_group2 = MPI_Group_f2c(*group2);
  
  *ierr = OMPI_INT_2_FINT(MPI_Group_union(c_group1, c_group2, &c_newgroup));

  /* translate the results from c to fortran */
  *newgroup = c_newgroup->grp_f_to_c_index;
}

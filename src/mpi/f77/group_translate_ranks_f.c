/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "group/group.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_TRANSLATE_RANKS = mpi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks = mpi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks_ = mpi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks__ = mpi_group_translate_ranks_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_TRANSLATE_RANKS,
                           pmpi_group_translate_ranks,
                           pmpi_group_translate_ranks_,
                           pmpi_group_translate_ranks__,
                           pmpi_group_translate_ranks_f,
                           (MPI_Fint *group1, MPI_Fint *n, MPI_Fint *ranks1, MPI_Fint *group2, MPI_Fint *ranks2, MPI_Fint *ierr),
                           (group1, n, ranks1, group2, ranks2, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_TRANSLATE_RANKS = mpi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks = mpi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks_ = mpi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks__ = mpi_group_translate_ranks_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_TRANSLATE_RANKS,
                           mpi_group_translate_ranks,
                           mpi_group_translate_ranks_,
                           mpi_group_translate_ranks__,
                           mpi_group_translate_ranks_f,
                           (MPI_Fint *group1, MPI_Fint *n, MPI_Fint *ranks1, MPI_Fint *group2, MPI_Fint *ranks2, MPI_Fint *ierr),
                           (group1, n, ranks1, group2, ranks2, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_group_translate_ranks_f(MPI_Fint *group1, MPI_Fint *n, 
				  MPI_Fint *ranks1, MPI_Fint *group2,
				  MPI_Fint *ranks2, MPI_Fint *ierr)
{
  ompi_group_t *c_group1, *c_group2;
  OMPI_ARRAY_NAME_DECL(ranks1);
  OMPI_ARRAY_NAME_DECL(ranks2);

  /* Make the fortran to c representation conversion */
  c_group1 = MPI_Group_f2c(*group1);
  c_group2 = MPI_Group_f2c(*group2);

  OMPI_ARRAY_FINT_2_INT(ranks1, *n);
  OMPI_ARRAY_FINT_2_INT_ALLOC(ranks2, *n);

  *ierr = OMPI_INT_2_FINT(MPI_Group_translate_ranks(c_group1, 
						    OMPI_FINT_2_INT(*n),
						    OMPI_ARRAY_NAME_CONVERT(ranks1), 
						    c_group2, 
						    OMPI_ARRAY_NAME_CONVERT(ranks2)
						    ));

  OMPI_ARRAY_FINT_2_INT_CLEANUP(ranks1);
  OMPI_ARRAY_INT_2_FINT(ranks2, *n);
}

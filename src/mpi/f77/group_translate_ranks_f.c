/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_TRANSLATE_RANKS = mpi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks = mpi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks_ = mpi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks__ = mpi_group_translate_ranks_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_TRANSLATE_RANKS,
                           pmpi_group_translate_ranks,
                           pmpi_group_translate_ranks_,
                           pmpi_group_translate_ranks__,
                           pmpi_group_translate_ranks_f,
                           (MPI_Fint *group1, MPI_Fint *n, MPI_Fint *ranks1, MPI_Fint *group2, MPI_Fint *ranks2, MPI_Fint *ierr),
                           (group1, n, ranks1, group2, ranks2, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_TRANSLATE_RANKS = mpi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks = mpi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks_ = mpi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks__ = mpi_group_translate_ranks_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GROUP_TRANSLATE_RANKS,
                           mpi_group_translate_ranks,
                           mpi_group_translate_ranks_,
                           mpi_group_translate_ranks__,
                           mpi_group_translate_ranks_f,
                           (MPI_Fint *group1, MPI_Fint *n, MPI_Fint *ranks1, MPI_Fint *group2, MPI_Fint *ranks2, MPI_Fint *ierr),
                           (group1, n, ranks1, group2, ranks2, ierr) )
#endif

void mpi_group_translate_ranks_f(MPI_Fint *group1, MPI_Fint *n, MPI_Fint *ranks1, MPI_Fint *group2, MPI_Fint *ranks2, MPI_Fint *ierr)
{

}

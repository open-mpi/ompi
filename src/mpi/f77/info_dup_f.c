/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INFO_DUP = mpi_info_dup_f
#pragma weak pmpi_info_dup = mpi_info_dup_f
#pragma weak pmpi_info_dup_ = mpi_info_dup_f
#pragma weak pmpi_info_dup__ = mpi_info_dup_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INFO_DUP,
                           pmpi_info_dup,
                           pmpi_info_dup_,
                           pmpi_info_dup__,
                           pmpi_info_dup_f,
                           (MPI_Fint *info, MPI_Fint *newinfo, MPI_Fint *ierr),
                           (info, newinfo, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_DUP = mpi_info_dup_f
#pragma weak mpi_info_dup = mpi_info_dup_f
#pragma weak mpi_info_dup_ = mpi_info_dup_f
#pragma weak mpi_info_dup__ = mpi_info_dup_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INFO_DUP,
                           mpi_info_dup,
                           mpi_info_dup_,
                           mpi_info_dup__,
                           mpi_info_dup_f,
                           (MPI_Fint *info, MPI_Fint *newinfo, MPI_Fint *ierr),
                           (info, newinfo, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_info_dup_f(MPI_Fint *info, MPI_Fint *newinfo, MPI_Fint *ierr)
{

}

/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INFO_DUP = mpi_info_dup_f
#pragma weak pmpi_info_dup = mpi_info_dup_f
#pragma weak pmpi_info_dup_ = mpi_info_dup_f
#pragma weak pmpi_info_dup__ = mpi_info_dup_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_DUP,
                           pmpi_info_dup,
                           pmpi_info_dup_,
                           pmpi_info_dup__,
                           pmpi_info_dup_f,
                           (MPI_Fint *info, MPI_Fint *newinfo, MPI_Fint *ierr),
                           (info, newinfo, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_DUP = mpi_info_dup_f
#pragma weak mpi_info_dup = mpi_info_dup_f
#pragma weak mpi_info_dup_ = mpi_info_dup_f
#pragma weak mpi_info_dup__ = mpi_info_dup_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_DUP,
                           mpi_info_dup,
                           mpi_info_dup_,
                           mpi_info_dup__,
                           mpi_info_dup_f,
                           (MPI_Fint *info, MPI_Fint *newinfo, MPI_Fint *ierr),
                           (info, newinfo, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_info_dup_f(MPI_Fint *info, MPI_Fint *newinfo, MPI_Fint *ierr)
{

}

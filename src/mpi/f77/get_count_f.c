/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GET_COUNT = mpi_get_count_f
#pragma weak pmpi_get_count = mpi_get_count_f
#pragma weak pmpi_get_count_ = mpi_get_count_f
#pragma weak pmpi_get_count__ = mpi_get_count_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GET_COUNT,
                           pmpi_get_count,
                           pmpi_get_count_,
                           pmpi_get_count__,
                           pmpi_get_count_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_COUNT = mpi_get_count_f
#pragma weak mpi_get_count = mpi_get_count_f
#pragma weak mpi_get_count_ = mpi_get_count_f
#pragma weak mpi_get_count__ = mpi_get_count_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GET_COUNT,
                           mpi_get_count,
                           mpi_get_count_,
                           mpi_get_count__,
                           mpi_get_count_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_get_count_f(MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr)
{

}

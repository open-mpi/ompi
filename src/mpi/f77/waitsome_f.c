/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WAITSOME = mpi_waitsome_f
#pragma weak pmpi_waitsome = mpi_waitsome_f
#pragma weak pmpi_waitsome_ = mpi_waitsome_f
#pragma weak pmpi_waitsome__ = mpi_waitsome_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WAITSOME,
                           pmpi_waitsome,
                           pmpi_waitsome_,
                           pmpi_waitsome__,
                           pmpi_waitsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITSOME = mpi_waitsome_f
#pragma weak mpi_waitsome = mpi_waitsome_f
#pragma weak mpi_waitsome_ = mpi_waitsome_f
#pragma weak mpi_waitsome__ = mpi_waitsome_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WAITSOME,
                           mpi_waitsome,
                           mpi_waitsome_,
                           mpi_waitsome__,
                           mpi_waitsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_waitsome_f(MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{

}

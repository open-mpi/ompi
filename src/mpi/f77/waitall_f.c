/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WAITALL = mpi_waitall_f
#pragma weak pmpi_waitall = mpi_waitall_f
#pragma weak pmpi_waitall_ = mpi_waitall_f
#pragma weak pmpi_waitall__ = mpi_waitall_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAITALL,
                           pmpi_waitall,
                           pmpi_waitall_,
                           pmpi_waitall__,
                           pmpi_waitall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, array_of_statuses, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITALL = mpi_waitall_f
#pragma weak mpi_waitall = mpi_waitall_f
#pragma weak mpi_waitall_ = mpi_waitall_f
#pragma weak mpi_waitall__ = mpi_waitall_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAITALL,
                           mpi_waitall,
                           mpi_waitall_,
                           mpi_waitall__,
                           mpi_waitall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, array_of_statuses, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_waitall_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{

}

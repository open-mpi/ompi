/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_STARTALL = mpi_startall_f
#pragma weak pmpi_startall = mpi_startall_f
#pragma weak pmpi_startall_ = mpi_startall_f
#pragma weak pmpi_startall__ = mpi_startall_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_STARTALL,
                           pmpi_startall,
                           pmpi_startall_,
                           pmpi_startall__,
                           pmpi_startall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *ierr),
                           (count, array_of_requests, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STARTALL = mpi_startall_f
#pragma weak mpi_startall = mpi_startall_f
#pragma weak mpi_startall_ = mpi_startall_f
#pragma weak mpi_startall__ = mpi_startall_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_STARTALL,
                           mpi_startall,
                           mpi_startall_,
                           mpi_startall__,
                           mpi_startall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *ierr),
                           (count, array_of_requests, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_startall_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *ierr)
{

}

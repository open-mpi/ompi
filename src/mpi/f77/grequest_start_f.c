/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GREQUEST_START = mpi_grequest_start_f
#pragma weak pmpi_grequest_start = mpi_grequest_start_f
#pragma weak pmpi_grequest_start_ = mpi_grequest_start_f
#pragma weak pmpi_grequest_start__ = mpi_grequest_start_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GREQUEST_START,
                           pmpi_grequest_start,
                           pmpi_grequest_start_,
                           pmpi_grequest_start__,
                           pmpi_grequest_start_f,
                           (MPI_Fint *query_fn, MPI_Fint *free_fn, MPI_Fint *cancel_fn, char *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GREQUEST_START = mpi_grequest_start_f
#pragma weak mpi_grequest_start = mpi_grequest_start_f
#pragma weak mpi_grequest_start_ = mpi_grequest_start_f
#pragma weak mpi_grequest_start__ = mpi_grequest_start_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GREQUEST_START,
                           mpi_grequest_start,
                           mpi_grequest_start_,
                           mpi_grequest_start__,
                           mpi_grequest_start_f,
                           (MPI_Fint *query_fn, MPI_Fint *free_fn, MPI_Fint *cancel_fn, char *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_grequest_start_f(MPI_Fint *query_fn, MPI_Fint *free_fn, MPI_Fint *cancel_fn, char *extra_state, MPI_Fint *request, MPI_Fint *ierr)
{

}

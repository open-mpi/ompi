/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_QUERY_THREAD = mpi_query_thread_f
#pragma weak pmpi_query_thread = mpi_query_thread_f
#pragma weak pmpi_query_thread_ = mpi_query_thread_f
#pragma weak pmpi_query_thread__ = mpi_query_thread_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_QUERY_THREAD,
                           pmpi_query_thread,
                           pmpi_query_thread_,
                           pmpi_query_thread__,
                           pmpi_query_thread_f,
                           (MPI_Fint *provided, MPI_Fint *ierr),
                           (provided, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_QUERY_THREAD = mpi_query_thread_f
#pragma weak mpi_query_thread = mpi_query_thread_f
#pragma weak mpi_query_thread_ = mpi_query_thread_f
#pragma weak mpi_query_thread__ = mpi_query_thread_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_QUERY_THREAD,
                           mpi_query_thread,
                           mpi_query_thread_,
                           mpi_query_thread__,
                           mpi_query_thread_f,
                           (MPI_Fint *provided, MPI_Fint *ierr),
                           (provided, ierr) )
#endif

void mpi_query_thread_f(MPI_Fint *provided, MPI_Fint *ierr)
{

}

/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INIT_THREAD = mpi_init_thread_f
#pragma weak pmpi_init_thread = mpi_init_thread_f
#pragma weak pmpi_init_thread_ = mpi_init_thread_f
#pragma weak pmpi_init_thread__ = mpi_init_thread_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INIT_THREAD,
                           pmpi_init_thread,
                           pmpi_init_thread_,
                           pmpi_init_thread__,
                           pmpi_init_thread_f,
                           (MPI_Fint *argc, char *argv, MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (argc, argv, required, provided, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT_THREAD = mpi_init_thread_f
#pragma weak mpi_init_thread = mpi_init_thread_f
#pragma weak mpi_init_thread_ = mpi_init_thread_f
#pragma weak mpi_init_thread__ = mpi_init_thread_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INIT_THREAD,
                           mpi_init_thread,
                           mpi_init_thread_,
                           mpi_init_thread__,
                           mpi_init_thread_f,
                           (MPI_Fint *argc, char *argv, MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (argc, argv, required, provided, ierr) )
#endif

void mpi_init_thread_f(MPI_Fint *argc, char *argv, MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr)
{

}

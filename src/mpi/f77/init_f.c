/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INIT = mpi_init_f
#pragma weak pmpi_init = mpi_init_f
#pragma weak pmpi_init_ = mpi_init_f
#pragma weak pmpi_init__ = mpi_init_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INIT,
                           pmpi_init,
                           pmpi_init_,
                           pmpi_init__,
                           pmpi_init_f,
                           (MPI_Fint *argc, char *argv, MPI_Fint *ierr),
                           (argc, argv, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT = mpi_init_f
#pragma weak mpi_init = mpi_init_f
#pragma weak mpi_init_ = mpi_init_f
#pragma weak mpi_init__ = mpi_init_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INIT,
                           mpi_init,
                           mpi_init_,
                           mpi_init__,
                           mpi_init_f,
                           (MPI_Fint *argc, char *argv, MPI_Fint *ierr),
                           (argc, argv, ierr) )
#endif

void mpi_init_f(MPI_Fint *argc, char *argv, MPI_Fint *ierr)
{

}

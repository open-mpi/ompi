/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_KEYVAL_FREE = mpi_keyval_free_f
#pragma weak pmpi_keyval_free = mpi_keyval_free_f
#pragma weak pmpi_keyval_free_ = mpi_keyval_free_f
#pragma weak pmpi_keyval_free__ = mpi_keyval_free_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_KEYVAL_FREE,
                           pmpi_keyval_free,
                           pmpi_keyval_free_,
                           pmpi_keyval_free__,
                           pmpi_keyval_free_f,
                           (MPI_Fint *keyval, MPI_Fint *ierr),
                           (keyval, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_KEYVAL_FREE = mpi_keyval_free_f
#pragma weak mpi_keyval_free = mpi_keyval_free_f
#pragma weak mpi_keyval_free_ = mpi_keyval_free_f
#pragma weak mpi_keyval_free__ = mpi_keyval_free_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_KEYVAL_FREE,
                           mpi_keyval_free,
                           mpi_keyval_free_,
                           mpi_keyval_free__,
                           mpi_keyval_free_f,
                           (MPI_Fint *keyval, MPI_Fint *ierr),
                           (keyval, ierr) )
#endif

void mpi_keyval_free_f(MPI_Fint *keyval, MPI_Fint *ierr)
{

}

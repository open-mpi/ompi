/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_KEYVAL_CREATE = mpi_keyval_create_f
#pragma weak pmpi_keyval_create = mpi_keyval_create_f
#pragma weak pmpi_keyval_create_ = mpi_keyval_create_f
#pragma weak pmpi_keyval_create__ = mpi_keyval_create_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_KEYVAL_CREATE,
                           pmpi_keyval_create,
                           pmpi_keyval_create_,
                           pmpi_keyval_create__,
                           pmpi_keyval_create_f,
                           (MPI_Fint *copy_fn, MPI_Fint *delete_fn, MPI_Fint *keyval, char *extra_state, MPI_Fint *ierr),
                           (copy_fn, delete_fn, keyval, extra_state, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_KEYVAL_CREATE = mpi_keyval_create_f
#pragma weak mpi_keyval_create = mpi_keyval_create_f
#pragma weak mpi_keyval_create_ = mpi_keyval_create_f
#pragma weak mpi_keyval_create__ = mpi_keyval_create_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_KEYVAL_CREATE,
                           mpi_keyval_create,
                           mpi_keyval_create_,
                           mpi_keyval_create__,
                           mpi_keyval_create_f,
                           (MPI_Fint *copy_fn, MPI_Fint *delete_fn, MPI_Fint *keyval, char *extra_state, MPI_Fint *ierr),
                           (copy_fn, delete_fn, keyval, extra_state, ierr) )
#endif

void mpi_keyval_create_f(MPI_Fint *copy_fn, MPI_Fint *delete_fn, MPI_Fint *keyval, char *extra_state, MPI_Fint *ierr)
{

}

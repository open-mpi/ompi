/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_CREATE_KEYVAL = mpi_comm_create_keyval_f
#pragma weak pmpi_comm_create_keyval = mpi_comm_create_keyval_f
#pragma weak pmpi_comm_create_keyval_ = mpi_comm_create_keyval_f
#pragma weak pmpi_comm_create_keyval__ = mpi_comm_create_keyval_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_CREATE_KEYVAL,
                           pmpi_comm_create_keyval,
                           pmpi_comm_create_keyval_,
                           pmpi_comm_create_keyval__,
                           pmpi_comm_create_keyval_f,
                           (MPI_Fint *comm_copy_attr_fn, MPI_Fint *comm_delete_attr_fn, MPI_Fint *comm_keyval, char *extra_state, MPI_Fint *ierr),
                           (comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CREATE_KEYVAL = mpi_comm_create_keyval_f
#pragma weak mpi_comm_create_keyval = mpi_comm_create_keyval_f
#pragma weak mpi_comm_create_keyval_ = mpi_comm_create_keyval_f
#pragma weak mpi_comm_create_keyval__ = mpi_comm_create_keyval_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_CREATE_KEYVAL,
                           mpi_comm_create_keyval,
                           mpi_comm_create_keyval_,
                           mpi_comm_create_keyval__,
                           mpi_comm_create_keyval_f,
                           (MPI_Fint *comm_copy_attr_fn, MPI_Fint *comm_delete_attr_fn, MPI_Fint *comm_keyval, char *extra_state, MPI_Fint *ierr),
                           (comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_create_keyval_f(MPI_Fint *comm_copy_attr_fn, MPI_Fint *comm_delete_attr_fn, MPI_Fint *comm_keyval, char *extra_state, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}

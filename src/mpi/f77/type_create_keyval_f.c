/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_KEYVAL = mpi_type_create_keyval_f
#pragma weak pmpi_type_create_keyval = mpi_type_create_keyval_f
#pragma weak pmpi_type_create_keyval_ = mpi_type_create_keyval_f
#pragma weak pmpi_type_create_keyval__ = mpi_type_create_keyval_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_KEYVAL,
                           pmpi_type_create_keyval,
                           pmpi_type_create_keyval_,
                           pmpi_type_create_keyval__,
                           pmpi_type_create_keyval_f,
                           (MPI_Fint *type_copy_attr_fn, MPI_Fint *type_delete_attr_fn, MPI_Fint *type_keyval, char *extra_state, MPI_Fint *ierr),
                           (type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_KEYVAL = mpi_type_create_keyval_f
#pragma weak mpi_type_create_keyval = mpi_type_create_keyval_f
#pragma weak mpi_type_create_keyval_ = mpi_type_create_keyval_f
#pragma weak mpi_type_create_keyval__ = mpi_type_create_keyval_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_KEYVAL,
                           mpi_type_create_keyval,
                           mpi_type_create_keyval_,
                           mpi_type_create_keyval__,
                           mpi_type_create_keyval_f,
                           (MPI_Fint *type_copy_attr_fn, MPI_Fint *type_delete_attr_fn, MPI_Fint *type_keyval, char *extra_state, MPI_Fint *ierr),
                           (type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_create_keyval_f(MPI_Fint *type_copy_attr_fn, MPI_Fint *type_delete_attr_fn, MPI_Fint *type_keyval, char *extra_state, MPI_Fint *ierr)
{

}

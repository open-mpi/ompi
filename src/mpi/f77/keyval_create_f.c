/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi/f77/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_KEYVAL_CREATE = mpi_keyval_create_f
#pragma weak pmpi_keyval_create = mpi_keyval_create_f
#pragma weak pmpi_keyval_create_ = mpi_keyval_create_f
#pragma weak pmpi_keyval_create__ = mpi_keyval_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_KEYVAL_CREATE,
                           pmpi_keyval_create,
                           pmpi_keyval_create_,
                           pmpi_keyval_create__,
                           pmpi_keyval_create_f,
                           (MPI_F_copy_function* copy_fn, MPI_F_delete_function* delete_fn, MPI_Fint *keyval, char *extra_state, MPI_Fint *ierr),
                           (copy_fn, delete_fn, keyval, extra_state, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_KEYVAL_CREATE = mpi_keyval_create_f
#pragma weak mpi_keyval_create = mpi_keyval_create_f
#pragma weak mpi_keyval_create_ = mpi_keyval_create_f
#pragma weak mpi_keyval_create__ = mpi_keyval_create_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_KEYVAL_CREATE,
                           mpi_keyval_create,
                           mpi_keyval_create_,
                           mpi_keyval_create__,
                           mpi_keyval_create_f,
                           (MPI_F_copy_function* copy_fn, MPI_F_delete_function* delete_fn, MPI_Fint *keyval, MPI_Fint *extra_state, MPI_Fint *ierr),
                           (copy_fn, delete_fn, keyval, extra_state, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_keyval_create_f";

void mpi_keyval_create_f(MPI_F_copy_function* copy_attr_fn,
                         MPI_F_delete_function* delete_attr_fn,
			 MPI_Fint *keyval, MPI_Fint *extra_state, MPI_Fint *ierr)
{
    int ret, c_err;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t del_fn;

    copy_fn.attr_F_copy_fn = copy_attr_fn;
    del_fn.attr_F_delete_fn = delete_attr_fn;

    ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, del_fn,
                                  keyval, extra_state, OMPI_KEYVAL_F77);

    if (MPI_SUCCESS != ret) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
				       MPI_ERR_OTHER,
				       FUNC_NAME);
	*ierr = OMPI_INT_2_FINT(c_err);
    } else {
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
    }
}

/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"
#include "attribute/attribute.h"
#include "communicator/communicator.h"

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
                           (void *comm_copy_attr_fn, void *comm_delete_attr_fn, MPI_Fint *comm_keyval, char *extra_state, MPI_Fint *ierr),
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
                           (void *comm_copy_attr_fn, void *comm_delete_attr_fn, MPI_Fint *comm_keyval, char *extra_state, MPI_Fint *ierr),
                           (comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

     static const char FUNC_NAME[] = "MPI_Comm_create_keyval_f";

void mpi_comm_create_keyval_f(void *comm_copy_attr_fn,
			      void *comm_delete_attr_fn,
			      MPI_Fint *comm_keyval,
			      char *extra_state, MPI_Fint *ierr)
{
    int ret, c_err;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t del_fn;

    /* See the note in src/mpi/f77/prototypes_mpi.h about the use of
       (void*) for function pointers in this function */

    copy_fn.attr_F_copy_fn = (MPI_F_copy_function *) comm_copy_attr_fn;
    del_fn.attr_F_delete_fn = (MPI_F_delete_function *) comm_delete_attr_fn;

    ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, del_fn,
                                  comm_keyval, extra_state, OMPI_KEYVAL_F77);

    if (MPI_SUCCESS != ret) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
				       MPI_ERR_OTHER,
				       FUNC_NAME)
	*ierr = OMPI_INT_2_FINT(c_err);
    } else {
        *ierr = MPI_SUCCESS;
    }
}

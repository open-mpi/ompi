/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "attribute/attribute.h"

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
#include "mpi/f77/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Type_create_keyval_f";

void mpi_type_create_keyval_f(MPI_Fint *type_copy_attr_fn, MPI_Fint *type_delete_attr_fn, MPI_Fint *type_keyval, char *extra_state, MPI_Fint *ierr)
{
    int ret;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t del_fn;

    if (MPI_PARAM_CHECK) {
        if ((NULL == type_copy_attr_fn)   || 
            (NULL == type_delete_attr_fn) ||
            (NULL == type_keyval)              ) {
            *ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
                                           MPI_ERR_ARG,
                                           FUNC_NAME);
        }
    }

    copy_fn.attr_F_copy_fn = (MPI_F_copy_function *)type_copy_attr_fn;
    del_fn.attr_F_delete_fn = (MPI_F_delete_function *)type_delete_attr_fn;

    ret = ompi_attr_create_keyval(TYPE_ATTR, copy_fn, del_fn,
                                  type_keyval, extra_state, OMPI_KEYVAL_F77);

    if (MPI_SUCCESS != ret) {
        *ierr = OMPI_INT_2_FINT(OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
						       MPI_ERR_OTHER,
						       FUNC_NAME))
    } else {
        *ierr = MPI_SUCCESS;
    }
}

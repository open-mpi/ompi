/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_keyval = PMPI_Type_create_keyval
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Type_create_keyval";

int
MPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn,
                       MPI_Type_delete_attr_function *type_delete_attr_fn,
                       int *type_keyval,
                       void *extra_state)
{
    int ret;
    lam_attribute_fn_ptr_union_t copy_fn;
    lam_attribute_fn_ptr_union_t del_fn;

    if (MPI_PARAM_CHECK) {
	if ((NULL == type_copy_attr_fn) || (NULL == type_delete_attr_fn) ||
	    (NULL == type_keyval)) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
					 FUNC_NAME);
	}
    }
	
    copy_fn.attr_datatype_copy_fn = type_copy_attr_fn;
    del_fn.attr_datatype_delete_fn = type_delete_attr_fn;

    ret = lam_attr_create_keyval(TYPE_ATTR, copy_fn, del_fn,
				 type_keyval, extra_state, 0);

    LAM_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}



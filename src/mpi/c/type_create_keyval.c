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

int
MPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn,
                       MPI_Type_delete_attr_function *type_delete_attr_fn,
                       int *type_keyval,
                       void *extra_state)
{
    int ret;

    if ((NULL == type_copy_attr_fn) || (NULL == type_delete_attr_fn) ||
	(NULL == type_keyval))
	return MPI_ERR_ARG;

    ret = lam_attr_create_keyval(TYPE_ATTR, (void *)type_copy_attr_fn, 
				 (void *)type_delete_attr_fn,
				 type_keyval, extra_state, 0);

    /* Error handling code here */

    return ret;
}



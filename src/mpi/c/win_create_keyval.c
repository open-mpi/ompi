/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_create_keyval = PMPI_Win_create_keyval
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_create_keyval";


OMPI_EXPORT
int MPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,
                          MPI_Win_delete_attr_function *win_delete_attr_fn,
                          int *win_keyval, void *extra_state) 
{
    int ret;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t del_fn;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
	if ((NULL == win_copy_attr_fn) || (NULL == win_delete_attr_fn) ||
	    (NULL == win_keyval)) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
					 FUNC_NAME);
	}
    }
    copy_fn.attr_win_copy_fn = win_copy_attr_fn;
    del_fn.attr_win_delete_fn = win_delete_attr_fn;

    ret = ompi_attr_create_keyval(WIN_ATTR, copy_fn, del_fn,
				 win_keyval, extra_state, 0);
    OMPI_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}

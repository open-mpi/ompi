/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_create_keyval = PMPI_Win_create_keyval
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Win_create_keyval";

int MPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,
                          MPI_Win_delete_attr_function *win_delete_attr_fn,
                          int *win_keyval, void *extra_state) {

    int ret;
    lam_attribute_fn_ptr_union_t copy_fn;
    lam_attribute_fn_ptr_union_t del_fn;

    if (MPI_PARAM_CHECK) {
	if ((NULL == win_copy_attr_fn) || (NULL == win_delete_attr_fn) ||
	    (NULL == win_keyval)) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
					 FUNC_NAME);
	}
    }
    copy_fn.attr_win_copy_fn = win_copy_attr_fn;
    del_fn.attr_win_delete_fn = win_delete_attr_fn;

    ret = lam_attr_create_keyval(WIN_ATTR, copy_fn, del_fn,
				 win_keyval, extra_state, 0);

    LAM_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}

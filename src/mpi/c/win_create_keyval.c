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

int MPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,
                          MPI_Win_delete_attr_function *win_delete_attr_fn,
                          int *win_keyval, void *extra_state) {

    int ret;

    if ((NULL == win_copy_attr_fn) || (NULL == win_delete_attr_fn) ||
	(NULL == win_keyval))
	return MPI_ERR_ARG;

    ret = lam_attr_create_keyval(WIN_ATTR, (void *)win_copy_attr_fn, 
				 (void *)win_delete_attr_fn,
				 win_keyval, extra_state, 0);

    /* Error handling code here */

    return ret;
}

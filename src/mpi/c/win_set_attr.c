/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_set_attr = PMPI_Win_set_attr
#endif

int MPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val) {

    int ret;

    if (MPI_WIN_NULL == win)
	return MPI_ERR_WIN;

    ret = lam_attr_set(WIN_ATTR, win, win_keyval, attribute_val, 0);

    /* Error handling here */

    return ret;
}

/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_get_attr = PMPI_Win_get_attr
#endif

int MPI_Win_get_attr(MPI_Win win, int win_keyval,
                     void *attribute_val, int *flag) {
    int ret;

    if ((NULL == attribute_val) || (NULL == flag))

	return MPI_ERR_ARG;

    ret = lam_attr_get(WIN_ATTR, win, win_keyval, 
		       attribute_val, flag);

    return ret;
}

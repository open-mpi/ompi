/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_free_keyval = PMPI_free_keyval
#endif

int MPI_Win_free_keyval(int *win_keyval) {
  
    int ret;

    /* Check for valid key pointer */

    if (NULL == win_keyval)
	return MPI_ERR_ARG;

    ret = lam_attr_free_keyval(WIN_ATTR, win_keyval, 0);

    return ret;
}

/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_delete_attr = PMPI_Win_delete_attr
#endif

int MPI_Win_delete_attr(MPI_Win win, int win_keyval) 
{
    int ret; 

    if (MPI_WIN_NULL == win)
	return MPI_ERR_WIN;
  
    ret = lam_attr_delete(WIN_ATTR, win, win_keyval, 0);

    /* Error hanndling code here */
  
    return ret;
}

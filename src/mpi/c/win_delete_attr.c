/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_delete_attr = PMPI_Win_delete_attr
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_delete_attr";


int MPI_Win_delete_attr(MPI_Win win, int win_keyval) 
{
    int ret; 

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
	if (MPI_WIN_NULL == win) {
	    return OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_WIN, 
					 FUNC_NAME);
	}
    }
  
    ret = ompi_attr_delete(WIN_ATTR, win, win->w_keyhash, win_keyval, 
                           false, true);
    OMPI_ERRHANDLER_RETURN(ret, win, MPI_ERR_OTHER, FUNC_NAME);  
}

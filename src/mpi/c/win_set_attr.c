/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_set_attr = PMPI_Win_set_attr
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_set_attr";


OMPI_EXPORT
int MPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val) 
{
    int ret;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
	if (MPI_WIN_NULL == win) {
	    return OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_WIN, 
					 FUNC_NAME);
	}
    }

    ret = ompi_attr_set(WIN_ATTR, win, win->w_keyhash, 
		       win_keyval, attribute_val, 0);
    OMPI_ERRHANDLER_RETURN(ret, win, MPI_ERR_OTHER, FUNC_NAME);  
}

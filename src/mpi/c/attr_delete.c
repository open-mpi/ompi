/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_delete = PMPI_Attr_delete
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Attr_delete";

int MPI_Attr_delete(MPI_Comm comm, int keyval)
{
    int ret;

    if (MPI_PARAM_CHECK) {
	if (MPI_COMM_NULL == comm) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
					 FUNC_NAME);
	}
    }
  
    ret = lam_attr_delete(COMM_ATTR, comm, comm->c_keyhash, keyval, 0);

    LAM_ERRHANDLER_RETURN(ret, comm, MPI_ERR_OTHER, FUNC_NAME);  
}


/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_put = PMPI_Attr_put
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Attr_put";

int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val) 
{
    int ret;

    if (MPI_PARAM_CHECK) {
	if (MPI_COMM_NULL == comm) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
					 FUNC_NAME);
	}
    }

    ret = lam_attr_set(COMM_ATTR, comm, comm->c_keyhash, 
		       keyval, attribute_val, 0);

    LAM_ERRHANDLER_RETURN(ret, comm, MPI_ERR_OTHER, FUNC_NAME);  
}


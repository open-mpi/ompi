/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_get = PMPI_Attr_get
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Attr_get";

int MPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag)
{
    int ret;

    if (MPI_PARAM_CHECK) {
	if ((NULL == attribute_val) || (NULL == flag)) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
					 FUNC_NAME);
	}
    }
        
    ret = lam_attr_get(comm->c_keyhash, keyval, attribute_val, flag);
    
}


/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_set_attribute = PMPI_Comm_set_attribute
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Comm_set_attr";

int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val) 
{
    int ret;

    if (MPI_PARAM_CHECK) {
	if (MPI_COMM_NULL == comm) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
					 FUNC_NAME);
	}
    }
    
    ret = lam_attr_set(COMM_ATTR, comm, comm->c_keyhash, 
		       comm_keyval, attribute_val, 0);

    LAM_ERRHANDLER_RETURN(ret, comm, MPI_ERR_OTHER, FUNC_NAME);  
}

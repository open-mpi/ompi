/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Keyval_free = PMPI_Keyval_free
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Keyval_free";

int MPI_Keyval_free(int *keyval) 
{
    int ret;

    /* Check for valid key pointer */
    if (MPI_PARAM_CHECK) {
	if (NULL == keyval) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
					 FUNC_NAME);
	}
    }
      
    ret = lam_attr_free_keyval(COMM_ATTR, keyval, 0);

    LAM_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}

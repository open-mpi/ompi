/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Keyval_free = PMPI_Keyval_free
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Keyval_free";


int MPI_Keyval_free(int *keyval) 
{
    int ret;

    /* Check for valid key pointer */
    if (MPI_PARAM_CHECK) {
	if (NULL == keyval) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_KEYVAL,
					 FUNC_NAME);
	}
    }
      
    ret = ompi_attr_free_keyval(COMM_ATTR, keyval, 0);
    OMPI_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}

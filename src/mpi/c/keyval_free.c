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

int MPI_Keyval_free(int *keyval) 
{
    int ret;

    /* Check for valid key pointer */

    if (NULL == keyval)
	return MPI_ERR_ARG;
      
    ret = lam_attr_free_keyval(COMM_ATTR, keyval, 0);

    return ret;

}

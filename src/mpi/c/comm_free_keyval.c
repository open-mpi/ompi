/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_free_keyval = PMPI_Comm_free_keyval
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_free_keyval(int *comm_keyval) 
{
    int ret;

    /* Check for valid key pointer */

    if (NULL == comm_keyval)
	return MPI_ERR_ARG;
      
    ret = lam_attr_free_keyval(COMM_ATTR, comm_keyval, 0);

    return ret;
}

/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Create_keyval = PMPI_Create_keyval
#endif

int MPI_Comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn,
                           MPI_Comm_delete_attr_function *comm_delete_attr_fn,
			   int *comm_keyval, void *extra_state)
{
    int ret;
  
    if ((NULL == comm_copy_attr_fn) || (NULL == comm_delete_attr_fn) ||
	(NULL == comm_keyval))
	return MPI_ERR_ARG;

    ret = lam_attr_create_keyval(COMM_ATTR, (void *)comm_copy_attr_fn, 
				 (void *)comm_delete_attr_fn,
				 comm_keyval, extra_state, 0);

    /* Error handling code here */  

    return ret;
}

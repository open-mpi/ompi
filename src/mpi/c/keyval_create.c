/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Keyval_create = PMPI_Keyval_create
#endif

int MPI_Keyval_create(MPI_Copy_function *copy_attr_fn,
                      MPI_Delete_function *delete_attr_fn,
                      int *keyval, void *extra_state) 
{
    int ret;
    
    if ((NULL == copy_attr_fn) || (NULL == delete_attr_fn) ||
	(NULL == keyval))
	return MPI_ERR_ARG;

    ret = lam_attr_create_keyval(COMM_ATTR, (void *)copy_attr_fn, 
				 (void *)delete_attr_fn,
				 keyval, extra_state, 0);

    /* Error handling code here */  

    return ret;

}

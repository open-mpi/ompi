/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_free_keyval = PMPI_Type_free_keyval
#endif

int
MPI_Type_free_keyval(int *type_keyval)
{
    int ret;

    /* Check for valid key pointer */

    if (NULL == type_keyval) 
	return MPI_ERR_ARG;

    ret = lam_attr_free_keyval(TYPE_ATTR, type_keyval, 0);
  
    return ret;
}

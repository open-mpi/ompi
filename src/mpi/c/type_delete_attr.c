/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_delete_attr = PMPI_Type_delete_attr
#endif

int
MPI_Type_delete_attr (MPI_Datatype type, int type_keyval)
{
    int ret;

    if (MPI_DATATYPE_NULL == type)
	return MPI_ERR_TYPE;
  
    ret = lam_attr_delete(TYPE_ATTR, type, type_keyval, 0);

    /* Error handling code here */

    return ret;
}

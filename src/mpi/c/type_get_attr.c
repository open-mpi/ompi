/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_get_attr = PMPI_Type_get_attr
#endif

int
MPI_Type_get_attr (MPI_Datatype type,
                   int type_keyval,
                   void *attribute_val,
                   int *flag)
{
    int ret;

    if ((NULL == attribute_val) || (NULL == flag))
	return MPI_ERR_ARG;

    ret = lam_attr_get(TYPE_ATTR, type, type_keyval, 
		       attribute_val, flag);
  
    return ret;
}

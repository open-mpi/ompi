/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_get = PMPI_Attr_get
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag)
{
    int ret;
    if ((NULL == attribute_val) || (NULL == flag))
	return MPI_ERR_ARG;
    
    ret = lam_attr_get(COMM_ATTR, comm, keyval, attribute_val, flag);
    
    return ret;
}


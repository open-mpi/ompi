/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_delete = PMPI_Attr_delete
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Attr_delete(MPI_Comm comm, int keyval)
{
    int ret;

    if (MPI_COMM_NULL == comm)
	return MPI_ERR_COMM;
  
    ret = lam_attr_delete(COMM_ATTR, comm, keyval, 0);

    /* Error hanndling code here */

    return ret;
}


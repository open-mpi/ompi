/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_put = PMPI_Attr_put
#endif

int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val) 
{
    int ret;
    if (MPI_COMM_NULL == comm)
	return MPI_ERR_COMM;

    ret = lam_attr_set(COMM_ATTR, comm, keyval, attribute_val, 0);

    /* Error handling code here  */

    return ret;
}


/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_set_attribute = PMPI_Comm_set_attribute
#endif

int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val) 
{
    int ret;
    if (MPI_COMM_NULL == comm)
	return MPI_ERR_COMM;

    ret = lam_attr_set(COMM_ATTR, comm, comm_keyval, attribute_val, 0);

    /* Error handling code here  */

    return ret;
}

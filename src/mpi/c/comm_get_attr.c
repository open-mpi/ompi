/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_get_attr = PMPI_Comm_get_attr
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_get_attr(MPI_Comm comm, int comm_keyval,
                      void *attribute_val, int *flag)
{
    int ret;
    if ((NULL == attribute_val) || (NULL == flag))
	return MPI_ERR_ARG;

    ret = lam_attr_get(COMM_ATTR, comm, comm_keyval, 
		       attribute_val, flag);

    return ret;
}

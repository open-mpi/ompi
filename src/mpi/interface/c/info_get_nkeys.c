/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_nkeys = PMPI_Info_get_nkeys
#endif

/**
 * MPI_Info_get_nkeys - Returns the number of keys defined on an
 * 'MPI_Info' object
 *
 * @param info info object (handle)
 * @param nkeys number of keys defined on 'info' (integer)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_ARG
 *
 * This function returns the number of elements in the list 
 * containing the key-value pairs
 */
int MPI_Info_get_nkeys(MPI_Info info, int *nkeys) {
    return MPI_SUCCESS;
}

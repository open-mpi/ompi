/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_nthkey = PMPI_Info_get_nthkey
#endif

/**
 *   MPI_Info_get_nthkey - Get a key indexed by integer from an 'MPI_Info' obje
 *
 *   @param info info object (handle)
 *   @param n index of key to retrieve (integer)
 *   @param key character string of at least 'MPI_MAX_INFO_KEY' characters
 *   
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 */
int MPI_Info_get_nthkey(MPI_Info info, int n, char *key) {
    return MPI_SUCCESS;
}

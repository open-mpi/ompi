/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "lfc/lam_list.h"
#include "info/info.h"
#include <string.h>
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_nthkey = PMPI_Info_get_nthkey
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
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
    int nkeys;
    int err;

    /*
     * 1. Check if info is a valid handle
     * 2. Check if there are atleast "n" elements
     * 3. If so, give the nth defined key
     */
    if (MPI_PARAM_CHECK) {
        if (NULL == info || 0 > n){
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Info_get_nthkey");
        }
    }

    MPI_Info_get_nkeys(info, &nkeys);
    
    if (nkeys < n) {
        return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_INFO_KEY,
                                      "MPI_Info_get_nthkey");

    } else {
        /*
         * Everything seems alright. Call the back end key copy
         */
        err = lam_info_get_nthkey (info, n, key);
    }

    /*
     * Have to check whether there are any error condditions. It appears
     * that there are not too many error conditions from the look of it
     */
    return MPI_SUCCESS;
}

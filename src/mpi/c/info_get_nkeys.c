/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "lfc/lam_list.h"
#include "info/info.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_nkeys = PMPI_Info_get_nkeys
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
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
    int err;

    if (MPI_PARAM_CHECK) {
        if (NULL == info){
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                         "MPI_Info_nkeys");
        }
    }
    err = lam_info_get_nkeys(info, nkeys);

    /*
     * check if there are any errors. There does not seem to be any 
     * error possible in this. But have to look at it again
     */

    return MPI_SUCCESS;
}

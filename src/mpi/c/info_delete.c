/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "lfc/lam_list.h"
#include "info/info.h"
#include <stdlib.h>
#include <string.h>
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_delete = PMPI_Info_delete
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

/**
 * Delete a (key,value) pair from "info"
 *
 * @param info MPI_Info handle on which we need to operate
 * @param key The key portion of the (key,value) pair that 
 *            needs to be deleted
 *
 * @retval MPI_SUCCESS If the (key,val) pair was deleted
 * @retval MPI_ERR_ARG
 * @retval MPI_ERR_NOKEY
 */
int MPI_Info_delete(MPI_Info info, char *key) {
    int key_length;
    int err;
    /**
     * This function merely deletes the (key,val) pair in info
     */
    if (MPI_PARAM_CHECK) {
        if (NULL == info || NULL == key){
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                         "MPI_Info_delete");
        }
    }

    key_length = (key) ? strlen (key) : 0;
    if ( (0 == key_length) || (MPI_MAX_INFO_KEY <= key_length)) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO_KEY,
                                     "MPI_Info_delete");
    }

    err = lam_info_delete (info, key);

    if (MPI_ERR_INFO_NOKEY == err) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO_NOKEY,
                                     "MPI_Info_delete");
    }
    return err;
}

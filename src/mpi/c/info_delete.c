/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/lam_list.h"
#include "mpi/info/info.h"
#include <stdlib.h>
#include <string.h>

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_delete = PMPI_Info_delete
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
    if (NULL == info){
        printf ("Invalid MPI_Info handle passed\n");
        return MPI_ERR_ARG;
    }

    key_length = (key) ? strlen (key) : 0;
    if ( (0 == key_length) || (MPI_MAX_INFO_KEY <= key_length)) {
        printf ("The key passed to MPI_INFO_SET is too long\n");
        return MPI_ERR_INFO_KEY;
    }

    err = lam_info_delete (info, key);

    if (MPI_ERR_INFO_NOKEY == err) {
        printf ("Invalid Key given\n");
        return err;
    }
    return err;
}

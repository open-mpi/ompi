/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

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
 * @retval MPI_ERR_KEY
 * @retval MPI_ERR_NOKEY
 * @retval MPI_ERR_INTERN
 */
int MPI_Info_delete(MPI_Info info, char *key) {

    /**
     * This function merely deletes the (key,val) pair in info
     */
    return MPI_SUCCESS;
}

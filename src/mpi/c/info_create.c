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
#pragma weak MPI_Info_create = PMPI_Info_create
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

/**
 * Create a new info object 
 *
 * @param info Pointer to the MPI_Info handle
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_ARG
 * @retval MPI_ERR_SYSRESOURCE
 *
 * When an MPI_Info object is not being used, it should be freed using
 * MPI_Info_free
 */
int MPI_Info_create(MPI_Info *info) {
    /* list of invalid conditions
     * 1. MPI_ERR_ARG - If info is NULL
     * 2. MPI_ERR_SYSRESOURCE - If LAM_MALLOC fails
     * NOTE:
     * Yet to add stuff for fortran handles
     */
    if (MPI_PARAM_CHECK) {
        if (NULL == info) {
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                         "MPI_Info_create");
        }
    }

    /*
     * Call the object create function. This function not only
     * allocates the space for MPI_Info, but also calls all the
     * relevant init functions. Should I check if the fortran 
     * handle is valid
     */
    (*info) = OBJ_NEW(lam_info_t);
    
    if (NULL == (*info)) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_SYSRESOURCE,
                                     "MPI_Info_create");
    }

    return MPI_SUCCESS;
}

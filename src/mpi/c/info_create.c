/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "class/ompi_list.h"
#include "info/info.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Info_create = PMPI_Info_create
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Info_create";

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
     * 2. MPI_ERR_SYSRESOURCE - If OMPI_MALLOC fails
     * NOTE:
     * Yet to add stuff for fortran handles
     */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == info) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    /*
     * Call the object create function. This function not only
     * allocates the space for MPI_Info, but also calls all the
     * relevant init functions. Should I check if the fortran 
     * handle is valid
     */
    (*info) = OBJ_NEW(ompi_info_t);
    if (NULL == (*info)) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                      FUNC_NAME);
    }

    return MPI_SUCCESS;
}

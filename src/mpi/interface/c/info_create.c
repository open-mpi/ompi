/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/lam_list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_create = PMPI_Info_create
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
    if (NULL == info) {
        printf ("Info handle passed is invalid\n");
        return MPI_ERR_ARG;
    }

    /*
     * Call the object create function. This function not only
     * allocates the space for MPI_Info, but also calls all the
     * relevant init functions.
     */
    (*info) = OBJ_NEW(lam_info_t);
    
    if (NULL == (*info)) {
        printf ("Malloc failed. Ran out of resources\n");
        return MPI_ERR_SYSRESOURCE;
    }

    return MPI_SUCCESS;
}

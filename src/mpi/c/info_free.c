/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "lfc/lam_list.h"
#include "info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_free = PMPI_Info_free
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

/**
 *   MPI_Info_free - Free an 'MPI_Info' object.
 *
 *   @param info pointer to info object to be freed (handle)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *
 *   Upon successful completion, 'info' will be set to 'MPI_INFO_NULL'.
 */
int MPI_Info_free(MPI_Info *info) {
    int err;
    /*
     * Free all the alloced items from MPI_Info info.
     * Make sure the items are freed in an orderly
     * fashion so that there are no dangling pointers.
     * Also, something needs to be done about the 
     * fortran handle.
     */
    if (NULL == info){
        printf ("Invalid MPI_Info handle passed\n");
        return MPI_ERR_ARG;
    }

    /*
     * Now call the back end. Once again, it does not look like 
     * there can be any error from this, but then who knows. Have
     * to recheck this part too.
     */
    err = lam_info_free (info);
    
    return MPI_SUCCESS;
}

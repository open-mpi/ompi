/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_free = PMPI_Info_free
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
    /*
     * Free all the alloced items from MPI_Info info.
     * Make sure the items are freed in an orderly
     * fashion so that there are no dangling pointers.
     * Also, something needs to be done about the 
     * fortran handle.
     */
    return MPI_SUCCESS;
}

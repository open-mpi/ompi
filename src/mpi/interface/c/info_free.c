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
    lam_info_entry_t *iterator;
    int nkeys;
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
     * Now to actually free all the values
     */
    err = MPI_Info_get_nkeys(*info, &nkeys);

    /*
     * We could just get each element from the list and then call
     * MPI_Info_delete. But this causes unnecessary delay because 
     * MPI_Info_delete has extra logic to it. So, do the simple 
     * remove operation to save time.
     */
    for (iterator = (lam_info_entry_t *)lam_list_get_first(&((*info)->super));
         nkeys > 0;
         nkeys--) {
        iterator = (lam_info_entry_t *)iterator->super.lam_list_next;
        OBJ_RELEASE(iterator->super.lam_list_prev);
    }

    /*
     * Anju:
     * Add things to remove the fortran handle from the mapping table
     */
    
    OBJ_RELEASE(*info);
    *info = MPI_INFO_NULL;
    
    return MPI_SUCCESS;
}

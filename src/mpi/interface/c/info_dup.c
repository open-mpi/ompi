/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_dup = PMPI_Info_dup
#endif

/**
 *   MPI_Info_dup - Duplicate an 'MPI_Info' object
 *
 *   @param info source info object (handle)
 *   @param newinfo pointer to the new info object (handle)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *
 *   Not only will the (key, value) pairs be duplicated, the order of keys
 *   will be the same in 'newinfo' as it is in 'info'.
 *   When an info object is no longer being used, it should be freed with
 *   'MPI_Info_free'.
 */
int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo) {
    lam_info_entry_t *iterator;
    int err;
    int nkeys;
    /**
     * Here we need to do 2 things
     * 1. Create a newinfo object using MPI_Info_create
     * 2. Fetch all the values from info and copy them to 
     *    newinfo using MPI_Info_set
     * The new implementation facilitates traversal in many ways.
     * I have chosen to get the number of elements on the list 
     * and copy them to newinfo one by one
     */

    if (NULL == info){
        printf ("Invalid MPI_Info handle passed\n");
        return MPI_ERR_ARG;
    }

    err = MPI_Info_create(newinfo);
    if (MPI_SUCCESS != err) {
        printf ("Creation of newinfo falied\n");
        return err;
    }
    /*
     * Now to actually duplicate all the values
     */
    err = MPI_Info_get_nkeys (info, &nkeys);

    for (iterator = (lam_info_entry_t *)lam_list_get_first(&(info->super));
         nkeys > 0;
         nkeys--) {
        err = MPI_Info_set (*newinfo, iterator->ie_key, iterator->ie_value);
        if (MPI_SUCCESS != err) {
            printf ("Failed to set a key in newinfo\n");
            return err;
        }
        iterator = (lam_info_entry_t *)iterator->super.lam_list_next;
    }

    return MPI_SUCCESS;
}

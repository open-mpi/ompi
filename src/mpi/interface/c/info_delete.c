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
    lam_info_entry_t *search;
    lam_info_entry_t *found;
    int key_length;
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

    search = lam_info_find_key (info, key);

    if (NULL == search){
        printf ("Invalid key given\n");
        return MPI_ERR_INFO_NOKEY;
    } else {
        /*
         * An entry with this key value was found. Remove the item
         * and free the memory allocated to it
         */
        found = (lam_info_entry_t *)
                lam_list_remove_item (&(info->super),
                                      (lam_list_item_t *)search);
        OBJ_RELEASE(search);
    }

    return MPI_SUCCESS;
}

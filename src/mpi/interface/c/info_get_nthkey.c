/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"
#include <string.h>

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_nthkey = PMPI_Info_get_nthkey
#endif

/**
 *   MPI_Info_get_nthkey - Get a key indexed by integer from an 'MPI_Info' obje
 *
 *   @param info info object (handle)
 *   @param n index of key to retrieve (integer)
 *   @param key character string of at least 'MPI_MAX_INFO_KEY' characters
 *   
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 */
int MPI_Info_get_nthkey(MPI_Info info, int n, char *key) {
    lam_info_entry_t *iterator;
    int nkeys;

    /*
     * 1. Check if info is a valid handle
     * 2. Check if there are atleast "n" elements
     * 3. If so, give the nth defined key
     */
    if (NULL == info){
        printf ("Invalid MPI_Info handle passed\n");
        return MPI_ERR_ARG;
    }

    MPI_Info_get_nkeys(info, &nkeys);
    
    if (nkeys < n) {
        printf ("Requested key does not exist\n");
        return MPI_ERR_ARG;
    } else {
        /*
         * Iterate over and over till we get to the nth key
         */
        iterator = (lam_info_entry_t *)lam_list_get_first(&(info->super));
        for ( ; n > 0; n--) {
            iterator = (lam_info_entry_t *)
                        iterator->super.lam_list_next;
        }
        /*
         * iterator is of the type lam_list_item_t. We have to 
         * cast it to lam_info_entry_t before we can use it to
         * access the value
         */
        strcpy(key, iterator->ie_key);
    }
    return MPI_SUCCESS;
}

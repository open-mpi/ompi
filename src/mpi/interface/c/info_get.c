/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"
#include "lam/util/strncpy.h"
#include <stdlib.h>
#include <string.h>

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get = PMPI_Info_get
#endif

/**
 *   MPI_Info_get - Get a (key, value) pair from an 'MPI_Info' object
 *
 *   @param info info object (handle)
 *   @param key null-terminated character string of the index key
 *   @param valuelen maximum length of 'value' (integer)
 *   @param value null-terminated character string of the value
 *   @param flag true (1) if 'key' defined on 'info', false (0) if not
 *               (logical)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *   @retval MPI_ERR_INFO_KEY
 *
 *   In C and C++, 'valuelen' should be one less than the allocated space
 *   to allow for for the null terminator.
 */
int MPI_Info_get(MPI_Info info, char *key, int valuelen,
                 char *value, int *flag) {

    lam_info_entry_t *search;
    int key_length;
    int value_length;

    /*
     * Simple function. All we need to do is search for the value
     * having the "key" associated with it and then populate the
     * necessary structures.
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
        *flag = 0;
    } else {
        /*
         * We have found the element, so we can return the value
         * Set the flag, value_length and value
         */
        *flag = 1;
        value_length = strlen(search->ie_value);
        /*
         * If the stored value is shorter than valuelen, then
         * we can copy the entire value out. Else, we have to 
         * copy ONLY valuelen bytes out
         */
        if (value_length < valuelen ) {
               strcpy(value, search->ie_value);
        } else {
            lam_strncpy(value, search->ie_value, valuelen);
            value[valuelen] = 0;
        }
    }

    return MPI_SUCCESS;
}

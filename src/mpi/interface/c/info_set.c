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
#pragma weak MPI_Info_set = PMPI_Info_set
#endif

/**
 *   MPI_Info_set - Set a (key, value) pair in an 'MPI_Info' object
 *
 *   @param key null-terminated character string of the index key
 *   @param value null-terminated character string of the value
 *   @param info info object (handle)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *   @retval MPI_ERR_INFO_KEY
 *   @retval MPI_ERR_INFO_VAL
 *   @retval MPI_ERR_INFO_NOKEY
 *   @retval MPI_ERR_INTERN
 *
 *   MPI_Info_set adds the (key,value) pair to info, and overrides
 *   teh value if for the same key a previsou value was set. key and
 *   value must be NULL terminated strings in C. In fortan, leading 
 *   and trailing spaces in key and value are stripped. If either 
 *   key or value is greater than the allowed maxima, MPI_ERR_INFO_KEY
 *   and MPI_ERR_INFO_VALUE are raised
 */
int MPI_Info_set(MPI_Info info, char *key, char *value) {

    lam_info_entry_t *new_info;
    lam_info_entry_t *old_info; 
    int key_length;
    int value_length;
    char *new_value; 

    /*
     * Error conditions are
     * 1. MPI_ERR_ARG if
     *          - info is NULL
     * 2. MPI_ERR_SYSRESOURCE
     *          - No storage space available for the new value
     * 3. MPI_ERR_INFO_KEY
     *          - Key length exceeded MPI_MAX_KEY_VAL
     * 3. MPI_ERR_INFO_VAL
     *          - value length exceeded MPI_MAX_KEY_VAL
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

    value_length = (value) ? strlen (value) : 0;
    if ( (0 == value_length) || (MPI_MAX_INFO_KEY <= value_length)) {
        printf ("The value passed to MPI_INFO_SET is too long\n");
        return MPI_ERR_INFO_VALUE;
    }

    /*
     * If all is right with the arguments, then:
     * 1. Allocate space for value 
     * 2. Check if the key was associated with a previous value
     *      - If so delete that value and store the new value
     * 3. Store the (key, value) pair
     */

    new_value = malloc(value_length * sizeof(char));
    if (NULL == new_value) {
        printf ("Unable to malloc memory for new (key, value) pair\n");
        return MPI_ERR_SYSRESOURCE;
    }

    strcpy (new_value, value);
    old_info = lam_info_find_key (info, key);

    if (NULL != old_info) {
        /*
         * key already exists. remove the value associated with it
         */
        free(old_info->ie_value);
        old_info->ie_value = new_value;
    } else {
        new_info = OBJ_NEW(lam_info_entry_t);
        if (NULL == new_info) {
            printf ("Unable to malloc memory for new (key, value) pair\n");
            return MPI_ERR_SYSRESOURCE;
        }
        strcpy (new_info->ie_key, key);
        new_info->ie_value = new_value;
        lam_list_append (&(info->super), (lam_list_item_t *) new_info);
    }
    
    return MPI_SUCCESS;
}

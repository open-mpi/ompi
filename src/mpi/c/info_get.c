/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/lam_list.h"
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

    int err;
    int key_length;

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

    err = lam_info_get (info, key, valuelen, value, flag);
    
    /*
     * Once again, lam_info_get does not return any error. So, as of
     * now there is no error condition to check for. But maybe this 
     * needs to be re-evaluated and then something can be done 
     */
    return MPI_SUCCESS;
}

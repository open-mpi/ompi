/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "class/ompi_list.h"
#include "info/info.h"
#include <stdlib.h>
#include <string.h>
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Info_get_valuelen = PMPI_Info_get_valuelen
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

/**
 *   MPI_Info_get_valuelen - Get the length of a value for a given key in an 'M
 *
 *   @param info - info object (handle)
 *   @param key - null-terminated character string of the index key
 *   @param valuelen - length of the value associated with 'key' (integer)
 *   @param flag - true (1) if 'key' defined on 'info', false (0) if not
 *   (logical)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *   @retval MPI_ERR_INFO_KEY
 *
 *   The length returned in C and C++ does not include the end-of-string
 *   character.  If the 'key' is not found on 'info', 'valuelen' is left 
 *   alone.
 */
int MPI_Info_get_valuelen(MPI_Info info, char *key, int *valuelen,
                          int *flag) {

    int key_length;
    int err;

    /*
     * Simple function. All we need to do is search for the value
     * having the "key" associated with it and return the length
     */
    if (MPI_PARAM_CHECK) {
        if (NULL == info || NULL == key){
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                         "MPI_Info_get_valuelen");
        }
        key_length = (key) ? strlen (key) : 0;
        if ( (0 == key_length) || (MPI_MAX_INFO_KEY <= key_length)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO_KEY,
                                         "MPI_Info_get_valuelen");
        }
    }

    err = ompi_info_get_valuelen (info, key, valuelen, flag);
    /*
     * Once again, the error problem. ompi_info_get_valuelen
     * does not have an obvious error return.
     */
    return err;
}

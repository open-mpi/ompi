/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "class/ompi_list.h"
#include "info/info.h"
#include "util/strncpy.h"
#include <stdlib.h>
#include <string.h>
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Info_get = PMPI_Info_get
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
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
    if (MPI_PARAM_CHECK) {
        if (NULL == info || NULL == key || 0 > valuelen){
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                         "MPI_Info_get");
        }

        key_length = (key) ? strlen (key) : 0;
        if ( (0 == key_length) || (MPI_MAX_INFO_KEY <= key_length)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO_KEY,
                                         "MPI_Info_get");
        }
    }

    err = ompi_info_get (info, key, valuelen, value, flag);
    
    /*
     * Once again, ompi_info_get does not return any error. So, as of
     * now there is no error condition to check for. But maybe this 
     * needs to be re-evaluated and then something can be done 
     */
    return MPI_SUCCESS;
}

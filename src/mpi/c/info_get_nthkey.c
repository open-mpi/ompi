/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "class/ompi_list.h"
#include "info/info.h"
#include <string.h>
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Info_get_nthkey = PMPI_Info_get_nthkey
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Info_get_nthkey";


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
int MPI_Info_get_nthkey(MPI_Info info, int n, char *key) 
{
    int nkeys;
    int err;

    /*
     * 1. Check if info is a valid handle
     * 2. Check if there are atleast "n" elements
     * 3. If so, give the nth defined key
     */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == info || MPI_INFO_NULL == info) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_INFO,
                                           FUNC_NAME);
        }
        if (0 > n) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                           FUNC_NAME);
        }
        if (NULL == key) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_INFO_KEY,
                                           FUNC_NAME);
        }
    }

    err = ompi_info_get_nkeys(info, &nkeys);
    OMPI_ERRHANDLER_CHECK(err, MPI_COMM_WORLD, err, FUNC_NAME);
    if (nkeys < n) {
        return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_INFO_KEY,
                                       FUNC_NAME);
    }
    
    /* Everything seems alright. Call the back end key copy */

    err = ompi_info_get_nthkey (info, n, key);
    OMPI_ERRHANDLER_RETURN(err, MPI_COMM_WORLD, err, FUNC_NAME);
}

/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

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

    /*
     * Simple function. All we need to do is search for the value
     * having the "key" associated with it and then populate the
     * necessary structures.
     */

    return MPI_SUCCESS;
}

/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

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
 *   value must nen NULL terminated strings in C. In fortan, leading 
 *   and trailing spaces in key and value are stripped. If either 
 *   key or value is greater than the allowed maxima, MPI_ERR_INFO_KEY
 *   and MPI_ERR_INFO_VALUE are raised
 */
int MPI_Info_set(MPI_Info info, char *key, char *value) {
    return MPI_SUCCESS;
}

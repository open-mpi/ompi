/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "lfc/lam_list.h"
#include "info/info.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_c2f = PMPI_Info_c2f
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

/**
 * Converts info into a fortan handle
 *
 * @param info The C handle which has to be converted 
 * @retval Fortran handle corresponding to info.
 *
 * If info is a valid C handle, the MPI_Info_c2f returns a valid 
 * fortran handle to the same MPI_INFO object.
 */
MPI_Fint MPI_Info_c2f(MPI_Info info) {

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (NULL == info) {
            return (MPI_Fint) LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                    "MPI_Info_c2f");
        }
    }
    /* return the index */
    return (MPI_Fint)(info->i_fhandle);
}

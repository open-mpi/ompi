/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "class/ompi_list.h"
#include "info/info.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Info_c2f = PMPI_Info_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Info_c2f";


/**
 * Converts info into a fortan handle
 *
 * @param info The C handle which has to be converted 
 * @retval Fortran handle corresponding to info.
 *
 * If info is a valid C handle, the MPI_Info_c2f returns a valid 
 * fortran handle to the same MPI_INFO object.
 */
MPI_Fint MPI_Info_c2f(MPI_Info info) 
{
    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == info || MPI_INFO_NULL == info ||
            ompi_info_is_freed(info)) {
            return (MPI_Fint) OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, 
                                                     MPI_ERR_INFO,
                                                     FUNC_NAME);
        }
    }

    /* return the index */
    return (MPI_Fint)(info->i_fhandle);
}

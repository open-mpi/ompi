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
#pragma weak MPI_Info_f2c = PMPI_Info_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Info_f2c";


/**
 * Converts the MPI_Fint info into a valid C MPI_Info handle
 *
 * @param info Integer handle to an MPI_INFO object
 * @retval C handle corresponding to MPI_INFO object
 */
MPI_Info MPI_Info_f2c(MPI_Fint info) 
{
    size_t info_index = (size_t) info;

    /* check the arguments */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (info_index < 0 || 
            info_index >= ompi_pointer_array_get_size(&ompi_info_f_to_c_table)) {
            (void) OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
            return MPI_INFO_NULL;
        }
    }

    /* return the index */ 
    return ompi_info_f_to_c_table.addr[info_index];
}

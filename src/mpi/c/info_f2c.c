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

/**
 * Converts the MPI_Fint info into a valid C MPI_Info handle
 *
 * @param info Integer handle to an MPI_INFO object
 * @retval C handle corresponding to MPI_INFOO object
 */
MPI_Info MPI_Info_f2c(MPI_Fint info) {
    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (0 > info || info >= ompi_pointer_array_get_size(ompi_info_f_to_c_table)) {
            return MPI_INFO_NULL;
        }
    }
    /* return the index */ 
    return ompi_info_f_to_c_table->addr[info];
}

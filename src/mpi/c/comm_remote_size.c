/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_remote_size = PMPI_Comm_remote_size
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


static const char FUNC_NAME[] = "MPI_Comm_remote_size";


OMPI_EXPORT
int MPI_Comm_remote_size(MPI_Comm comm, int *size) {

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (MPI_COMM_NULL == comm || ompi_comm_invalid (comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }

        if ( NULL == size ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    *size = ompi_comm_remote_size ((ompi_communicator_t*)comm);
    return MPI_SUCCESS;
}

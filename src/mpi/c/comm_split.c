/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_split = PMPI_Comm_split
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_split";


int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) {

    int rc;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( comm == MPI_COMM_NULL || ompi_comm_invalid ( comm )) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }

        if ( color < 0 &&  MPI_UNDEFINED != color ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        
        if ( NULL == newcomm ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
    }
    
    rc = ompi_comm_split ( (ompi_communicator_t*)comm, color, key, 
                          (ompi_communicator_t**)newcomm, false);
    OMPI_ERRHANDLER_RETURN ( rc, comm, rc, FUNC_NAME);
}

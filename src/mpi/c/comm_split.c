/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_split = PMPI_Comm_split
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) {

    int rc;

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized ) 
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                      "MPI_Comm_split");

        if ( comm == MPI_COMM_NULL || ompi_comm_invalid ( comm ))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                      "MPI_Comm_split");

        if ( color < 0 &&  MPI_UNDEFINED != color ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                      "MPI_Comm_split");
        
        if ( NULL == newcomm )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                      "MPI_Comm_split");
    }
    
    rc = ompi_comm_split ( (ompi_communicator_t*)comm, color, key, 
                          (ompi_communicator_t**)newcomm );
    OMPI_ERRHANDLER_RETURN ( rc, comm, rc, "MPI_Comm_split");
}

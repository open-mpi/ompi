/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "util/strncpy.h"
#include "include/totalview.h"
#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_get_name = PMPI_Comm_get_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_get_name(MPI_Comm comm, char *name, int *length)  {

    ompi_communicator_t* comp;

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized ) 
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_INTERN,
                                           "MPI_Comm_get_name");

        if ( MPI_COMM_NULL == comm || ompi_comm_invalid ( comm ) )
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM, 
                                           "MPI_Comm_get_name");

        if ( NULL == name || NULL == length ) 
            return OMPI_ERRHANDLER_INVOKE ( comm, MPI_ERR_ARG, 
                                           "MPI_Comm_get_name");
    }

    comp = (ompi_communicator_t*) comm;

    if ( comp->c_flags & OMPI_COMM_NAMEISSET ) {
        strncpy ( name, comp->c_name, MPI_MAX_OBJECT_NAME );
        *length = strlen ( comp->c_name );
    }
    else {
        memset ( name, 0, MPI_MAX_OBJECT_NAME );
        *length = 0;
    }

    return MPI_SUCCESS;
}

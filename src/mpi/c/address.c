/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Address = PMPI_Address
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Address";

int MPI_Address(void *location, MPI_Aint *address)
{
    if( MPI_PARAM_CHECK ) {
        if( OMPI_MPI_INVALID_STATE ) {
            OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, (ompi_communicator_t*)NULL,
                                    MPI_ERR_INTERN, FUNC_NAME );
        }
    }
    *address = (MPI_Aint)location;
	return MPI_SUCCESS;
}

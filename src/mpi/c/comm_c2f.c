/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_c2f = PMPI_Comm_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_c2f";


OMPI_EXPORT
MPI_Fint MPI_Comm_c2f(MPI_Comm comm) 
{
    ompi_communicator_t *cptr=(ompi_communicator_t *)comm;

    if ( MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        
    /* mapping an invalid handle to a null handle */
	/* not invoking an error handler */
        if ( ompi_comm_invalid (cptr)) {
			cptr = (ompi_communicator_t *) MPI_COMM_NULL;
		}
    }

    return ((MPI_Fint) comm->c_f_to_c_index);
}

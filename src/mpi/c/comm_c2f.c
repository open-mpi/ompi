/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_c2f = PMPI_Comm_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_c2f";


MPI_Fint MPI_Comm_c2f(MPI_Comm comm) 
{
    ompi_communicator_t *cptr=(ompi_communicator_t *)comm;

    if ( MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        
        if ( ompi_comm_invalid (cptr))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                        FUNC_NAME);
    }

    /* Since MPI_COMM_NULL is an object itself, we do not have to check
       for that */
    return ((MPI_Fint) comm->c_f_to_c_index);
}

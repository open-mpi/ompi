/*
 * $HEADER$
 */
#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mpi/runtime/mpiruntime.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Abort = PMPI_Abort
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Abort";


int MPI_Abort(MPI_Comm comm, int errorcode) 
{
    /* Don't even bother checking comm and errorcode values for
       errors */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    return ompi_mpi_abort(comm, errorcode, true);
}

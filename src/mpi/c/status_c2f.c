/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mpi/f77/constants.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Status_c2f = PMPI_Status_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Status_c2f";


int MPI_Status_c2f(MPI_Status *c_status, MPI_Fint *f_status) 
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* MPI-2:4.12.5 says that if you pass in
           MPI_STATUS[ES]_IGNORE, it's erroneous */
        
        if (NULL == c_status || MPI_STATUS_IGNORE == c_status || 
            MPI_STATUSES_IGNORE == c_status || NULL == f_status) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, 
                                          MPI_ERR_IN_STATUS, FUNC_NAME);
        }
    }

    /* We can't use OMPI_INT_2_FINT here because of some complications
       with include files.  :-( So just do the casting manually. */

    f_status[0] = (MPI_Fint) c_status->MPI_SOURCE;
    f_status[1] = (MPI_Fint) c_status->MPI_TAG;
    f_status[2] = (MPI_Fint) c_status->MPI_ERROR;
    f_status[3] = (MPI_Fint) c_status->_count;

    return MPI_SUCCESS;
}

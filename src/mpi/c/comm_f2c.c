/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_f2c = PMPI_Comm_f2c
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


MPI_Comm MPI_Comm_f2c(MPI_Fint comm) 
{
    size_t o_index= (size_t) comm;

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_f2c");

        if ( 0 > o_index ||
             o_index >= lam_pointer_array_get_size(&lam_mpi_communicators)) {
            return MPI_COMM_NULL;
        }
    }
        
    return lam_mpi_communicators.addr[o_index];
}

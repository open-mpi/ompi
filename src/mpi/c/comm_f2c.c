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
#pragma weak MPI_Comm_f2c = PMPI_Comm_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_f2c";


MPI_Comm MPI_Comm_f2c(MPI_Fint comm) 
{
    size_t o_index= (size_t) comm;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /* Per MPI-2:4.12.4, do not invoke an error handler if we get an
       invalid fortran handle */

    if ( 0 > o_index ||
         o_index >= ompi_pointer_array_get_size(&ompi_mpi_communicators)) {
        return MPI_COMM_NULL;
    }
        
    return ompi_pointer_array_get_item(&ompi_mpi_communicators, o_index);
}

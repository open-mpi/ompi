/*
 * $HEADER$
 */

#include "lam_config.h"

#include <string.h>

#include "util/strncpy.h"
#include "include/totalview.h"
#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES 
#pragma weak MPI_Comm_set_name = PMPI_Comm_set_name
#endif

int MPI_Comm_set_name(MPI_Comm comm, char *name) {


    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized ) 
            return LAM_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_INTERN,
                                           "MPI_Comm_set_name");

        if ( MPI_COMM_NULL == comm || lam_comm_invalid ( comm ) )
            return LAM_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                           "MPI_Comm_set_name");

        if ( NULL == name )
            return LAM_ERRHANDLER_INVOKE ( comm, MPI_ERR_ARG, 
                                           "MPI_Comm_set_name");
    }

    /* -- Thread safety entrance -- */

    /* Copy in the name */
    strncpy(comm->c_name, name, MPI_MAX_OBJECT_NAME);
    comm->c_name[MPI_MAX_OBJECT_NAME - 1] = 0;
    comm->c_flags |= LAM_COMM_NAMEISSET;

    /* -- Tracing information for new communicator name -- */
#if 0  
  /* Force TotalView DLL to take note of this name setting */

  ++lam_tv_comm_sequence_number;
#endif

  /* -- Thread safety exit -- */
  
  return MPI_SUCCESS;
}

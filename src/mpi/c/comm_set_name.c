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
#pragma weak MPI_Comm_set_name = PMPI_Comm_set_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Comm_set_name";


int MPI_Comm_set_name(MPI_Comm comm, char *name) {

    ompi_communicator_t* comp;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( MPI_COMM_NULL == comm || ompi_comm_invalid ( comm ) )
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                            FUNC_NAME);

        if ( NULL == name )
            return OMPI_ERRHANDLER_INVOKE ( comm, MPI_ERR_ARG, 
                                            FUNC_NAME);
    }

    /* -- Thread safety entrance -- */

    /* Copy in the name */
    comp = (ompi_communicator_t*) comm;
    
    strncpy(comp->c_name, name, MPI_MAX_OBJECT_NAME);
    comp->c_name[MPI_MAX_OBJECT_NAME - 1] = 0;
    comp->c_flags |= OMPI_COMM_NAMEISSET;

    /* -- Tracing information for new communicator name -- */
#if 0  
  /* Force TotalView DLL to take note of this name setting */

  ++ompi_tv_comm_sequence_number;
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

  /* -- Thread safety exit -- */
  
  return MPI_SUCCESS;
}

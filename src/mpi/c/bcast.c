/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/coll/coll.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Bcast = PMPI_Bcast
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Bcast";


int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype,
                int root, MPI_Comm comm)
{
    int size, err;
    mca_coll_base_bcast_fn_t func;

    if (MPI_PARAM_CHECK) {
      if (MPI_COMM_NULL == comm) {
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                     FUNC_NAME);
      }

      if (NULL == buffer) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
      }

      if (MPI_DATATYPE_NULL == datatype) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
      }
    
      if (count < 0) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
      }
    }

    if (OMPI_COMM_IS_INTRA(comm)) {
      MPI_Comm_size(comm, &size);
      if ((root >= size) || (root < 0)) {
	  return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
      }
      if (count == 0 && comm->c_coll.coll_bcast_optimization) {
	  return(MPI_SUCCESS);
      }

      /* If there's only one node, we're done */
      
      else if (size <= 1) {
	return(MPI_SUCCESS);
      }
    } else {
      MPI_Comm_remote_size(comm, &size);
      if (!(((root < size) && (root >= 0)) 
	|| (root == MPI_ROOT) || (root == MPI_PROC_NULL))) {
	  return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
      }
    } 

    /* VPS: Need to change this to another pointer, because we wont have
     two pointers - intra and inter - cached in the new design */
    func = comm->c_coll.coll_bcast_intra;

    if (NULL == func)
	return MPI_ERR_OTHER;

    err = func(buffer, count, datatype, root, comm);
    
    OMPI_ERRHANDLER_RETURN(err, comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}

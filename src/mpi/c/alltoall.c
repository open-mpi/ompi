/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/coll/coll.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Alltoall = PMPI_Alltoall
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Alltoall";


int MPI_Alltoall(void *sendbuf, int sendcount, MPI_Datatype sendtype,
		 void *recvbuf, int recvcount,
		 MPI_Datatype recvtype, MPI_Comm comm) 
{
    int err;
    mca_coll_base_alltoall_fn_t func;

    if (MPI_PARAM_CHECK) {
      if (MPI_COMM_NULL == comm) {
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                     FUNC_NAME);
      }

      if ((NULL == sendbuf) || (NULL == recvbuf)) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
      }

      if ((MPI_DATATYPE_NULL == sendtype) || (MPI_DATATYPE_NULL == recvtype)) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
      }
    
      if ((sendcount < 0) || (recvcount < 0)) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
      }
    }

    /* VPS: Need to change this to another pointer, because we wont have
     two pointers - intra and inter - cached in the new design */
    func = comm->c_coll.coll_alltoall_intra;

    if (NULL == func) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OTHER, FUNC_NAME);
    }

    err = func(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
               comm);
	       
    OMPI_ERRHANDLER_RETURN((err != MPI_SUCCESS), comm, MPI_ERR_UNKNOWN,
                          FUNC_NAME);
}


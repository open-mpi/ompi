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
#pragma weak MPI_Allgather = PMPI_Allgather
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Allgather";

int MPI_Allgather(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
		  void *recvbuf, int recvcount, MPI_Datatype recvtype,
		  MPI_Comm comm) 
{
    int err;
    mca_coll_base_allgather_fn_t func;

    if (MPI_PARAM_CHECK) {
	if (MPI_COMM_NULL == comm) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
					 FUNC_NAME);
	}
	
	if ((MPI_DATATYPE_NULL == sendtype) 
	    || (MPI_DATATYPE_NULL == recvtype)) {
	    return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
	}
	
	if ((sendcount < 0) || (recvcount < 0)) {
	    return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
	}
    }

    func = comm->c_coll.coll_allgather_intra;

    if (NULL == func) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OTHER, FUNC_NAME);
    }

    /* Call the coll SSI to actually perform the allgather */

    err = func(sendbuf, sendcount, sendtype, recvbuf, recvcount, 
	       recvtype, comm);

    OMPI_ERRHANDLER_RETURN(err, comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}


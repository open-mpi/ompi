/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/coll/coll.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Gather = PMPI_Gather
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Gather";

int MPI_Gather(void *sendbuf, int sendcount, MPI_Datatype sendtype,
               void *recvbuf, int recvcount, MPI_Datatype recvtype,
               int root, MPI_Comm comm) 
{
    int rank;
    int size;
    int err;
    mca_coll_base_gather_fn_t func;
    
    if (MPI_PARAM_CHECK) {
	if (MPI_COMM_NULL == comm) {
	    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
					 FUNC_NAME);
	}
    }	

    func = comm->c_coll.coll_gather_intra;

    if (LAM_COMM_IS_INTRA(comm)) {
	/* conditions for intracomm */
	MPI_Comm_size(comm, &size);
	MPI_Comm_rank(comm, &rank);
	if ((root >= size) || (root < 0)) {
	    return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
	}
	if ((sendcount < 0) || (rank == root && recvcount < 0)) {
	    return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
	}
	if ((sendtype == MPI_DATATYPE_NULL) ||
	    (rank == root && recvtype == MPI_DATATYPE_NULL)) {
	    return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME); 
	}
    } else {
	/* Conditions for intercomm */
	MPI_Comm_remote_size(comm, &size);
	if (((root != MPI_PROC_NULL) && (sendtype == MPI_DATATYPE_NULL)) ||
	    (root == MPI_ROOT  && recvtype == MPI_DATATYPE_NULL)) {
	    return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME); 
	}
	if (!(((root < size) && (root >= 0)) 
	      || (root == MPI_ROOT) || (root == MPI_PROC_NULL))) {
	    return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME); 
	}
    }

    if (func == NULL) {
	return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_OTHER, FUNC_NAME); 
    }


    /* Call the coll SSI to actually perform the bcast */
	
    err = func(sendbuf, sendcount, sendtype, recvbuf,
	       recvcount, recvtype, root, comm);

    LAM_ERRHANDLER_RETURN(err, comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}

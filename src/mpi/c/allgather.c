/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/coll/coll.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Allgather = PMPI_Allgather
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Allgather";


int MPI_Allgather(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
		  void *recvbuf, int recvcount, MPI_Datatype recvtype,
		  MPI_Comm comm) 
{
    int err;

    if (MPI_PARAM_CHECK) {

        /* Unrooted operation -- same checks for all ranks on both
           intracommunicators and intercommunicators */

        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
	if (ompi_comm_invalid(comm)) {
          OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
	} else if (MPI_DATATYPE_NULL == recvtype) {
          err = MPI_ERR_TYPE;
        } else if (recvcount < 0) {
          err = MPI_ERR_COUNT;
        } else {
          OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcount);
        }
        OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
    }

    /* Can we optimize?  Everyone had to give the same send signature,
       which means that everyone must have given a sendcount > 0 if
       there's anything to send. */

    if (sendcount == 0) {
      return MPI_SUCCESS;
    }

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_allgather(sendbuf, sendcount, sendtype, 
                                      recvbuf, recvcount, recvtype, comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}


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
#pragma weak MPI_Alltoallw = PMPI_Alltoallw
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Alltoallw";


int MPI_Alltoallw(void *sendbuf, int *sendcounts, int *sdispls, 
                  MPI_Datatype *sendtypes,
                  void *recvbuf, int *recvcounts, int *rdispls, 
                  MPI_Datatype *recvtypes, MPI_Comm comm) 
{
    int i, size, err;

    if (MPI_PARAM_CHECK) {

      /* Unrooted operation -- same checks for all ranks */

      err = MPI_SUCCESS;
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (ompi_comm_invalid(comm)) {
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                     FUNC_NAME);
      }

      if ((NULL == sendcounts) || (NULL == sdispls) || (NULL == sendtypes) ||
          (NULL == recvcounts) || (NULL == rdispls) || (NULL == recvtypes)) {
        return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
      }

      /* We always define the remote group to be the same as the local
         group in the case of an intracommunicator, so it's safe to
         get the size of the remote group here for both intra- and
         intercommunicators */

      size = ompi_comm_remote_size(comm);
      for (i = 0; i < size; ++i) {
        if (recvcounts[i] < 0) {
          err = MPI_ERR_COUNT;
        } else if (MPI_DATATYPE_NULL == recvtypes[i]) {
          err = MPI_ERR_TYPE;
        } else {
          OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtypes[i], sendcounts[i]);
        }
        OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
      }
    }

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_alltoallw(sendbuf, sendcounts, sdispls, sendtypes, 
                                      recvbuf, recvcounts, rdispls, recvtypes,
                                      comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}


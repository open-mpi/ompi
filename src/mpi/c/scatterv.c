/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/coll/coll.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Scatterv = PMPI_Scatterv
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Scatterv";


OMPI_EXPORT
int MPI_Scatterv(void *sendbuf, int *sendcounts, int *displs,
                 MPI_Datatype sendtype, void *recvbuf, int recvcount,
                 MPI_Datatype recvtype, int root, MPI_Comm comm) 
{
    int i, size, err;
    
    if (MPI_PARAM_CHECK) {
        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
	}

        /* Errors for intracommunicators */

        if (OMPI_COMM_IS_INTRA(comm)) {

          /* Errors for all ranks */

          if ((root >= ompi_comm_size(comm)) || (root < 0)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
          }

          if (recvcount < 0) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
          }

          if (MPI_DATATYPE_NULL == recvtype) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME); 
          }

          /* Errors for the root.  Some of these could have been
             combined into compound if statements above, but since
             this whole section can be compiled out (or turned off at
             run time) for efficiency, it's more clear to separate
             them out into individual tests. */

          if (ompi_comm_rank(comm) == root) {
            if (NULL == displs) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
            }

            if (NULL == sendcounts) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
            }

            size = ompi_comm_size(comm);
            for (i = 0; i < size; ++i) {
              OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcounts[i]);
              OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
            }
          }
        }

        /* Errors for intercommunicators */

        else {
          if (! ((root >= 0 && root < ompi_comm_remote_size(comm)) ||
                 MPI_ROOT == root || MPI_PROC_NULL == root)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
          }

          /* Errors for the receivers */

          if (MPI_ROOT != root && MPI_PROC_NULL != root) {
            if (recvcount < 0) {
              return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
            }

            if (MPI_DATATYPE_NULL == recvtype) {
              return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME); 
            }
          }

          /* Errors for the root.  Ditto on the comment above -- these
             error checks could have been combined above, but let's
             make the code easier to read. */

          else if (MPI_ROOT == root) {
            if (NULL == displs) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
            }

            if (NULL == sendcounts) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
            }

            size = ompi_comm_size(comm);
            for (i = 0; i < size; ++i) {
              OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcounts[i]);
              OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
            }
          }
        }
    }

    /* Invoke the coll component to perform the back-end operation */
	
    err = comm->c_coll.coll_scatterv(sendbuf, sendcounts, displs, sendtype, 
                                     recvbuf, recvcount, recvtype, root, comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

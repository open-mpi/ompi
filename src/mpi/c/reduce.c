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
#include "op/op.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Reduce = PMPI_Reduce
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Reduce";


int MPI_Reduce(void *sendbuf, void *recvbuf, int count,
               MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm) 
{
    int err;

    if (MPI_PARAM_CHECK) {
        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
	}

        /* Checks for all ranks */
	
        else if (MPI_OP_NULL == op) {
          err = MPI_ERR_OP;
        } else if (ompi_op_is_intrinsic(op) &&
                   datatype->id < DT_MAX_PREDEFINED &&
                   -1 == ompi_op_ddt_map[datatype->id]) {
          err = MPI_ERR_OP;
        } else {
          OMPI_CHECK_DATATYPE_FOR_SEND(err, datatype, count);
        }
        OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);

        /* Intercommunicator errors */

        if (!OMPI_COMM_IS_INTRA(comm)) {
          if (! ((root >= 0 && root < ompi_comm_remote_size(comm)) ||
                 MPI_ROOT == root || MPI_PROC_NULL == root)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
          }
        }
    }

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_reduce(sendbuf, recvbuf, count,
                                   datatype, op, root, comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

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
#pragma weak MPI_Scan = PMPI_Scan
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Scan";


int MPI_Scan(void *sendbuf, void *recvbuf, int count,
             MPI_Datatype datatype, MPI_Op op, MPI_Comm comm) 
{
    int err;

    if (MPI_PARAM_CHECK) {
        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
	}

        /* No intercommunicators allowed! (MPI does not define
           MPI_SCAN on intercommunicators) */

        else if (OMPI_COMM_IS_INTER(comm)) {
          err = MPI_ERR_COMM;
        }

        /* Unrooted operation; checks for all ranks */
	
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
    }

    /* If everyone supplied count == 0, we can just return */

    if (0 == count) {
        return MPI_SUCCESS;
    }

    /* Call the coll component to actually perform the allgather */

    err = comm->c_coll.coll_scan(sendbuf, recvbuf, count,
                                 datatype, op, comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

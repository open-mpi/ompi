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
#pragma weak MPI_Reduce_scatter = PMPI_Reduce_scatter
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Reduce_scatter";


int MPI_Reduce_scatter(void *sendbuf, void *recvbuf, int *recvcounts,
                       MPI_Datatype datatype, MPI_Op op, MPI_Comm comm) 
{
    int i, err, size;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
	}

        /* Unrooted operation; same checks for all ranks on both
           intracommunicators and intercommunicators */
	
        if (MPI_DATATYPE_NULL == datatype) {
          return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
        }
	
        if (MPI_OP_NULL == op) {
          return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OP, FUNC_NAME);
        }

        if (ompi_op_is_intrinsic(op) && datatype->id < DT_MAX_PREDEFINED &&
            -1 == ompi_op_ddt_map[datatype->id]) {
          return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OP, FUNC_NAME);
        }

        if (NULL == recvcounts) {
          return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
        }

        if (OMPI_COMM_IS_INTRA(comm)) {
          size = ompi_comm_size(comm);
        } else {
          size = ompi_comm_remote_size(comm);
        }
        for (i = 0; i < size; ++i) {
          if (recvcounts[i] < 0) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
          }
        }
    }

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_reduce_scatter(sendbuf, recvbuf, recvcounts,
                                           datatype, op, comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

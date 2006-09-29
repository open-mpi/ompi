/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/datatype/datatype.h"
#include "ompi/op/op.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Reduce_scatter = PMPI_Reduce_scatter
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Reduce_scatter";


int MPI_Reduce_scatter(void *sendbuf, void *recvbuf, int *recvcounts,
                       MPI_Datatype datatype, MPI_Op op, MPI_Comm comm) 
{
    int i, err, size, count;

    if (MPI_PARAM_CHECK) {
        char *msg;
        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }

        /* Unrooted operation; same checks for all ranks on both
           intracommunicators and intercommunicators */

        else if (MPI_OP_NULL == op) {
          err = MPI_ERR_OP;
        } else if (!ompi_op_is_valid(op, datatype, &msg, FUNC_NAME)) {
            int ret = OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OP, msg);
            free(msg);
            return ret;
        } else if (NULL == recvcounts) {
          err = MPI_ERR_COUNT;
        } else if (MPI_IN_PLACE == recvbuf) {
          err = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);

        /* We always define the remote group to be the same as the
           local group in the case of an intracommunicator, so it's
           safe to get the size of the remote group here for both
           intra- and intercommunicators */

        size = ompi_comm_size(comm);
        for (i = 0; i < size; ++i) {
          OMPI_CHECK_DATATYPE_FOR_SEND(err, datatype, recvcounts[i]);
          OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
        }
    }

    /* MPI-1, p114, says that each process must supply at least one
       element.  But at least the Pallas benchmarks call MPI_REDUCE
       with a count of 0.  So be sure to handle it.  Grrr... */

    size = ompi_comm_size(comm);
    for (count = i = 0; i < size; ++i) {
        if (0 == recvcounts[i]) {
            ++count;
        }
    }
    if (size == count) {
        return MPI_SUCCESS;
    }

    /* Invoke the coll component to perform the back-end operation */

    OBJ_RETAIN(op);
    err = comm->c_coll.coll_reduce_scatter(sendbuf, recvbuf, recvcounts,
                                           datatype, op, comm);
    OBJ_RELEASE(op);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

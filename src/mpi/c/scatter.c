/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
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
#pragma weak MPI_Scatter = PMPI_Scatter
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Scatter";


int MPI_Scatter(void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype,
                int root, MPI_Comm comm) 
{
    int err;
    
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
            err = MPI_ERR_ROOT;
          } else if (recvcount < 0) {
            err = MPI_ERR_COUNT;
          } else if (MPI_DATATYPE_NULL == recvtype) {
            err = MPI_ERR_TYPE;
          }

          /* Errors for the root.  Some of these could have been
             combined into compound if statements above, but since
             this whole section can be compiled out (or turned off at
             run time) for efficiency, it's more clear to separate
             them out into individual tests. */

          else if (ompi_comm_rank(comm) == root) {
            OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcount);
          }
          OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
        }

        /* Errors for intercommunicators */

        else {
          if (! ((root >= 0 && root < ompi_comm_remote_size(comm)) ||
                 MPI_ROOT == root || MPI_PROC_NULL == root)) {
            err = MPI_ERR_ROOT;
          }

          /* Errors for the receivers */

          else if (MPI_ROOT != root && MPI_PROC_NULL != root) {
            if (recvcount < 0) {
              err = MPI_ERR_COUNT;
            } else if (MPI_DATATYPE_NULL == recvtype) {
              err = MPI_ERR_TYPE;
            }
          }

          /* Errors for the root.  Ditto on the comment above -- these
             error checks could have been combined above, but let's
             make the code easier to read. */

          else if (MPI_ROOT == root) {
            OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcount);
          }
          OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
        }
    }

    /* If we have nothing to receive, return success (everyone must
       have given the same recvcount) */

    if (0 == recvcount) {
        return MPI_SUCCESS;
    }

    /* Invoke the coll component to perform the back-end operation */
	
    err = comm->c_coll.coll_scatter(sendbuf, sendcount, sendtype, recvbuf,
                                    recvcount, recvtype, root, comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

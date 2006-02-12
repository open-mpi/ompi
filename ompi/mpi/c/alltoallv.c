/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Alltoallv = PMPI_Alltoallv
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Alltoallv";


int MPI_Alltoallv(void *sendbuf, int *sendcounts, int *sdispls,
                  MPI_Datatype sendtype, 
                  void *recvbuf, int *recvcounts, int *rdispls, 
                  MPI_Datatype recvtype, MPI_Comm comm) 
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

      if ((NULL == sendcounts) || (NULL == sdispls) ||
          (NULL == recvcounts) || (NULL == rdispls) ||
          MPI_IN_PLACE == sendbuf || MPI_IN_PLACE == recvbuf) {
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
        } else if (MPI_DATATYPE_NULL == recvtype) {
          err = MPI_ERR_TYPE;
        } else {
          OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcounts[i]);
        }
        OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
      }
    }

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_alltoallv(sendbuf, sendcounts, sdispls, sendtype, 
                                      recvbuf, recvcounts, rdispls, recvtype,
                                      comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}


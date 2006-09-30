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
#pragma weak MPI_Alltoall = PMPI_Alltoall
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Alltoall";


int MPI_Alltoall(void *sendbuf, int sendcount, MPI_Datatype sendtype,
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
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                     FUNC_NAME);
      } else if (MPI_DATATYPE_NULL == recvtype) {
        err = MPI_ERR_TYPE;
      } else if (recvcount < 0) {
        err = MPI_ERR_COUNT;
      } else if (MPI_IN_PLACE == sendbuf || MPI_IN_PLACE == recvbuf) {
        err = MPI_ERR_ARG;
      } else {
        OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcount);
      }
      OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
    }

    /* Do we need to do anything? */

    if (0 == sendcount && 0 == recvcount) {
        return MPI_SUCCESS;
    }

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_alltoall(sendbuf, sendcount, sendtype, 
                                     recvbuf, recvcount, recvtype,
                                     comm);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}


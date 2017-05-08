/*
 * Copyright (c)      2012 Oak Rigde National Laboratory. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

static const char FUNC_NAME[] = "MPI_Ibcast_init";


int MPI_Ibcast_init(void *buffer, int count, MPI_Datatype datatype,
              int root, MPI_Comm comm,  MPI_Request *request)
{
	//printf(" ** entered MPI_Ibcast_init **\n");
    int err;

    MEMCHECKER(
        memchecker_datatype(datatype);
        memchecker_call(&opal_memchecker_base_isdefined, buffer, count, datatype);
        memchecker_comm(comm);
    );

    if (MPI_PARAM_CHECK) {
      err = MPI_SUCCESS;
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (ompi_comm_invalid(comm)) {
          return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
                                     FUNC_NAME);
      }

      /* Errors for all ranks */

      OMPI_CHECK_DATATYPE_FOR_SEND(err, datatype, count);
      OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
      if (MPI_IN_PLACE == buffer) {
          return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
      }

      /* Errors for intracommunicators */

      if (OMPI_COMM_IS_INTRA(comm)) {
        if ((root >= ompi_comm_size(comm)) || (root < 0)) {
          return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
        }
      }

      /* Errors for intercommunicators */

      else {
        if (! ((root >= 0 && root < ompi_comm_remote_size(comm)) ||
               MPI_ROOT == root || MPI_PROC_NULL == root)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ROOT, FUNC_NAME);
        }
      }
    }

//     if (MPI_PROC_NULL == request) {
//            *request = OBJ_NEW(ompi_request_t);
//            /* Other fields were initialized by the constructor for
//               ompi_request_t */
//            (*request)->req_type = OMPI_REQUEST_NOOP;
//            (*request)->req_status = ompi_request_empty.req_status;
//            (*request)->req_complete = true;
//            (*request)->req_state = OMPI_REQUEST_INACTIVE;
//            (*request)->req_persistent = true;
//            (*request)->req_free = ompi_request_persistent_proc_null_free;
//            //return MPI_SUCCESS;
//        }

    OPAL_CR_ENTER_LIBRARY();

    /* Invoke the coll component to perform the back-end operation */
    //printf(" ** invoking c_coll.coll_ibcast_init **\n");
    err = comm->c_coll.coll_ibcast_init(buffer, count, datatype, root, comm,
                                  request,
                                  comm->c_coll.coll_ibcast_init_module);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_get_errhandler = PMPI_Comm_get_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


static const char FUNC_NAME[] = "MPI_Comm_get_errhandler";


int MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler)
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == comm || 
        MPI_COMM_NULL == comm) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                    FUNC_NAME);
    } else if (NULL == errhandler) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                    FUNC_NAME);
    }
  }

  /* Return the errhandler.  A quandry.  Should we increase the
     refcount here?

     - Consider that if we *get* an errhandler, we don't have to free
       it.  It's just a handle that was returned to the user.  If they
       never free it (and we increased the refcount), then it'll never
       be freed.

     - However, if we *don't* increase it and the user *does* free it,
       then this could cause the refcount to go to 0 prematurely, and
       a communicator could be left with a stale error handler.

     Add to the mix that MPI-1:196:8-11 says that MPI_ERRHANDLER_FREE
     will only free the error handler when all the communicators using
     it have been freed.

     All in all, it seems like we should increase the refcount to be
     safe here.  We're still conformant -- error handlers won't be
     freed until all the communicators (or other objects using them)
     are freed *and* any outstanding handles returned by this function
     (or its peers) are also freed.
  */

  OBJ_RETAIN(comm->error_handler);
  *errhandler = comm->error_handler;

  /* All done */

  return MPI_SUCCESS;
}

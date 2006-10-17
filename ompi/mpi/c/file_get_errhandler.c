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

#include "ompi/mpi/c/bindings.h"
#include "ompi/file/file.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_get_errhandler = PMPI_File_get_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_get_errhandler";


int MPI_File_get_errhandler( MPI_File file, MPI_Errhandler *errhandler) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

    /* Note that MPI-2:9.7 (p265 in the ps; 261 in the pdf) explicitly
       says that you are allowed to set the error handler on
       MPI_FILE_NULL */

    if (NULL == file) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_FILE,
                                   "MPI_File_get_errhandler");
    } else if (NULL == errhandler) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_File_get_errhandler");
    }
  }

  /* Retain the errhandler, corresponding to object refcount
     decrease in errhandler_free.c. */
  OBJ_RETAIN(file->error_handler);
  *errhandler = file->error_handler;

  /* All done */

  return MPI_SUCCESS;
}

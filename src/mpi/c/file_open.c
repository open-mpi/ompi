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

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "info/info.h"
#include "file/file.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_open = PMPI_File_open
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_open";


int MPI_File_open(MPI_Comm comm, char *filename, int amode,
                  MPI_Info info, MPI_File *fh)
{
    int rc;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        } else if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
    }

    /* Note that MPI-2:9.7 (p265) says that errors in MPI_FILE_OPEN
       (before the file handle is created) should invoke the default
       error handler on MPI_FILE_NULL.  Hence, if we get a file handle
       out of ompi_file_open(), invoke the error handler on that.  If
       not, invoke the error handler on MPI_FILE_NULL. */

    /* Create an empty MPI_File handle */

    *fh = MPI_FILE_NULL;
    rc = ompi_file_open(comm, filename, amode, info, fh);

    /* Creating the file handle also selects a component to use,
       creates a module, and calls file_open() on the module.  So
       we're good to go. */

    OMPI_ERRHANDLER_RETURN(rc, *fh, rc, FUNC_NAME);
}

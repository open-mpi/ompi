/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "info/info.h"
#include "file/file.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"

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

    /* The io framework is only initialized lazily.  If it hasn't
       already been initialized, do so now (note that MPI_FILE_OPEN
       and MPI_FILE_DELETE are the only two places that it will be
       initialized). */

    if (!(mca_io_base_components_opened_valid ||
          mca_io_base_components_available_valid)) {
        bool user_threads = true;
        bool hidden_threads = true;
        if (OMPI_SUCCESS != (rc = mca_io_base_open())) {
            return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, rc, FUNC_NAME);
        }
        /* JMS Need to do something here with user_threads and
           hidden_threads -- technically this is no longer a query,
           it's a mandate.  The query part is left over from when this
           function was invoked during MPI_INIT.  Since we've now
           long-since decided the user threads and hidden threads
           stuff (i.e., during MPI_INIT), we can't change them now.
           This is not hugely important now, since ROMIO is the only
           io component that we have, but it should be fixed. */
        if (OMPI_SUCCESS != (rc = mca_io_base_find_available(&user_threads, 
                                                             &hidden_threads))) {
            return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, rc, FUNC_NAME);
        }
    }

    /* Create an empty MPI_File handle */

    *fh = MPI_FILE_NULL;
    rc = ompi_file_open(comm, filename, amode, info, fh);

    /* Creating the file handle also selects a component to use,
       creates a module, and calls file_open() on the module.  So
       we're good to go. */

    OMPI_ERRHANDLER_RETURN(rc, *fh, rc, FUNC_NAME);
}

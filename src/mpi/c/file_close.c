/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "file/file.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_close = PMPI_File_close
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_close";


OMPI_EXPORT
int MPI_File_close(MPI_File *fh) 
{
    int rc;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* Note that MPI-2:9.7 (p265) says that errors in
           MPI_FILE_CLOSE should invoke the default error handler on
           MPI_FILE_NULL */

        if (NULL == fh || ompi_file_invalid(*fh)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, MPI_ERR_FILE,
                                          FUNC_NAME);
        }
    }

    /* Release the MPI_File; the destructor releases the component,
       zeroes out fiels, etc. */

    rc = ompi_file_close(fh);
    OMPI_ERRHANDLER_RETURN(rc, *fh, rc, FUNC_NAME);
}

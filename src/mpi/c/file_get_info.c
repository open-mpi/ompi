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
#pragma weak MPI_File_get_info = PMPI_File_get_info
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_get_info";


int MPI_File_get_info(MPI_File fh, MPI_Info *info_used)
{
    int rc;

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_file_invalid(fh)) {
            rc = MPI_ERR_FILE;
            fh = MPI_FILE_NULL;
        } else if (NULL == info_used) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, fh, rc, FUNC_NAME);
    }

    /* Copy the handle and increase the refcount if it's not NULL */

    *info_used = fh->f_info;
    if (NULL != fh->f_info) {
        OBJ_RETAIN(fh->f_info);
    }

    /* One of the few times that we know that we can succeed.  Woot! */

    return MPI_SUCCESS;
}

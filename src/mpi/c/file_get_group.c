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
#pragma weak MPI_File_get_group = PMPI_File_get_group
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_get_group";


int MPI_File_get_group(MPI_File fh, MPI_Group *group)
{
    int rc;

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_file_invalid(fh)) {
            rc = MPI_ERR_FILE;
            fh = MPI_FILE_NULL;
        } else if (NULL == group) {
            rc = MPI_ERR_GROUP;
        }
        OMPI_ERRHANDLER_CHECK(rc, fh, rc, FUNC_NAME);
    }

    /* Does not need to invoke a back-end io function */

    rc = ompi_comm_group (fh->f_comm, group);
    OMPI_ERRHANDLER_RETURN(rc, fh, rc, FUNC_NAME);
}

/*
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
#pragma weak MPI_File_delete = PMPI_File_delete
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_delete";


int MPI_File_delete(char *filename, MPI_Info info) 
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    /* Note that MPI-2:9.7 (p265) says that errors in MPI_FILE_DELETE
       should invoke the default error handler on MPI_FILE_NULL */

    /* This function is not yet implemented */

    return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, MPI_ERR_INTERN, FUNC_NAME);
}

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
#pragma weak MPI_File_read = PMPI_File_read
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_read";


int MPI_File_read(MPI_File fh, void *buf, int count, MPI_Datatype
                  datatype, MPI_Status *status)
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_file_invalid(fh)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, MPI_ERR_FILE,
                                          FUNC_NAME);
        }
    }

    /* This function is not yet implemented */
    
    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, FUNC_NAME);
}

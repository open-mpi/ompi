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
#pragma weak MPI_File_c2f = PMPI_File_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_c2f";


OMPI_EXPORT
MPI_Fint MPI_File_c2f(MPI_File file)
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_file_invalid(file)) {
            return (MPI_Fint) OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, 
                                                     MPI_ERR_FILE,
                                                     FUNC_NAME);
        }
    }
    
    return (MPI_Fint) file->f_f_to_c_index;
}

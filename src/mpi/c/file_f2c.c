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
#pragma weak MPI_File_f2c = PMPI_File_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_f2c";


MPI_File MPI_File_f2c(MPI_Fint file_f) 
{
    size_t file_index = (size_t) file_f;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (file_index < 0 ||
            file_index >= ompi_pointer_array_get_size(&ompi_file_f_to_c_table)) {
            (void) OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, MPI_ERR_FILE,
                                          FUNC_NAME);
            return MPI_FILE_NULL;
        }
    }

    return ompi_file_f_to_c_table.addr[file_index];
}

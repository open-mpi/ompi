/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "file/file.h"
#include "mca/io/io.h"
#include "mca/io/base/io_base_request.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_iwrite = PMPI_File_iwrite
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_iwrite";


int MPI_File_iwrite(MPI_File fh, void *buf, int count, MPI_Datatype
                    datatype, MPI_Request *request)
{
    int rc;
    mca_io_base_request_t *io_request;

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_file_invalid(fh)) {
            fh = MPI_FILE_NULL;
            rc = MPI_ERR_FILE;
        } else if (count < 0) {
            rc = MPI_ERR_COUNT;
        } else {
           OMPI_CHECK_DATATYPE_FOR_SEND(rc, datatype, count);
        }
        OMPI_ERRHANDLER_CHECK(rc, fh, rc, FUNC_NAME);
    }

    /* Get a request */

    if (OMPI_SUCCESS != mca_io_base_request_alloc(fh, &io_request)) {
        return OMPI_ERRHANDLER_INVOKE(fh, MPI_ERR_NO_MEM, FUNC_NAME);
    }
    *request = (ompi_request_t*) io_request;

    /* Call the back-end io component function */

    switch (fh->f_io_version) {
    case MCA_IO_BASE_V_1_0_0:
        rc = fh->f_io_selected_module.v1_0_0.
            io_module_file_iwrite(fh, buf, count, datatype, io_request);
        break;

    default:
        rc = MPI_ERR_INTERN;
        break;
    }

    /* All done */
    
    OMPI_ERRHANDLER_RETURN(rc, fh, rc, FUNC_NAME);
}

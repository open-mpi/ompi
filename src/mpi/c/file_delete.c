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
#include "mca/io/base/base.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_delete = PMPI_File_delete
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_delete";


int MPI_File_delete(char *filename, MPI_Info info) 
{
    int rc;

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == info || ompi_info_is_freed(info)) {
            rc = MPI_ERR_INFO;
        } else if (NULL == filename) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_FILE_NULL, rc, FUNC_NAME);
    }

    /* Since there is no MPI_File handle associated with this
       function, the MCA has to do a selection and perform the
       action */

    rc = mca_io_base_delete(filename, info);
    OMPI_ERRHANDLER_RETURN(rc, MPI_FILE_NULL, rc, FUNC_NAME);
}

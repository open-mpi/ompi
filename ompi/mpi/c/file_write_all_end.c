/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "ompi/mpi/c/bindings.h"
#include "ompi/file/file.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_write_all_end = PMPI_File_write_all_end
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_write_all_end";


int MPI_File_write_all_end(MPI_File fh, void *buf, MPI_Status *status)
{
    int rc;

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_file_invalid(fh)) {
            fh = MPI_FILE_NULL;
            rc = MPI_ERR_FILE;
        }
        OMPI_ERRHANDLER_CHECK(rc, fh, rc, FUNC_NAME);
    }

    /* Call the back-end io component function */

    switch (fh->f_io_version) {
    case MCA_IO_BASE_V_1_0_0:
        rc = fh->f_io_selected_module.v1_0_0.
            io_module_file_write_all_end(fh, buf, status);
        break;

    default:
        rc = MPI_ERR_INTERN;
        break;
    }

    /* All done */
    
    OMPI_ERRHANDLER_RETURN(rc, fh, rc, FUNC_NAME);
}

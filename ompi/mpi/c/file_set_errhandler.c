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
#pragma weak MPI_File_set_errhandler = PMPI_File_set_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_set_errhandler";


int MPI_File_set_errhandler( MPI_File file, MPI_Errhandler errhandler) 
{
    /* Error checking */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* Note that MPI-2:9.7 (p265 in the ps; p261 in the pdf)
           explicitly says that you are allowed to set the error
           handler on MPI_FILE_NULL */

        if (NULL == file) {
            return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, MPI_ERR_FILE,
                                          FUNC_NAME);
        } else if (NULL == errhandler ||
                   MPI_ERRHANDLER_NULL == errhandler ||
                   (OMPI_ERRHANDLER_TYPE_FILE != errhandler->eh_mpi_object_type && 
		    OMPI_ERRHANDLER_TYPE_PREDEFINED != errhandler->eh_mpi_object_type) ) {
            return OMPI_ERRHANDLER_INVOKE(file, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    /* Ditch the old errhandler, and decrement its refcount */

    OBJ_RELEASE(file->error_handler);

    /* We have a valid comm and errhandler, so increment its refcount */

    file->error_handler = errhandler;
    OBJ_RETAIN(file->error_handler);

    /* All done */

    return MPI_SUCCESS;
}

/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/errhandler/errcode.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Error_class = PMPI_Error_class
#endif
#define MPI_Error_class PMPI_Error_class
#endif

static const char FUNC_NAME[] = "MPI_Error_class";


int MPI_Error_class(int errorcode, int *errorclass)
{
    int ret;

    /* make sure the infrastructure is initialized */
    ret = ompi_mpi_instance_retain ();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret, FUNC_NAME);
    }

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_errcode_is_invalid(errorcode)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                                   FUNC_NAME);
        }
    }


    *errorclass = ompi_mpi_errcode_get_class(errorcode);
    ompi_mpi_instance_release ();

    return MPI_SUCCESS;
}

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
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <string.h>

#include "opal/util/string_copy.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/errhandler/errcode.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Error_string = PMPI_Error_string
#endif
#define MPI_Error_string PMPI_Error_string
#endif

static const char FUNC_NAME[] = "MPI_Error_string";


int MPI_Error_string(int errorcode, char *string, int *resultlen)
{
    int ret;
    char *tmpstring;

    /* make sure the infrastructure is initialized */
    ret = ompi_mpi_instance_retain ();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret,
                                               FUNC_NAME);
    }

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_errcode_is_invalid(errorcode)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                                   FUNC_NAME);
        }
    }

    tmpstring = ompi_mpi_errnum_get_string (errorcode);
    opal_string_copy(string, tmpstring, MPI_MAX_ERROR_STRING);
    *resultlen = (int)strlen(string);

    ompi_mpi_instance_release();

    return MPI_SUCCESS;
}

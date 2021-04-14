/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"
#include "ompi/mpi/c/bindings.h"
#include "ompi/request/grequest.h"
#include "ompi/runtime/params.h"

#if OMPI_BUILD_MPI_PROFILING
#    if OPAL_HAVE_WEAK_SYMBOLS
#        pragma weak MPI_Grequest_complete = PMPI_Grequest_complete
#    endif
#    define MPI_Grequest_complete PMPI_Grequest_complete
#endif

static const char FUNC_NAME[] = "MPI_Grequest_complete";

int MPI_Grequest_complete(MPI_Request request)
{
    int rc = MPI_SUCCESS;

    MEMCHECKER(memchecker_request(&request););

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (MPI_REQUEST_NULL == request || NULL == request) {
            rc = MPI_ERR_REQUEST;
        } else if (OMPI_REQUEST_GEN != request->req_type) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, rc, FUNC_NAME);
    }

    rc = ompi_grequest_complete(request);
    OMPI_ERRHANDLER_NOHANDLE_RETURN(rc, MPI_ERR_INTERN, FUNC_NAME);
}

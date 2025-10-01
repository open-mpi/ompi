/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024-2025 Triad National Security, LLC. All rights
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
#include "ompi/request/request.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Request_toint = PMPI_Request_toint
#endif
#define MPI_Request_toint PMPI_Request_toint
#endif

static const char FUNC_NAME[] = "MPI_Request_toint";

int MPI_Request_toint(MPI_Request_ABI_INTERNAL request)
{
    int o_index;
    ompi_request_t *request_ptr;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == request) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_FILE, FUNC_NAME);
        }
    }

    if (OMPI_ABI_HANDLE_BASE_OFFSET > (intptr_t)request) {
        intptr_t request_int = (intptr_t)request;
        return (int)request_int;
    }

    request_ptr = (ompi_request_t *)request;
    if (MPI_UNDEFINED == request_ptr->req_f_to_c_index) {
        request_ptr->req_f_to_c_index =
            opal_pointer_array_add(&ompi_request_f_to_c_table, request_ptr);
    }

    o_index = request_ptr->req_f_to_c_index;
    o_index += OMPI_ABI_HANDLE_BASE_OFFSET;

    return o_index;
}

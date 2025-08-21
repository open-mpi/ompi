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
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"
#include "ompi/mpi/c/abi_handle_convert.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Errhandler_toint = PMPI_Errhandler_toint
#endif
#define MPI_Errhandler_toint PMPI_Errhandler_toint
#endif

static const char FUNC_NAME[] = "MPI_Errhandler_toint";

int MPI_Errhandler_toint(MPI_Errhandler_ABI_INTERNAL errhandler)
{
    int o_index;
    ompi_errhandler_t *errhandler_ptr;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == errhandler) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ERRHANDLER, FUNC_NAME);
        }
    }

    if (ompi_abi_handle_int_is_predefined((intptr_t)errhandler)) {
        return (int)(intptr_t)errhandler;
    }

    errhandler_ptr = (ompi_errhandler_t *)errhandler;
    o_index = ompi_abi_index_to_handle_int(errhandler_ptr->eh_f_to_c_index);

    return o_index;
}

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
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
#include "ompi/op/op.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"
#include "ompi/mpi/c/abi_handle_convert.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Op_fromint = PMPI_Op_fromint
#endif
#define MPI_Op_fromint PMPI_Op_fromint
#endif

static const char FUNC_NAME[] = "MPI_Op_fromint";

MPI_Op_ABI_INTERNAL MPI_Op_fromint(int op)
{
    int o_index;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    if (ompi_abi_handle_int_is_predefined((intptr_t)op)) {
        return (MPI_Op_ABI_INTERNAL)(intptr_t)op;
    }

    o_index = ompi_abi_handle_int_to_index((intptr_t)op);

    return (MPI_Op_ABI_INTERNAL)opal_pointer_array_get_item(ompi_op_f_to_c_table, o_index);
}

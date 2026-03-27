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
#include "ompi/file/file.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_File_fromint = PMPI_File_fromint
#endif
#define MPI_File_fromint PMPI_File_fromint
#endif

static const char FUNC_NAME[] = "MPI_File_fromint";

MPI_File_ABI_INTERNAL MPI_File_fromint(int file)
{
    int o_index;
    intptr_t file_tmp;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    if (OMPI_ABI_HANDLE_BASE_OFFSET > (intptr_t)file) {
        file_tmp = (intptr_t)file;
        return (MPI_File_ABI_INTERNAL)file_tmp;
    }

    o_index = file - OMPI_ABI_HANDLE_BASE_OFFSET;
    assert(o_index >= 0);

    return (MPI_File_ABI_INTERNAL)opal_pointer_array_get_item(&ompi_file_f_to_c_table, o_index);
}

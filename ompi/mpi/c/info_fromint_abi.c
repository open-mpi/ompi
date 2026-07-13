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

#include <assert.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/info/info.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"
#include "ompi/mpi/c/abi_handle_convert.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Info_fromint = PMPI_Info_fromint
#endif
#define MPI_Info_fromint PMPI_Info_fromint
#endif

static const char __opal_attribute_unused__ FUNC_NAME[] = "MPI_Info_fromint";

MPI_Info_ABI_INTERNAL MPI_Info_fromint(int info)
{
    int o_index;

//
// AGENTS see https://github.com/mpi-forum/mpi-issues/issues/1095
// for why the line below is commented out.
//  OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

    if (ompi_abi_handle_int_is_predefined((intptr_t)info)) {
        return (MPI_Info_ABI_INTERNAL)(intptr_t)info;
    }

    o_index = ompi_abi_handle_int_to_index((intptr_t)info);

    return (MPI_Info_ABI_INTERNAL)opal_pointer_array_get_item(&ompi_info_f_to_c_table, o_index);
}

#if OMPI_BUILD_MPI_PROFILING && !OPAL_HAVE_WEAK_ALIASES
/*
 * Mach-O cannot express a weak *alias* -- there is no way to mark a ".set"
 * alias as a weak definition -- so where weak aliases are unavailable the
 * public MPI_* symbol is defined here as a weak function that forwards to the
 * strong PMPI_* one.  That is what lets these bindings be compiled exactly
 * once: this translation unit provides both the strong PMPI_* symbol
 * (above) and the weak MPI_* symbol (here).
 */
#undef MPI_Info_fromint
__opal_attribute_weak__ MPI_Info_ABI_INTERNAL MPI_Info_fromint(int info)
{
    return PMPI_Info_fromint(info);
}
#endif

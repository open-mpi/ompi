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
#else
/*
 * The MPI Forum ABI requires that the public MPI_* symbols be *weak*
 * definitions.  An application built against another implementation's
 * libmpi_abi imports them as weak definitions, and (at least on macOS)
 * the loader will only satisfy such an import from another weak
 * definition -- a strong one is rejected.  When the bindings are compiled
 * separately (i.e., when weak aliases are unavailable), this is the only
 * definition of the MPI_* symbol, so mark it weak here.
 */
#pragma weak MPI_Info_fromint
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

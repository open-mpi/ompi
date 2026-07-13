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
#include "ompi/win/win.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"
#include "ompi/mpi/c/abi_handle_convert.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Win_fromint = PMPI_Win_fromint
#endif
#define MPI_Win_fromint PMPI_Win_fromint
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
#pragma weak MPI_Win_fromint
#endif

static const char FUNC_NAME[] = "MPI_Win_fromint";

MPI_Win_ABI_INTERNAL MPI_Win_fromint(int win)
{
    int o_index;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    if (ompi_abi_handle_int_is_predefined((intptr_t)win)) {
        return (MPI_Win_ABI_INTERNAL)(intptr_t)win;
    }

    o_index = ompi_abi_handle_int_to_index((intptr_t)win);

    return (MPI_Win_ABI_INTERNAL)opal_pointer_array_get_item(&ompi_mpi_windows, o_index);
}

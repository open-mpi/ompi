/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/tool/mpit-internal.h"
#ifdef OMPI_NO_MPI_PROTOTYPES
#include "ompi/mpi/c/abi.h"
#endif

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_T_source_get_num = PMPI_T_source_get_num
#endif
#define MPI_T_source_get_num PMPI_T_source_get_num
#elif defined(OMPI_NO_MPI_PROTOTYPES)
/*
 * This file is compiled into both libmpi and libmpi_abi;
 * OMPI_NO_MPI_PROTOTYPES is defined only for the libmpi_abi compiles.
 *
 * The MPI Forum ABI requires that the public MPI_* symbols be *weak*
 * definitions.  An application built against another implementation's
 * libmpi_abi imports them as weak definitions, and (at least on macOS)
 * the loader will only satisfy such an import from another weak
 * definition -- a strong one is rejected.  When the bindings are compiled
 * separately (i.e., when weak aliases are unavailable), this is the only
 * definition of the symbol in libmpi_abi, so mark it weak here.
 */
#pragma weak MPI_T_source_get_num
#endif

int MPI_T_source_get_num (int *num_source)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == num_source) {
        return MPI_ERR_ARG;
    }

    *num_source = 0;
    return MPI_SUCCESS;
}

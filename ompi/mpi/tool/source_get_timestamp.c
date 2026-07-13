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
#pragma weak MPI_T_source_get_timestamp = PMPI_T_source_get_timestamp
#endif
#define MPI_T_source_get_timestamp PMPI_T_source_get_timestamp
#endif

int MPI_T_source_get_timestamp (int source_index, MPI_Count *timestamp)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && !timestamp) {
        return MPI_ERR_ARG;
    }

    return MPI_T_ERR_INVALID_INDEX;
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
#undef MPI_T_source_get_timestamp
__opal_attribute_weak__ int MPI_T_source_get_timestamp(int source_index, MPI_Count *timestamp)
{
    return PMPI_T_source_get_timestamp(source_index, timestamp);
}
#endif

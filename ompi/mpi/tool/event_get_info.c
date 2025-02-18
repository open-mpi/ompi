/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2025 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/tool/mpit-internal.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_event_get_info = PMPI_T_event_get_info
#endif
#define MPI_T_event_get_info PMPI_T_event_get_info
#endif

int MPI_T_event_get_info (int event_index, char *name, int *name_len,
                          int *verbosity, MPI_Datatype *array_of_datatypes,
                          MPI_Aint *array_of_displacements, int *num_elements,
                          MPI_T_enum *enumtype, MPI_Info *info,
                          char *desc, int *desc_len, int *bind)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    return MPI_T_ERR_INVALID_INDEX;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mpi/tool/mpit-internal.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_T_source_get_info = PMPI_T_source_get_info
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/tool/profile/defines.h"
#endif

int MPI_T_source_get_info (int source_id, char *name, int *name_len, char *desc, int *desc_len, MPI_T_source_order *ordering,
                           MPI_Count *ticks_per_second, MPI_Count *max_timestamp, MPI_Info *info)
{
    mca_base_source_t *source;
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();
    source = mca_base_source_get (source_id);
    ompi_mpit_unlock ();
    if (OPAL_UNLIKELY(NULL == source)) {
        return MPI_T_ERR_INVALID_INDEX;
    }

    if (name && name_len) {
        strncpy (name, source->source_name, *name_len);
        *name_len = strlen (name);
    }

    if (desc && desc_len) {
        strncpy (desc, source->source_description, *desc_len);
        *desc_len = strlen (desc);
    }

    if (ordering) {
        *ordering = source->source_ordered;
    }

    if (ticks_per_second) {
        *ticks_per_second = source->source_ticks;
    }

    if (max_timestamp) {
        *max_timestamp = SIZE_T_MAX;
    }

    if (*info) {
        *info = OBJ_NEW(ompi_info_t);
    }

    return MPI_SUCCESS;
}

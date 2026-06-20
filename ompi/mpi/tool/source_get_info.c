/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/tool/mpit-internal.h"
#include "ompi/info/info.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_source_get_info = PMPI_T_source_get_info
#endif
#define MPI_T_source_get_info PMPI_T_source_get_info
#endif

int MPI_T_source_get_info (int source_index, char *name, int *name_len, char *desc, int *desc_len, MPI_T_source_order *ordering,
                           MPI_Count *ticks_per_second, MPI_Count *max_timestamp, MPI_Info *info)
{
    mca_base_event_source_t *source;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        rc = mca_base_event_source_get_by_index (source_index, &source);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_T_ERR_INVALID_INDEX;
            break;
        }

        mpit_copy_string (name, name_len, source->name);
        mpit_copy_string (desc, desc_len, source->description);

        if (NULL != ordering) {
            *ordering = (MCA_BASE_EVENT_SOURCE_ORDERED == source->ordering)
                            ? MPI_T_SOURCE_ORDERED
                            : MPI_T_SOURCE_UNORDERED;
        }

        if (NULL != ticks_per_second) {
            *ticks_per_second = (MPI_Count) source->ticks_per_second;
        }

        if (NULL != max_timestamp) {
            *max_timestamp = (MPI_Count) source->max_ticks;
        }

        /* The standard requires a fresh, user-freeable MPI_Info.  An event
           source carries no info hints in this implementation, so return an
           empty one (ompi_info_allocate yields a valid, user-freeable handle). */
        rc = MPI_SUCCESS;
        if (NULL != info) {
            *info = ompi_info_allocate ();
            if (NULL == *info) {
                rc = MPI_T_ERR_MEMORY;
                break;
            }
        }
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}

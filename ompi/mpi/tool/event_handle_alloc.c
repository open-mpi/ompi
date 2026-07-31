/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2025 Triad National Security, LLC. All rights
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
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_T_event_handle_alloc = PMPI_T_event_handle_alloc
#endif
#define MPI_T_event_handle_alloc PMPI_T_event_handle_alloc
#endif

int MPI_T_event_handle_alloc (int event_index, void *obj_handle, MPI_Info info,
                              MPI_T_event_registration *event_registration)
{
    mca_base_event_registration_t *reg;
    opal_info_t *opal_info;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && (NULL == event_registration)) {
        return MPI_ERR_ARG;
    }

    /* MPI_Info is an ompi_info_t whose first member is an opal_info_t. */
    opal_info = (MPI_INFO_NULL == info) ? NULL : &info->super;

    /* No big lock: the OPAL framework is internally synchronized, and the fold
       this can drive invokes user dropped handlers, which must not run with
       ompi_mpit_big_lock held (sec. 5.10) -- a handler re-entering MPI_T would
       deadlock on the non-recursive lock. */
    rc = mca_base_event_handle_alloc (event_index, obj_handle, opal_info, &reg);

    switch (rc) {
    case OPAL_SUCCESS:
        *event_registration = ompit_event_handle (reg);
        return MPI_SUCCESS;
    case OPAL_ERR_VALUE_OUT_OF_BOUNDS:
        return MPI_T_ERR_INVALID_INDEX;
    case OPAL_ERR_BAD_PARAM:
        /* An object-bound event type was given a NULL obj_handle. */
        return MPI_T_ERR_INVALID_HANDLE;
    case OPAL_ERR_NOT_SUPPORTED:
        return MPI_T_ERR_INVALID_HANDLE;
    case OPAL_ERR_OUT_OF_RESOURCE:
        return MPI_T_ERR_OUT_OF_HANDLES;
    default:
        return MPI_T_ERR_INVALID;
    }
}

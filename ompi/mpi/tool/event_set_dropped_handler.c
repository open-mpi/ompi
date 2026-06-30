/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_event_set_dropped_handler = PMPI_T_event_set_dropped_handler
#endif
#define MPI_T_event_set_dropped_handler PMPI_T_event_set_dropped_handler
#endif

int MPI_T_event_set_dropped_handler (MPI_T_event_registration handle, MPI_T_event_dropped_cb_function dropped_cb_function)

{
    ompi_mpit_event_cb_ctx_t *ctx = NULL;
    mca_base_event_ctx_release_fn_t release_cb = NULL;
    mca_base_event_dropped_cb_fn_t dropped_cb = NULL;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* A non-NULL handler is wrapped in a context owned by OPAL until the
       slot's refcount hits 0, at which point OPAL invokes our release_cb.
       The MPI_T dropped-handler signature has no user_data, so pass NULL.
       A NULL handler clears any installed handler (no context needed). */
    if (NULL != dropped_cb_function) {
        ctx = ompit_event_cb_ctx_new ((ompit_generic_fn_t) dropped_cb_function, NULL);
        if (NULL == ctx) {
            return MPI_T_ERR_MEMORY;
        }
        release_cb = ompit_event_ctx_release;
        dropped_cb = ompit_event_dropped_trampoline;
    }

    /* No big lock: the OPAL framework is internally synchronized, and replacing
       the handler can run a slot's release_cb at teardown, which must not run
       with ompi_mpit_big_lock held (sec. 5.10). */
    rc = mca_base_event_set_dropped_handler (ompit_event_reg (handle), dropped_cb, ctx,
                                             release_cb);

    if (OPAL_SUCCESS != rc) {
        /* On immediate failure OPAL does NOT invoke release_cb, so free the
           context ourselves to avoid a leak. */
        ompit_event_ctx_release (ctx);
        return MPI_T_ERR_INVALID_HANDLE;
    }

    return MPI_SUCCESS;
}

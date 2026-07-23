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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_T_event_handle_free = PMPI_T_event_handle_free
#endif
#define MPI_T_event_handle_free PMPI_T_event_handle_free
#endif

int MPI_T_event_handle_free (MPI_T_event_registration event_registration,
                             void *user_data,
                             MPI_T_event_free_cb_function free_cb_function)
{
    ompi_mpit_event_cb_ctx_t *ctx = NULL;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* Wrap the user's free callback + user pointer so the OPAL-signature
       trampoline can forward to the MPI_T callback at teardown. */
    if (NULL != free_cb_function) {
        ctx = ompit_event_cb_ctx_new ((ompit_generic_fn_t) free_cb_function, user_data);
        if (NULL == ctx) {
            return MPI_T_ERR_MEMORY;
        }
    }

    /* Do NOT hold ompi_mpit_big_lock here: the OPAL framework is internally
       synchronized, and handle_free flushes the dropped handler and invokes the
       free callback.  Per sec. 5.10 no user callback may run while the big lock
       is held -- the standard lets such a callback re-enter MPI_T, which would
       deadlock on the non-recursive lock. */
    rc = mca_base_event_handle_free (ompit_event_reg (event_registration), ctx,
                                     (NULL != free_cb_function) ? ompit_event_free_trampoline
                                                               : NULL);

    if (OPAL_SUCCESS != rc) {
        /* Stale / double free: OPAL did not take ownership of the ctx. */
        ompit_event_ctx_release (ctx);
        return MPI_T_ERR_INVALID_HANDLE;
    }

    return MPI_SUCCESS;
}

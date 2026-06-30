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
#include "ompi/runtime/ompi_mpit_events.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_event_register_callback = PMPI_T_event_register_callback
#endif
#define MPI_T_event_register_callback PMPI_T_event_register_callback
#endif

int MPI_T_event_register_callback (MPI_T_event_registration event_registration,
                                   MPI_T_cb_safety cb_safety, MPI_Info info, void *user_data,
                                   MPI_T_event_cb_function event_cb_function)
{
    ompi_mpit_event_cb_ctx_t *ctx = NULL;
    mca_base_event_ctx_release_fn_t release_cb = NULL;
    mca_base_event_cb_fn_t cb = NULL;
    opal_info_t *opal_info;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* Record which MPI ABI this registering tool is using, so the producers
       emit object handles (MPI_Comm / MPI_Win / MPI_Errhandler / MPI_Session)
       in the matching representation.  This entry point is the Open MPI ABI one,
       so hard-code the Open MPI ABI; when the MPI Standard ABI lands
       (open-mpi/ompi#13280) its separate entry point will set
       OMPI_MPIT_ABI_STANDARD here instead. */
    ompi_mpit_callback_abi = OMPI_MPIT_ABI_OMPI;

    /* MPI_Info is an ompi_info_t whose first member is an opal_info_t. */
    opal_info = (MPI_INFO_NULL == info) ? NULL : &info->super;

    /* A non-NULL callback is wrapped in a context owned by OPAL until the
       slot's refcount hits 0, at which point OPAL invokes our release_cb.
       A NULL callback removes any installed callback (no context needed). */
    if (NULL != event_cb_function) {
        ctx = ompit_event_cb_ctx_new ((ompit_generic_fn_t) event_cb_function, user_data);
        if (NULL == ctx) {
            return MPI_T_ERR_MEMORY;
        }
        release_cb = ompit_event_ctx_release;
        cb = ompit_event_cb_trampoline;
    }

    /* No big lock: the OPAL framework is internally synchronized, and replacing
       a callback can run a slot's release_cb at teardown, which must not run
       with ompi_mpit_big_lock held (sec. 5.10). */
    rc = mca_base_event_register_callback (ompit_event_reg (event_registration),
                                           (mca_base_event_cb_safety_t) cb_safety, opal_info,
                                           ctx, release_cb, cb);

    if (OPAL_SUCCESS != rc) {
        /* On immediate failure OPAL does NOT invoke release_cb, so free the
           context ourselves to avoid a leak. */
        ompit_event_ctx_release (ctx);
        return MPI_T_ERR_INVALID_HANDLE;
    }

    return MPI_SUCCESS;
}

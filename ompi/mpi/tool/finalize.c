/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
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

#include "ompi/runtime/ompi_info_support.h"
#include "opal/include/opal/sys/atomic.h"
#include "opal/runtime/opal.h"
#include "opal/util/event.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_T_finalize = PMPI_T_finalize
#endif
#define MPI_T_finalize PMPI_T_finalize
#endif

int MPI_T_finalize (void)
{
    ompi_mpit_lock ();

    if (!mpit_is_initialized ()) {
        ompi_mpit_unlock ();
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (0 == --ompi_mpit_init_count) {
        (void) ompi_info_close_components ();

        /* Release the reference on the shared event base taken in
           MPI_T_init_thread(), now that the framework closes just
           above have run -- they may have been the *true* closes
           (component_close() deleting events from the base) if we were
           the last framework reference holder.  If MPI is still
           initialized, its own reference keeps the base alive. */
        (void) opal_event_finalize ();

        int32_t state = ompi_mpi_state;
        if ((state < OMPI_MPI_STATE_INIT_COMPLETED ||
             state >= OMPI_MPI_STATE_FINALIZE_PAST_COMM_SELF_DESTRUCT) &&
            (NULL != ompi_mpi_main_thread)) {
            /* we are not between MPI_Init and MPI_Finalize so we
             * have to free the ompi_mpi_main_thread */
            OBJ_RELEASE(ompi_mpi_main_thread);
            ompi_mpi_main_thread = NULL;
        }

        (void) opal_finalize_util ();
    }

    ompi_mpit_unlock ();

    return MPI_SUCCESS;
}

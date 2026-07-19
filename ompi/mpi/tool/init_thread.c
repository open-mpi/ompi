/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/instance/instance.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_T_init_thread = PMPI_T_init_thread
#endif
#define MPI_T_init_thread PMPI_T_init_thread
#endif

extern opal_mutex_t ompi_mpit_big_lock;

extern volatile uint32_t ompi_mpit_init_count;

/* Set when a first MPI_T_init_thread() failed partway: the framework
   registration bookkeeping is not unwindable (see below), so MPI_T can
   never be brought up in this process again.  Without this latch, the
   *next* MPI_T_init_thread() would take the nested-init early exit and
   report a fully working MPI_T that is in fact broken. */
static bool ompi_mpit_init_failed = false;
extern volatile int32_t initted;


int MPI_T_init_thread (int required, int *provided)
{
    int rc = MPI_SUCCESS;

    ompi_mpit_lock ();
    /* Also serialize against instance (world/session) init and teardown:
       a first MPI_T initialization shares unlocked state (OPAL init
       counters, the shared event base refcount, MCA variable and
       framework registration) with instance bring-up on other threads.
       Lock ordering: the MPI_T lock above always comes first; see
       ompi_mpi_instance_lock() in instance.c. */
    ompi_mpi_instance_lock ();

    do {
        if (ompi_mpit_init_failed) {
            rc = MPI_T_ERR_CANNOT_INIT;
            break;
        }

        if (0 != ompi_mpit_init_count++) {
            break;
        }

        /* call opal_init_util to initialize the MCA system */
        rc = opal_init_util (NULL, NULL);
        if (OPAL_SUCCESS != rc) {
            --ompi_mpit_init_count;
            rc = MPI_T_ERR_INVALID;
            break;
        }

        /* Take a reference on the shared event base
           (opal_sync_event_base).  Registering the frameworks below
           bumps every framework's refcount, which defers each
           framework's *true* close until MPI_T_finalize() -- and MPI_T
           is reference counted independently of MPI, so that close can
           legally run after MPI_Finalize() has already run
           opal_finalize().  Components delete events from the shared
           base when they close, so the base must stay alive until our
           deferred closes have run; this reference (released in
           MPI_T_finalize() after ompi_info_close_components()) is what
           keeps it alive. */
        rc = opal_event_register_params ();
        if (OPAL_SUCCESS != rc) {
            (void) opal_finalize_util ();
            --ompi_mpit_init_count;
            rc = MPI_T_ERR_INVALID;
            break;
        }

        rc = opal_event_init ();
        if (OPAL_SUCCESS != rc) {
            (void) opal_finalize_util ();
            --ompi_mpit_init_count;
            rc = MPI_T_ERR_INVALID;
            break;
        }

        /* register all parameters */
        rc = ompi_info_register_framework_params (NULL);
        if (OMPI_SUCCESS != rc) {
            /* Framework registration rolls itself back on failure (the
               project loops close what they registered, and a framework
               whose own registration fails unwinds its reference), so a
               failed init can release everything it acquired and leave
               MPI_T honestly uninitialized: a subsequent
               MPI_T_finalize() returns MPI_T_ERR_NOT_INITIALIZED.

               Retries are still refused (see the latch): the
               non-framework parameter registration inside the info
               machinery (e.g. ompi_mpi_register_params()) is not
               verified to be re-entrant after a partial failure. */
            (void) opal_event_finalize ();
            if (OPAL_ERR_BAD_PARAM != rc) {
                (void) opal_finalize_util ();
            }
            /* else: on BAD_PARAM the registrations were deliberately
               left alive (for ompi_info's diagnostic dump), so the util
               reference underpinning them must be kept -- leaked, like
               the registrations themselves, in this configuration-error
               path. */
            --ompi_mpit_init_count;
            ompi_mpit_init_failed = true;
            rc = MPI_T_ERR_INVALID;
            break;
        }

        /* determine the thread level. TODO -- this might
           be wrong */
        ompi_mpi_thread_level (required, provided);
    } while (0);

    ompi_mpi_instance_unlock ();
    ompi_mpit_unlock ();

    return rc;
}

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
#include "opal/mca/threads/thread_usage.h"
#include "ompi/instance/instance.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_T_init_thread = PMPI_T_init_thread
#endif
#define MPI_T_init_thread PMPI_T_init_thread
#endif

extern opal_mutex_t ompi_mpit_big_lock;

extern volatile uint32_t ompi_mpit_init_count;

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
            /* Nested init: report the level pinned by the first init of
               this epoch (MPI 5.0 sec. 15.3.4: no effect beyond the
               reference count). */
            *provided = ompi_mpit_thread_level;
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

        /* The thread level of the MPI tool information interface is its
           own, scoped to MPI_T routines only (MPI 5.0 sec. 15.3.4).  It
           must NOT be written into the World Model's globals: those back
           MPI_QUERY_THREAD, which is pinned to whatever the original
           MPI_INIT_THREAD returned (sec. 11.6.2).  Writing them here both
           corrupted that report and changed the process's effective
           thread level in mid-flight -- an
           MPI_T_init_thread(MPI_THREAD_SINGLE) inside a THREAD_MULTIPLE
           application silently disabled locking process-wide, and an
           MPI_T_init_thread(MPI_THREAD_MULTIPLE) before a session flipped
           MPI onto threaded code paths the session never chose.

           A tool at any level above MPI_THREAD_SINGLE may legally drive
           MPI_T from a different thread than the one driving MPI proper,
           so OPAL's process-wide thread flags must ratchet up -- and only
           up, because the pinned promises of other live scopes (world,
           sessions) may already depend on them.

           Ratchet only at a quiescent point: when no MPI instance (world
           or session) is active.  The instance lock we hold pins
           ompi_instance_count, and with no active instance a conforming
           program has no thread inside MPI or OPAL, so nothing can read
           these plain flags concurrently with the write.  When MPI *is*
           already active at a lower thread level, the flags cannot be
           changed (the write would race MPI's unlocked hot-path readers,
           and components already selected were configured at the lower
           level) -- so instead of promising concurrency the process
           cannot deliver, grant at most MPI_THREAD_SERIALIZED, the
           coupling that MPI 5.0 secs. 11.6.2/15.3.4 explicitly permit
           ("may influence").  Note that the MPI_T entry points that
           mutate shared state serialize on ompi_mpit_big_lock, but not
           every MPI_T query routine takes it, so the big lock alone is
           not grounds for granting MPI_THREAD_MULTIPLE. */
        if (MPI_THREAD_SINGLE != required) {
            if (0 == ompi_instance_count) {
                /* Write only on transition: a previous MPI_T epoch (or an
                   already-finalized scope) may have set these, and other
                   threads of this tool may be running unlocked MPI_T
                   query routines that read them. */
                if (!opal_using_threads ()) {
                    opal_set_using_threads (true);
                }
                if (opal_single_threaded) {
                    opal_single_threaded = false;
                }
            } else if (!ompi_mpi_thread_multiple
                       && MPI_THREAD_SERIALIZED < required) {
                required = MPI_THREAD_SERIALIZED;
            }
        }
        ompi_mpit_thread_level = required;
        *provided = ompi_mpit_thread_level;
    } while (0);

    ompi_mpi_instance_unlock ();
    ompi_mpit_unlock ();

    return rc;
}

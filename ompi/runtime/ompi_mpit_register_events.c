/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Registration of Open MPI's in-tree MPI_T event producers (sources and
 * event types).  The producer raise sites live in their respective
 * subsystems (the communicator, instance, and error-handler code); this
 * file only declares the event types and hands back the event-type
 * handles those sites raise.  See specs/mpi-t-events/spec.md section 7.
 */

#include "ompi_config.h"

#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/mca/base/mca_base_event.h"
#include "opal/mca/threads/thread_usage.h"

#include "ompi/runtime/ompi_mpit_events.h"

/* The MPI ABI of the registering MPI_T tool (process-global; see the header).
   Hard-coded to the Open MPI ABI until open-mpi/ompi#13280. */
ompi_mpit_abi_t ompi_mpit_callback_abi = OMPI_MPIT_ABI_OMPI;

mca_base_event_t *ompi_event_comm_created = NULL;
mca_base_event_t *ompi_event_comm_freed = NULL;
mca_base_event_t *ompi_event_comm_name_set = NULL;
mca_base_event_t *ompi_event_initialization = NULL;
mca_base_event_t *ompi_event_finalization = NULL;
mca_base_event_t *ompi_event_errhandler_invoked = NULL;
mca_base_event_t *ompi_event_win_created = NULL;
mca_base_event_t *ompi_event_win_freed = NULL;

/* The default-clock domain the producers advertise (nanoseconds). */
#define PRODUCER_TICKS_PER_SECOND ((opal_count_t) 1000000000)

static mca_base_event_t *register_event(const char *framework, const char *component,
                                        const char *name, const char *description,
                                        mca_base_var_info_lvl_t verbosity, int num_elements,
                                        const mca_base_var_type_t *types, const ptrdiff_t *offsets,
                                        mca_base_event_source_t *source)
{
    mca_base_event_t *event = NULL;
    int index = mca_base_event_register("ompi", framework, component, name, description, verbosity,
                                        num_elements, types, offsets, NULL,
                                        MCA_BASE_VAR_BIND_NO_OBJECT, 0, source, NULL);
    if (0 <= index) {
        (void) mca_base_event_get_by_index(index, &event);
    }
    return event;
}

/* Like register_event, but for an object-bound event type (sec. 6.1).  `bind` is
   an MCA_BASE_VAR_BIND_* value, reported verbatim to tools as the MPI_T_BIND_*
   binding (the two enumerations share their ordering); a tool then binds each
   registration to a specific object via MPI_T_event_handle_alloc. */
static mca_base_event_t *register_bound_event(const char *name, const char *description,
                                              mca_base_var_info_lvl_t verbosity, int bind,
                                              int num_elements, const mca_base_var_type_t *types,
                                              const ptrdiff_t *offsets,
                                              mca_base_event_source_t *source)
{
    mca_base_event_t *event = NULL;
    int index = mca_base_event_register("ompi", NULL, NULL, name, description, verbosity,
                                        num_elements, types, offsets, NULL, bind, 0, source, NULL);
    if (0 <= index) {
        (void) mca_base_event_get_by_index(index, &event);
    }
    return event;
}

void ompi_mpit_register_events(void)
{
    static int register_producers = 1;
    int src_index;
    mca_base_event_source_t *ompi_src, *unordered_src;
    /* Element layouts.  A trailing uint64 carrying an MPI handle / instance ID
       is aligned to offset 8 (its natural alignment), so the producer's payload
       struct and these offsets agree. */
    const mca_base_var_type_t t_init[5] = {MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_INT32_T,
                                           MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_INT32_T,
                                           MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_init[5] = {0, 4, 8, 12, 16};
    const mca_base_var_type_t t_final[3] = {MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_INT32_T,
                                            MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_final[3] = {0, 4, 8};
    const mca_base_var_type_t t_cc[2] = {MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_cc[2] = {0, 8};
    const mca_base_var_type_t t_cf[1] = {MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_cf[1] = {0};
    const mca_base_var_type_t t_wc[4] = {MCA_BASE_VAR_TYPE_INT64_T, MCA_BASE_VAR_TYPE_INT32_T,
                                         MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_wc[4] = {0, 8, 12, 16};
    const mca_base_var_type_t t_wf[2] = {MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_wf[2] = {0, 8};
    const mca_base_var_type_t t_eh[4] = {MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_INT32_T,
                                         MCA_BASE_VAR_TYPE_UINT64_T, MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_eh[4] = {0, 4, 8, 16};
    const mca_base_var_type_t t_mem[2] = {MCA_BASE_VAR_TYPE_UINT64_T, MCA_BASE_VAR_TYPE_UINT64_T};
    const ptrdiff_t o_mem[2] = {0, 8};

    /* This entry point is reachable from several MCA-parameter registration
       paths and across MPI_T_init/finalize cycles.  It does not use a one-shot
       guard: registration is idempotent by name (mca_base_event_register etc.
       return the existing index), so a redundant call is cheap, and a call
       after the event registry was torn down (a finalize that dropped the var
       system) correctly re-registers -- keeping the exported set stable. */
    (void) mca_base_var_register("opal", "mca", "base", "event_register_producers",
                                 "Whether to register Open MPI's built-in MPI_T event "
                                 "producers (default: enabled)",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &register_producers);

    /* Clear any handles left from a prior registration cycle before (possibly)
       re-registering.  The OPAL event registry may have been torn down by
       mca_base_event_finalize() since the last call, freeing the event objects
       these globals point at; resetting here ensures neither the disabled path
       below nor a partial (per-source failure) re-registration can leave a
       raise site holding a dangling pointer. */
    ompi_event_comm_created = ompi_event_comm_freed = NULL;
    ompi_event_comm_name_set = NULL;
    ompi_event_initialization = ompi_event_finalization = NULL;
    ompi_event_errhandler_invoked = NULL;
    ompi_event_win_created = ompi_event_win_freed = NULL;

    if (!register_producers) {
        return;
    }

    /* All in-tree producers share a single clock domain (the default,
       MPI_Wtime-equivalent clock).  They are split across only TWO sources, by
       the one characteristic that actually differs -- ordering:

         "ompi"           ORDERED: every event carries a timestamp from the
                          monotonic default clock, so timestamps within this
                          source order the events.  Holds all the lifecycle
                          events (init/finalize, communicator, window,
                          error-handler).
         "ompi.unordered" UNORDERED: the OS memory-release producer aggregates
                          many underlying releases into one delivered event, so
                          per-release chronology is not preserved.

       Both support ad-hoc reads (MPI_T_source_get_timestamp): the default clock
       is freely readable, so a tool can sample it on demand and compare the
       result to event timestamps FROM THE SAME SOURCE.  (Timestamps are only
       comparable within a source, and are not directly comparable to MPI_Wtime:
       same underlying clock, but integer nanoseconds vs. double seconds and a
       different, lazily-captured epoch.)

       Object handles in payloads (communicator/window/error-handler events) and
       the instance ID (init/finalize) are the C handle / object pointer value as
       a uint64 -- an opaque token for identifying and correlating objects (e.g.
       pairing a "created" event with its "freed", or a "finalization" with its
       "initialization"); it must not be dereferenced. */
    src_index = mca_base_event_source_register("ompi", "Open MPI runtime events (ordered)",
                                               MCA_BASE_EVENT_SOURCE_ORDERED,
                                               PRODUCER_TICKS_PER_SECOND, OPAL_COUNT_MAX, NULL, true,
                                               NULL);
    if (0 <= src_index && OPAL_SUCCESS == mca_base_event_source_get_by_index(src_index, &ompi_src)) {
        /* MPI initialization / finalization, for BOTH the world model
           (MPI_Init / MPI_Finalize) and the session model (MPI_Session_init /
           MPI_Session_finalize), which share the same internal instance path.
           The "model" element distinguishes them (0 == world, 1 == session);
           world_rank / world_size are meaningful only when model == world (a
           process has no rank in a session) and are -1 otherwise.  instance_id
           correlates a finalization with its initialization. */
        ompi_event_initialization = register_event(NULL, NULL, "mpi.initialization",
                                                   "An MPI instance was initialized "
                                                   "(MPI_Init or MPI_Session_init)",
                                                   OPAL_INFO_LVL_2, 5, t_init, o_init, ompi_src);
        ompi_event_finalization = register_event(NULL, NULL, "mpi.finalization",
                                                 "An MPI instance was finalized "
                                                 "(MPI_Finalize or MPI_Session_finalize)",
                                                 OPAL_INFO_LVL_2, 3, t_final, o_final, ompi_src);

        /* Communicator lifecycle.  Payloads carry the MPI_Comm handle for
           identification (and to pair "created" with "freed"). */
        ompi_event_comm_created = register_event(NULL, NULL, "mpi.communicator_created",
                                                 "An MPI communicator was created", OPAL_INFO_LVL_2,
                                                 2, t_cc, o_cc, ompi_src);
        ompi_event_comm_freed = register_event(NULL, NULL, "mpi.communicator_freed",
                                               "An MPI communicator was freed", OPAL_INFO_LVL_2, 1,
                                               t_cf, o_cf, ompi_src);

        /* Communicator naming (MPI_Comm_set_name).  Object-bound (sec. 6.1): a
           tool binds a registration to a specific communicator and is notified
           only when THAT communicator is (re)named.  The payload carries the
           MPI_Comm handle (== the bound object); the new name is not in the
           fixed payload -- a callback queries it with MPI_Comm_get_name on the
           communicator it bound to. */
        ompi_event_comm_name_set = register_bound_event("mpi.communicator_name_set",
                                                        "An MPI communicator was named "
                                                        "(MPI_Comm_set_name)",
                                                        OPAL_INFO_LVL_4, MCA_BASE_VAR_BIND_MPI_COMM,
                                                        1, t_cf, o_cf, ompi_src);

        /* Error-handler invocations.  Payload carries the MPI_Errhandler handle
           and the handle of the MPI object the handler is invoked on. */
        ompi_event_errhandler_invoked = register_event(NULL, NULL, "mpi.errhandler_invoked",
                                                       "An MPI error handler was invoked",
                                                       OPAL_INFO_LVL_4, 4, t_eh, o_eh, ompi_src);

        /* RMA window lifecycle.  Payloads carry the MPI_Win handle. */
        ompi_event_win_created = register_event(NULL, NULL, "mpi.win_created",
                                                "An MPI RMA window was created", OPAL_INFO_LVL_4, 4,
                                                t_wc, o_wc, ompi_src);
        ompi_event_win_freed = register_event(NULL, NULL, "mpi.win_freed",
                                              "An MPI RMA window was freed", OPAL_INFO_LVL_4, 2, t_wf,
                                              o_wf, ompi_src);
    }

    /* OS-level memory release tracking (sec. 7.1): the source/event are
       registered here in the OMPI layer; the OPAL memory-release hook (the
       "patcher" memory component intercepting munmap/brk/etc.) and its
       never-freed sink live in OPAL and are wired in via
       mca_base_event_memory_attach().  Registered only where the OPAL memory
       hooks can report releases (absent, e.g., on platforms with no active
       memory component).  The hooks are chunk-level (they fire only when memory
       is actually returned to the OS) and cannot distinguish the releasing API,
       so the payload is a coarse aggregate: {count, bytes} delivered as one
       data-bearing event when the framework drains the sink (sec. 7.1). */
    if (mca_base_event_memory_supported()) {
        src_index = mca_base_event_source_register("ompi.unordered",
                                                   "Open MPI unordered events "
                                                   "(deferred, aggregated delivery)",
                                                   MCA_BASE_EVENT_SOURCE_UNORDERED,
                                                   PRODUCER_TICKS_PER_SECOND, OPAL_COUNT_MAX, NULL,
                                                   true, NULL);
        if (0 <= src_index
            && OPAL_SUCCESS == mca_base_event_source_get_by_index(src_index, &unordered_src)) {
            mca_base_event_t *released
                = register_event(NULL, NULL, "mca.memory.patcher.released",
                                 "Memory was released to the OS (aggregate count and bytes)",
                                 OPAL_INFO_LVL_4, 2, t_mem, o_mem, unordered_src);
            if (NULL != released) {
                mca_base_event_memory_attach(unordered_src, released);
            }
        }
    }
}

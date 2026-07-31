/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Functional reproducer for open-mpi/ompi#14019 ("MPI_T events not
 * implemented"): a minimal tool flow -- initialize MPI_T, confirm at least one
 * event type and source are exported, allocate a handle on the first event,
 * register a callback, and clean up.  Prints "RESULT: HAS_EVENTS" on success.
 */

#include <mpi.h>
#include <stdio.h>

static void noop_cb(MPI_T_event_instance ev, MPI_T_event_registration h, MPI_T_cb_safety s,
                    void *u)
{
    (void) ev; (void) h; (void) s; (void) u;
}

int main(void)
{
    int provided, num_events = 0, num_sources = 0;
    MPI_T_event_registration reg;

    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        printf("RESULT: NO_MPI_T\n");
        return 1;
    }

    MPI_T_event_get_num(&num_events);
    MPI_T_source_get_num(&num_sources);
    if (num_events < 1 || num_sources < 1) {
        printf("RESULT: NO_EVENTS (events=%d sources=%d)\n", num_events, num_sources);
        MPI_T_finalize();
        return 1;
    }

    if (MPI_SUCCESS != MPI_T_event_handle_alloc(0, NULL, MPI_INFO_NULL, &reg)
        || MPI_SUCCESS
               != MPI_T_event_register_callback(reg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                                noop_cb)) {
        printf("RESULT: HANDLE_FAILED\n");
        MPI_T_finalize();
        return 1;
    }
    MPI_T_event_handle_free(reg, NULL, NULL);

    MPI_T_finalize();
    printf("RESULT: HAS_EVENTS (events=%d sources=%d)\n", num_events, num_sources);
    return 0;
}

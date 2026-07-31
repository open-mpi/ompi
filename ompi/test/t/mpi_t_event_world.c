/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * World-model coverage for the ompi.initialization / ompi.finalization events.
 * The producers test exercises the SESSION model (it calls MPI_Init first, so
 * the world-model events have already fired by the time it can register a
 * callback).  Here we initialize MPI_T *before* MPI_Init and register callbacks,
 * so the world-model events -- raised during MPI_Init (once MPI_COMM_WORLD
 * exists) and during MPI_Finalize -- are observed.  We confirm the model
 * payload element says "world" and that world_rank / world_size are populated
 * (they are -1 only for the session model).
 */

#include "opal_config.h"

#include <mpi.h>
#include <stdint.h>
#include <stdio.h>

static int     failures = 0;
static int      init_fired = 0;
static int      final_fired = 0;
static int32_t  init_model = -99;
static int32_t  init_world_rank = -99;
static int32_t  init_world_size = -99;
static uint64_t init_instance_id = 0;
static int32_t  final_model = -99;
static int32_t  final_world_rank = -99;
static uint64_t final_instance_id = 0;

static void expect(const char *what, int cond)
{
    printf("%s: %s\n", cond ? "PASS" : "FAIL", what);
    if (!cond) {
        ++failures;
    }
}

static void init_cb(MPI_T_event_instance ev,
                    MPI_T_event_registration h __opal_attribute_unused__,
                    MPI_T_cb_safety s __opal_attribute_unused__,
                    void *u __opal_attribute_unused__)
{
    ++init_fired;
    (void) MPI_T_event_read(ev, 0, &init_model);
    (void) MPI_T_event_read(ev, 2, &init_world_rank);
    (void) MPI_T_event_read(ev, 3, &init_world_size);
    (void) MPI_T_event_read(ev, 4, &init_instance_id);
}

static void final_cb(MPI_T_event_instance ev,
                     MPI_T_event_registration h __opal_attribute_unused__,
                     MPI_T_cb_safety s __opal_attribute_unused__,
                     void *u __opal_attribute_unused__)
{
    ++final_fired;
    (void) MPI_T_event_read(ev, 0, &final_model);
    (void) MPI_T_event_read(ev, 1, &final_world_rank);
    (void) MPI_T_event_read(ev, 2, &final_instance_id);
}

int main(int argc, char **argv)
{
    int provided, init_idx, final_idx;
    MPI_T_event_registration ireg, freg;

    /* MPI_T BEFORE MPI_Init, so a callback is in place when the world-model
       initialization event fires during MPI_Init. */
    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        printf("SKIP: MPI_T_init_thread failed\n");
        return 77;
    }
    if (MPI_SUCCESS != MPI_T_event_get_index("ompi.mpi.initialization", &init_idx)
        || MPI_SUCCESS != MPI_T_event_get_index("ompi.mpi.finalization", &final_idx)) {
        printf("SKIP: built-in initialization/finalization events not registered\n");
        MPI_T_finalize();
        return 77;
    }

    expect("alloc initialization handle",
           MPI_SUCCESS == MPI_T_event_handle_alloc(init_idx, NULL, MPI_INFO_NULL, &ireg));
    expect("alloc finalization handle",
           MPI_SUCCESS == MPI_T_event_handle_alloc(final_idx, NULL, MPI_INFO_NULL, &freg));
    (void) MPI_T_event_register_callback(ireg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL, init_cb);
    (void) MPI_T_event_register_callback(freg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL, final_cb);

    if (MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        printf("SKIP: MPI_Init failed (no singleton support here)\n");
        return 77;
    }

    expect("initialization fired for the world model during MPI_Init", init_fired >= 1);
    expect("initialization model says world", OMPI_T_MODEL_WORLD == init_model);
    expect("initialization world_rank is a real rank (>= 0)", init_world_rank >= 0);
    expect("initialization world_size is at least 1", init_world_size >= 1);
    expect("initialization carries a non-zero instance id", 0 != init_instance_id);

    MPI_Finalize();

    expect("finalization fired for the world model during MPI_Finalize", final_fired >= 1);
    expect("finalization model says world", OMPI_T_MODEL_WORLD == final_model);
    expect("finalization world_rank is a real rank (>= 0)", final_world_rank >= 0);
    expect("finalization instance id correlates with initialization",
           final_instance_id == init_instance_id);

    /* The handles were allocated on events that outlive MPI_Finalize; free them
       and tear MPI_T down. */
    (void) MPI_T_event_handle_free(ireg, NULL, NULL);
    (void) MPI_T_event_handle_free(freg, NULL, NULL);
    MPI_T_finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}

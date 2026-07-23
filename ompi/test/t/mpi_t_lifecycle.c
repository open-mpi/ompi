/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Tool-only MPI_T lifecycles: no MPI_Init and no sessions anywhere in this
 * process.  Exercises single, nested (MPI_T is reference counted), and
 * repeated full MPI_T init/finalize cycles -- the repeated case drives
 * OPAL's util layer and the MCA var system through full teardown and
 * re-initialization, which is exactly where stale-pointer bugs in
 * "register once" logic like to hide.
 */

#include "mpi_t_lifecycle.h"

int main(void)
{
    int provided;

    pin_tcp_btl();

    /* Plain cycle */
    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (plain)");
    STEP(MPI_T_finalize(), "T_finalize (plain)");

    /* Nested: only the outermost init/last finalize do real work */
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (nested, 1)");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (nested, 2)");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (nested, 3)");
    STEP(MPI_T_finalize(), "T_finalize (nested, 3)");
    STEP(MPI_T_finalize(), "T_finalize (nested, 2)");
    STEP(MPI_T_finalize(), "T_finalize (nested, 1)");

    /* Repeated full cycles */
    for (int i = 0; i < 3; ++i) {
        STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (repeat)");
        STEP(MPI_T_finalize(), "T_finalize (repeat)");
    }

    printf("SUCCESS: tool-only MPI_T lifecycles\n");
    return 0;
}

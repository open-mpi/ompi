/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * MPI_T vs. the world model, ordering: MPI_T entirely inside MPI.
 *
 *     MPI_Init() -> MPI_T_init_thread() -> MPI_T_finalize() -> MPI_Finalize()
 *
 * The tamest ordering: MPI's own references cover everything MPI_T touches.
 * Each world-model ordering needs its own executable because MPI_Init() is
 * legal only once per process.
 */

#include "mpi_t_lifecycle.h"

int main(void)
{
    int provided;

    pin_tcp_btl();

    STEP_OR_SKIP(MPI_Init(NULL, NULL), "MPI_Init (singleton)");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (inside MPI)");
    STEP(MPI_T_finalize(), "T_finalize (inside MPI)");

    /* A second MPI_T epoch requesting MPI_THREAD_MULTIPLE while the
       (SINGLE-level) world is active: the process cannot be safely
       upgraded mid-flight, so the grant must be capped at
       MPI_THREAD_SERIALIZED (MPI 5.0 secs. 11.6.2/15.3.4 permit the
       earlier scope's level to influence a later init's grant). */
    STEP(MPI_T_init_thread(MPI_THREAD_MULTIPLE, &provided), "T_init (MULTIPLE inside SINGLE MPI)");
    if (MPI_THREAD_SERIALIZED != provided) {
        printf("FAIL: T provided %d inside SINGLE world, want MPI_THREAD_SERIALIZED\n", provided);
        return 1;
    }
    STEP(MPI_T_finalize(), "T_finalize (second epoch)");

    STEP(MPI_Finalize(), "MPI_Finalize");

    printf("SUCCESS: Init -> T_init -> T_finalize -> Finalize\n");
    return 0;
}

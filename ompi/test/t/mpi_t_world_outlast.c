/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * MPI_T vs. the world model, ordering: MPI_T starts inside MPI and outlives
 * it, with nested MPI_T references straddling MPI_Finalize().
 *
 *     MPI_Init() -> MPI_T_init_thread() x2 -> MPI_Finalize()
 *                -> MPI_T_finalize() -> MPI_T_finalize()
 *
 * The first MPI_T_finalize() after MPI_Finalize() is a pure refcount
 * decrement; the second is the one that performs the deferred component
 * closes, after MPI (and OPAL's full layer) is gone.  This is the same
 * deferred-close hazard as the T-brackets-MPI ordering in
 * mpi_t_outlives_mpi.c, reached through a different path.  Each world-model
 * ordering needs its own executable because MPI_Init() is legal only once
 * per process.
 */

#include "mpi_t_lifecycle.h"

int main(void)
{
    int provided;

    pin_tcp_btl();

    STEP_OR_SKIP(MPI_Init(NULL, NULL), "MPI_Init (singleton)");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (inside MPI, 1)");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (inside MPI, 2)");
    STEP(MPI_Finalize(), "MPI_Finalize");
    STEP(MPI_T_finalize(), "T_finalize (after MPI, inner ref)");
    STEP(MPI_T_finalize(), "T_finalize (after MPI, last ref: deferred closes)");

    printf("SUCCESS: Init -> T_init x2 -> Finalize -> T_finalize x2\n");
    return 0;
}

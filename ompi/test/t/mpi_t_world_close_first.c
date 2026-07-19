/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * MPI_T vs. the world model, ordering: MPI_T opens first but closes first.
 *
 *     MPI_T_init_thread() -> MPI_Init() -> MPI_T_finalize() -> MPI_Finalize()
 *
 * MPI_T_finalize() runs its component closes while MPI is still alive, so
 * every framework close it performs is a mere refcount decrement (MPI holds
 * the frameworks too); the true closes happen inside MPI_Finalize().  Each
 * world-model ordering needs its own executable because MPI_Init() is legal
 * only once per process.
 */

#include "mpi_t_lifecycle.h"

int main(void)
{
    int provided;

    pin_tcp_btl();

    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init");

    /* The standard tool pattern: MPI_T initialized (e.g. by a PMPI
       wrapper) before the application's MPI_Init_thread().  The active
       MPI_T epoch must not cost the application its requested thread
       level. */
    STEP_OR_SKIP(MPI_Init_thread(NULL, NULL, MPI_THREAD_MULTIPLE, &provided),
                 "MPI_Init_thread (MULTIPLE, singleton, tool attached)");
    if (MPI_THREAD_MULTIPLE != provided) {
        printf("FAIL: world provided %d under an MPI_T epoch, want MPI_THREAD_MULTIPLE\n",
               provided);
        return 1;
    }

    STEP(MPI_T_finalize(), "T_finalize (MPI still alive)");
    STEP(MPI_Finalize(), "MPI_Finalize");

    printf("SUCCESS: T_init -> Init -> T_finalize -> Finalize\n");
    return 0;
}

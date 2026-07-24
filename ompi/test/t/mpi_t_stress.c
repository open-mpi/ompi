/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Stress: 10,000 full MPI_T init/finalize cycles with no MPI (world or
 * sessions) anywhere in the process.  Every cycle tears OPAL's util layer
 * and the MCA var system all the way down and back up, so growth bugs --
 * fd leaks, heap growth in registration, stale-pointer reuse across var
 * system generations -- surface as a failure or a crash long before the
 * loop ends.
 */

#include "mpi_t_lifecycle.h"

#define CYCLES 10000

int main(void)
{
    int provided, rc;

    pin_tcp_btl();

    /* First cycle separately: distinguish "cannot run here at all" (skip)
       from "degraded after N cycles" (failure). */
    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        printf("SKIP: MPI_T_init_thread() failed; cannot test here\n");
        return 77;
    }
    if (MPI_SUCCESS != MPI_T_finalize()) {
        printf("FAIL: MPI_T_finalize() failed on cycle 0\n");
        return 1;
    }

    for (int i = 1; i < CYCLES; ++i) {
        if (MPI_SUCCESS != (rc = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided))) {
            printf("FAIL: MPI_T_init_thread() returned %d on cycle %d\n", rc, i);
            return 1;
        }
        if (MPI_SUCCESS != (rc = MPI_T_finalize())) {
            printf("FAIL: MPI_T_finalize() returned %d on cycle %d\n", rc, i);
            return 1;
        }
    }

    printf("SUCCESS: %d MPI_T init/finalize cycles\n", CYCLES);
    return 0;
}

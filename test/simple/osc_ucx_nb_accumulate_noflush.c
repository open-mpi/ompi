/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define NELEM 64
#define BURST 16

int main(int argc, char **argv)
{
    int rank, size, i, it, errs = 0;
    MPI_Win win;
    double *winbuf, *origin;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Win_allocate(NELEM * sizeof(double), sizeof(double), MPI_INFO_NULL,
                     MPI_COMM_WORLD, &winbuf, &win);
    for (i = 0; i < NELEM; i++) {
        winbuf[i] = 0.0;
    }
    origin = malloc(NELEM * sizeof(double));
    for (i = 0; i < NELEM; i++) {
        origin[i] = 1.0;
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* Phase 1: shared lock, burst of accumulates with NO flush in between.
     * Every accumulate after the first has to acquire the remote accumulate
     * lock that only the deferred finalize of the previous one releases. */
    MPI_Win_lock_all(0, win);
    for (it = 0; it < BURST; it++) {
        MPI_Accumulate(origin, NELEM, MPI_DOUBLE, 0, 0, NELEM, MPI_DOUBLE,
                       MPI_SUM, win);
    }
    MPI_Win_unlock_all(win);

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        double expect = (double)size * BURST;
        for (i = 0; i < NELEM; i++) {
            if (winbuf[i] != expect) {
                fprintf(stderr, "MISMATCH at %d: got %g expected %g\n", i,
                        winbuf[i], expect);
                errs++;
            }
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* Phase 2: exclusive lock, same un-flushed burst.  With an exclusive lock
     * the accumulate lock is skipped, so this exercises the second blocking
     * site: the overlap bookkeeping loop in check_ops_and_flush(). */
    for (i = 0; i < size; i++) {
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, i, 0, win);
        for (it = 0; it < BURST; it++) {
            MPI_Accumulate(origin, NELEM, MPI_DOUBLE, i, 0, NELEM, MPI_DOUBLE,
                           MPI_SUM, win);
        }
        MPI_Win_unlock(i, win);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    {
        /* Phase 1 only targeted rank 0, phase 2 targeted every rank. */
        double expect = (rank == 0) ? (double)size * BURST * 2.0
                                    : (double)size * BURST;
        for (i = 0; i < NELEM; i++) {
            if (winbuf[i] != expect) {
                fprintf(stderr, "MISMATCH at %d after exclusive phase: got %g "
                        "expected %g\n", i, winbuf[i], expect);
                errs++;
            }
        }
    }

    if (rank == 0 && errs == 0) {
        printf("PASS: un-flushed nonblocking accumulate bursts completed "
               "without deadlock\n");
    }

    free(origin);
    MPI_Win_free(&win);
    MPI_Finalize();
    return errs ? 1 : 0;
}

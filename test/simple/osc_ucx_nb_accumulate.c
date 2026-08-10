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

#define MAXN 4096
#define ITERS 32

int main(int argc, char **argv)
{
    int rank, size, i, it, errs = 0;
    MPI_Win win;
    double *winbuf, *origin, *result;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Win_allocate(MAXN * sizeof(double), sizeof(double), MPI_INFO_NULL,
                     MPI_COMM_WORLD, &winbuf, &win);
    for (i = 0; i < MAXN; i++) {
        winbuf[i] = 0.0;
    }
    origin = malloc(MAXN * sizeof(double));
    result = malloc(MAXN * sizeof(double));
    for (i = 0; i < MAXN; i++) {
        origin[i] = 1.0;
    }

    /* Flush after each op so the completion callback runs the fenced finalize. */
    for (it = 0; it < ITERS; it++) {
        int n = 1 << (it % 13); /* 1 .. 4096 */
        if (n > MAXN) {
            n = MAXN;
        }
        MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, win);
        MPI_Accumulate(origin, n, MPI_DOUBLE, 0, 0, n, MPI_DOUBLE, MPI_SUM, win);
        MPI_Win_flush(0, win);
        MPI_Win_unlock(0, win);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    for (it = 0; it < ITERS; it++) {
        MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, win);
        MPI_Get_accumulate(origin, 1, MPI_DOUBLE, result, 1, MPI_DOUBLE, 0, 0, 1,
                           MPI_DOUBLE, MPI_SUM, win);
        MPI_Win_flush(0, win);
        MPI_Win_unlock(0, win);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        double expect = (double)size * (ITERS + ITERS);
        if (winbuf[0] != expect) {
            fprintf(stderr, "MISMATCH at 0: got %g expected %g\n", winbuf[0],
                    expect);
            errs++;
        }
        if (errs == 0) {
            printf("PASS: nonblocking accumulate + get-accumulate completed "
                   "without re-entry\n");
        }
    }

    free(origin);
    free(result);
    MPI_Win_free(&win);
    MPI_Finalize();
    return errs ? 1 : 0;
}

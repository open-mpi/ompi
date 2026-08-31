/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include <mpi.h>
#include <stdio.h>

#define N 16

/*
 * Collectively zero the window buffer on rank 0.  The fences use no
 * assertions: an epoch that follows immediately afterwards does issue
 * RMA, so MPI_MODE_NOSUCCEED / MPI_MODE_NOPRECEDE would be untrue here.
 */
static void reset_win(MPI_Win win, double *winbuf, int rank)
{
    int i;

    MPI_Win_fence(0, win);
    if (rank == 0) {
        for (i = 0; i < N; i++) {
            winbuf[i] = 0.0;
        }
    }
    MPI_Win_fence(0, win);
}

/*
 * Verify on rank 0 that every window element accumulated the sum over all
 * ranks.  Returns the number of mismatches found.
 */
static int check_win(const char *tag, const double *winbuf, int rank,
                     double sum_of_ranks)
{
    int i, errs = 0;
    double expect;

    if (rank != 0) {
        return 0;
    }

    for (i = 0; i < N; i++) {
        expect = (double)(i + 1) * sum_of_ranks;
        if (winbuf[i] != expect) {
            fprintf(stderr, "%s MISMATCH at %d: got %g exp %g\n", tag,
                    i, winbuf[i], expect);
            errs++;
        }
    }

    return errs;
}

int main(int argc, char **argv)
{
    int rank, size, i, errs = 0;
    MPI_Win win;
    double *winbuf, origin[N], result[N];
    MPI_Datatype abs_dt;
    MPI_Aint disp;
    int blocklen = N;
    double sum_of_ranks;
    MPI_Request req;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Win_allocate(N * sizeof(double), sizeof(double), MPI_INFO_NULL,
                     MPI_COMM_WORLD, &winbuf, &win);
    for (i = 0; i < N; i++) {
        origin[i] = (double)(rank + 1) * (i + 1);
    }

    /* Absolute-address origin: block at its own address, passed as MPI_BOTTOM. */
    MPI_Get_address(&origin[0], &disp);
    MPI_Type_create_hindexed(1, &blocklen, &disp, MPI_DOUBLE, &abs_dt);
    MPI_Type_commit(&abs_dt);

    sum_of_ranks = (double)size * (size + 1) / 2.0;

    reset_win(win, winbuf, rank);
    MPI_Win_fence(0, win);
    MPI_Accumulate(MPI_BOTTOM, 1, abs_dt, 0, 0, N, MPI_DOUBLE, MPI_SUM, win);
    MPI_Win_fence(0, win);
    errs += check_win("accumulate", winbuf, rank, sum_of_ranks);

    reset_win(win, winbuf, rank);
    MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, win);
    MPI_Raccumulate(MPI_BOTTOM, 1, abs_dt, 0, 0, N, MPI_DOUBLE, MPI_SUM, win,
                    &req);
    MPI_Wait(&req, MPI_STATUS_IGNORE);
    MPI_Win_unlock(0, win);
    MPI_Barrier(MPI_COMM_WORLD);
    errs += check_win("raccumulate", winbuf, rank, sum_of_ranks);

    reset_win(win, winbuf, rank);
    for (i = 0; i < N; i++) {
        result[i] = -1.0;
    }
    MPI_Win_fence(0, win);
    MPI_Get_accumulate(MPI_BOTTOM, 1, abs_dt, result, N, MPI_DOUBLE, 0, 0, N,
                       MPI_DOUBLE, MPI_SUM, win);
    MPI_Win_fence(0, win);
    errs += check_win("get_accumulate", winbuf, rank, sum_of_ranks);

    reset_win(win, winbuf, rank);
    MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, win);
    MPI_Rget_accumulate(MPI_BOTTOM, 1, abs_dt, result, N, MPI_DOUBLE, 0, 0, N,
                        MPI_DOUBLE, MPI_SUM, win, &req);
    MPI_Wait(&req, MPI_STATUS_IGNORE);
    MPI_Win_unlock(0, win);
    MPI_Barrier(MPI_COMM_WORLD);
    errs += check_win("rget_accumulate", winbuf, rank, sum_of_ranks);

    if (rank == 0 && errs == 0) {
        printf("PASS: MPI_BOTTOM absolute-datatype accumulate (all four APIs) "
               "correct\n");
    }

    MPI_Type_free(&abs_dt);
    MPI_Win_free(&win);
    MPI_Finalize();
    return errs ? 1 : 0;
}

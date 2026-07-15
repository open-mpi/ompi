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

#define N 256

struct di {
    double v;
    int i;
};

int main(int argc, char **argv)
{
    int rank, size, i, errs = 0;
    MPI_Win win;
    struct di *winbuf, *origin, *result;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Win_allocate(N * sizeof(struct di), sizeof(struct di), MPI_INFO_NULL,
                     MPI_COMM_WORLD, &winbuf, &win);
    origin = malloc(N * sizeof(struct di));
    result = malloc(N * sizeof(struct di));
    for (i = 0; i < N; i++) {
        origin[i].v = (double)(rank + 1) * (i + 1);
        origin[i].i = rank;
    }

    for (i = 0; i < N; i++) {
        winbuf[i].v = -1e300;
        winbuf[i].i = -1;
    }
    MPI_Win_fence(0, win);
    MPI_Accumulate(origin, N, MPI_DOUBLE_INT, 0, 0, N, MPI_DOUBLE_INT,
                   MPI_MAXLOC, win);
    MPI_Win_fence(0, win);
    if (rank == 0) {
        for (i = 0; i < N; i++) {
            if (winbuf[i].v != (double)size * (i + 1) || winbuf[i].i != size - 1) {
                fprintf(stderr, "accumulate MISMATCH at %d: (%g,%d) exp (%g,%d)\n",
                        i, winbuf[i].v, winbuf[i].i, (double)size * (i + 1),
                        size - 1);
                errs++;
            }
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Win_fence(0, win);
    if (rank == 0) {
        for (i = 0; i < N; i++) {
            winbuf[i].v = -2e300;
            winbuf[i].i = -2;
        }
    }
    MPI_Win_fence(0, win);
    for (i = 0; i < N; i++) {
        result[i].v = 0.0;
        result[i].i = -9;
    }
    MPI_Win_fence(0, win);
    if (rank == 0) {
        MPI_Get_accumulate(origin, N, MPI_DOUBLE_INT, result, N, MPI_DOUBLE_INT,
                           0, 0, N, MPI_DOUBLE_INT, MPI_MAXLOC, win);
    }
    MPI_Win_fence(0, win);
    if (rank == 0) {
        for (i = 0; i < N; i++) {
            if (winbuf[i].v != (double)(i + 1) || winbuf[i].i != 0) {
                fprintf(stderr, "get_acc win MISMATCH at %d: (%g,%d) exp (%g,%d)\n",
                        i, winbuf[i].v, winbuf[i].i, (double)(i + 1), 0);
                errs++;
            }
            if (result[i].v != -2e300 || result[i].i != -2) {
                fprintf(stderr, "get_acc result MISMATCH at %d: (%g,%d)\n", i,
                        result[i].v, result[i].i);
                errs++;
            }
        }
    }

    if (rank == 0 && errs == 0) {
        printf("PASS: padded MPI_DOUBLE_INT MAXLOC accumulate + get-accumulate "
               "correct\n");
    }

    free(origin);
    free(result);
    MPI_Win_free(&win);
    MPI_Finalize();
    return errs ? 1 : 0;
}

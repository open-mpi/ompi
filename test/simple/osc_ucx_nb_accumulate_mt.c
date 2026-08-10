/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <mpi.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NTHREADS 2
#define LOOPS    200

static MPI_Win        g_win;
static int            g_rank;

/* Portable start gate (pthread_barrier_t is not available on macOS). */
static pthread_mutex_t g_gate_mtx  = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  g_gate_cond = PTHREAD_COND_INITIALIZER;
static int             g_gate_count;

static void start_gate(void)
{
    pthread_mutex_lock(&g_gate_mtx);
    if (++g_gate_count == NTHREADS) {
        pthread_cond_broadcast(&g_gate_cond);
    } else {
        while (g_gate_count < NTHREADS) {
            pthread_cond_wait(&g_gate_cond, &g_gate_mtx);
        }
    }
    pthread_mutex_unlock(&g_gate_mtx);
}

typedef struct {
    int tid;
} thread_arg_t;

static void *worker(void *arg)
{
    thread_arg_t *ta = (thread_arg_t *)arg;
    int slot         = g_rank * NTHREADS + ta->tid;
    double one       = 1.0;
    int it;

    start_gate();

    for (it = 0; it < LOOPS; it++) {
        MPI_Accumulate(&one, 1, MPI_DOUBLE, 0, slot, 1, MPI_DOUBLE, MPI_SUM,
                       g_win);
        MPI_Win_flush(0, g_win);
    }
    return NULL;
}

int main(int argc, char **argv)
{
    int provided, size, i, errs = 0;
    double *winbuf;
    pthread_t threads[NTHREADS];
    thread_arg_t args[NTHREADS];

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (provided != MPI_THREAD_MULTIPLE) {
        fprintf(stderr, "SKIP: MPI_THREAD_MULTIPLE not provided\n");
        MPI_Finalize();
        return 77;
    }
    MPI_Comm_rank(MPI_COMM_WORLD, &g_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Win_allocate(size * NTHREADS * sizeof(double), sizeof(double),
                     MPI_INFO_NULL, MPI_COMM_WORLD, &winbuf, &g_win);
    for (i = 0; i < size * NTHREADS; i++) {
        winbuf[i] = 0.0;
    }
    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Win_lock_all(0, g_win);
    for (i = 0; i < NTHREADS; i++) {
        args[i].tid = i;
        pthread_create(&threads[i], NULL, worker, &args[i]);
    }
    for (i = 0; i < NTHREADS; i++) {
        pthread_join(threads[i], NULL);
    }
    MPI_Win_unlock_all(g_win);

    MPI_Barrier(MPI_COMM_WORLD);

    if (g_rank == 0) {
        for (i = 0; i < size * NTHREADS; i++) {
            if (winbuf[i] != (double)LOOPS) {
                fprintf(stderr, "MISMATCH at slot %d: got %g expected %d\n", i,
                        winbuf[i], LOOPS);
                errs++;
            }
        }
        if (errs == 0) {
            printf("PASS: THREAD_MULTIPLE concurrent accumulate (feature gated "
                   "to blocking)\n");
        }
    }

    MPI_Win_free(&g_win);
    MPI_Finalize();
    return errs ? 1 : 0;
}

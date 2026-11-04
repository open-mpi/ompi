/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Point-to-point traffic from several threads over one communicator.
 *
 * Derived from CP2K's message_passing_thread_unittest, which runs a
 * ring MPI_Sendrecv from two OpenMP threads of every rank, all of them
 * on the same communicator and told apart only by their tag:
 *
 *   https://github.com/cp2k/cp2k/blob/master/src/mpiwrap/
 *   message_passing_thread_unittest.F
 *
 * That test is what caught a hang in the shared memory fast boxes.  A
 * fast box is armed once a peer has been sent enough messages, and
 * from then on every sender thread reserves a slot in the one ring the
 * receiver reads strictly in order.  Threads that did not draw their
 * slot and its sequence number as one step left the receiver waiting
 * for a number that never arrived, with both ranks inside
 * MPI_Sendrecv.  CP2K runs 128 rounds and gets there about one run in
 * four; the default here is long enough to make it a certainty while
 * still finishing in a moment when nothing is wrong.
 *
 * A regression is as likely to hang as to fail, so launch with
 * --timeout.  Run it with btl self,sm to be sure the fast box is the
 * path under test.
 */

#include <mpi.h>

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define DEFAULT_THREADS    2
#define DEFAULT_ITERATIONS 4000

struct worker {
    pthread_t id;
    MPI_Comm comm;
    int rank;
    int size;
    int thread;
    int nthreads;
    int iterations;
    int errors;
};

static void *exercise(void *arg)
{
    struct worker *me = (struct worker *) arg;
    int dest = (me->rank + 1) % me->size;
    int source = (me->rank - 1 + me->size) % me->size;

    for (int iter = 1; iter <= me->iterations; ++iter) {
        /* Same scheme as CP2K: the tag names the thread and the round,
           so every message has exactly one receiver. */
        int tag = (iter - 1) * me->nthreads + me->thread;
        int outgoing = (me->size * (iter - 1) + me->rank) * me->nthreads + me->thread;
        int expected = (me->size * (iter - 1) + source) * me->nthreads + me->thread;
        int incoming = -1;

        MPI_Sendrecv(&outgoing, 1, MPI_INT, dest, tag, &incoming, 1, MPI_INT, source, tag,
                     me->comm, MPI_STATUS_IGNORE);

        if (expected != incoming) {
            fprintf(stderr, "ERROR: rank %d thread %d round %d: expected %d from rank %d, got %d\n",
                    me->rank, me->thread, iter, expected, source, incoming);
            ++me->errors;
        }
    }
    return NULL;
}

int main(int argc, char **argv)
{
    int nthreads = DEFAULT_THREADS;
    int iterations = DEFAULT_ITERATIONS;
    int provided, rank, size, errors = 0, total_errors;

    if (argc > 1) {
        nthreads = atoi(argv[1]);
    }
    if (argc > 2) {
        iterations = atoi(argv[2]);
    }
    if (nthreads < 1 || iterations < 1) {
        fprintf(stderr, "usage: %s [threads [iterations]]\n", argv[0]);
        return 2;
    }

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "ERROR: MPI_THREAD_MULTIPLE not provided (got %d)\n", provided);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    struct worker *workers = calloc((size_t) nthreads, sizeof(*workers));
    if (NULL == workers) {
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (int t = 0; t < nthreads; ++t) {
        workers[t] = (struct worker){.comm = MPI_COMM_WORLD,
                                     .rank = rank,
                                     .size = size,
                                     .thread = t,
                                     .nthreads = nthreads,
                                     .iterations = iterations};
        if (0 != pthread_create(&workers[t].id, NULL, exercise, &workers[t])) {
            fprintf(stderr, "ERROR: rank %d: pthread_create failed\n", rank);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }
    for (int t = 0; t < nthreads; ++t) {
        pthread_join(workers[t].id, NULL);
        errors += workers[t].errors;
    }
    free(workers);

    MPI_Allreduce(&errors, &total_errors, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (0 == rank) {
        printf("thread_sendrecv: %d ranks x %d threads x %d rounds: %s\n", size, nthreads,
               iterations, 0 == total_errors ? "ok" : "FAILED");
    }

    MPI_Finalize();
    return 0 == total_errors ? 0 : 1;
}

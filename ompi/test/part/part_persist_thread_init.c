/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Kankawee Aramrak.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Single-process (no launcher required) test for part/persist progress
 * fast-path and thread-safe initialization under MPI_THREAD_MULTIPLE:
 *
 *  (a) Exercise MPI_Test() / opal_progress() while no partitioned
 *      communication requests have been initialized (init_world == -1).
 *  (b) Concurrently transition init_world (-1 -> 0 -> 1) via
 *      MPI_Psend_init / MPI_Precv_init on one thread while peer threads
 *      hammer MPI_Test() / opal_progress() under MPI_THREAD_MULTIPLE,
 *      then drive the partitioned requests to completion and free them.
 */

#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"

#define NUM_POLL_THREADS 3
#define NUM_THREADS      (1 + NUM_POLL_THREADS)
#define PARTITIONS       4
#define COUNT            3
#define MAX_POLL         10000000

static pthread_mutex_t barrier_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t start_cond = PTHREAD_COND_INITIALIZER;
static int barrier_arrived = 0;
static int start_gen = 0;
static bool aborted = false;
static int failures = 0;
static pthread_mutex_t fail_lock = PTHREAD_MUTEX_INITIALIZER;

static double part_sendbuf[PARTITIONS * COUNT];
static double part_recvbuf[PARTITIONS * COUNT];
static MPI_Request part_sreq = MPI_REQUEST_NULL;
static MPI_Request part_rreq = MPI_REQUEST_NULL;

static bool sync_start_barrier(void)
{
    int gen;
    bool ok;

    pthread_mutex_lock(&barrier_lock);
    if (aborted) {
        pthread_mutex_unlock(&barrier_lock);
        return false;
    }

    gen = start_gen;
    ++barrier_arrived;
    if (barrier_arrived >= NUM_THREADS) {
        barrier_arrived = 0;
        ++start_gen;
        pthread_cond_broadcast(&start_cond);
    } else {
        while (!aborted && gen == start_gen) {
            pthread_cond_wait(&start_cond, &barrier_lock);
        }
    }

    ok = !aborted;
    pthread_mutex_unlock(&barrier_lock);
    return ok;
}

static void abort_start_barrier(void)
{
    pthread_mutex_lock(&barrier_lock);
    aborted = true;
    pthread_cond_broadcast(&start_cond);
    pthread_mutex_unlock(&barrier_lock);
}

static void record_failure(const char *msg)
{
    pthread_mutex_lock(&fail_lock);
    fprintf(stderr, "FAIL: %s\n", msg);
    ++failures;
    pthread_mutex_unlock(&fail_lock);
}

static void *poll_worker(void *arg)
{
    int tid = *(int *) arg;
    int sbuf = 1000 + tid;
    int rbuf = -1;
    MPI_Request req = MPI_REQUEST_NULL;
    int flag = 0;
    int i;

    if (MPI_SUCCESS != MPI_Irecv(&rbuf, 1, MPI_INT, 0, 200 + tid,
                                 MPI_COMM_SELF, &req)) {
        record_failure("poll_worker: MPI_Irecv failed");
        (void) sync_start_barrier();
        return NULL;
    }

    /* Synchronize with part_worker to race opal_progress() against -1 -> 0. */
    if (!sync_start_barrier()) {
        MPI_Cancel(&req);
        MPI_Wait(&req, MPI_STATUS_IGNORE);
        return NULL;
    }

    for (i = 0; i < 2000; ++i) {
        if (MPI_SUCCESS != MPI_Test(&req, &flag, MPI_STATUS_IGNORE)) {
            record_failure("poll_worker: MPI_Test failed");
            break;
        }
        if (flag) {
            record_failure("poll_worker: MPI_Irecv completed early");
            break;
        }
    }

    if (MPI_SUCCESS != MPI_Send(&sbuf, 1, MPI_INT, 0, 200 + tid,
                                MPI_COMM_SELF)) {
        record_failure("poll_worker: MPI_Send failed");
    }
    if (MPI_SUCCESS != MPI_Wait(&req, MPI_STATUS_IGNORE)) {
        record_failure("poll_worker: MPI_Wait failed");
    } else if (sbuf != rbuf) {
        record_failure("poll_worker: received data mismatch");
    }

    return NULL;
}

static void *part_worker(void *arg)
{
    int i, rc;

    (void) arg;

    for (i = 0; i < PARTITIONS * COUNT; ++i) {
        part_sendbuf[i] = (double) i;
        part_recvbuf[i] = -1.0;
    }

    /* Synchronize with poll_workers to race -1 -> 0 init_world transition. */
    if (!sync_start_barrier()) {
        return NULL;
    }

    rc = MPI_Psend_init(part_sendbuf, PARTITIONS, COUNT, MPI_DOUBLE, 0, 1,
                        MPI_COMM_SELF, MPI_INFO_NULL, &part_sreq);
    if (MPI_SUCCESS != rc) {
        record_failure("part_worker: MPI_Psend_init failed");
        return NULL;
    }

    rc = MPI_Precv_init(part_recvbuf, PARTITIONS, COUNT, MPI_DOUBLE, 0, 1,
                        MPI_COMM_SELF, MPI_INFO_NULL, &part_rreq);
    if (MPI_SUCCESS != rc) {
        record_failure("part_worker: MPI_Precv_init failed");
    }

    return NULL;
}

static void complete_and_free_part_requests(void)
{
    int sreq_started = 0;
    int rreq_started = 0;
    int i, flag, polls, rc;

    if (MPI_REQUEST_NULL == part_sreq || MPI_REQUEST_NULL == part_rreq) {
        goto cleanup;
    }

    if (MPI_SUCCESS != MPI_Start(&part_sreq)) {
        record_failure("MPI_Start(part_sreq) failed");
        goto cleanup;
    }
    sreq_started = 1;

    if (MPI_SUCCESS != MPI_Start(&part_rreq)) {
        record_failure("MPI_Start(part_rreq) failed");
        goto cleanup;
    }
    rreq_started = 1;

    for (i = 0; i < PARTITIONS; ++i) {
        if (MPI_SUCCESS != MPI_Pready(i, part_sreq)) {
            record_failure("MPI_Pready failed");
            break;
        }
        flag = 0;
        for (polls = 0; !flag && polls < MAX_POLL; ++polls) {
            rc = MPI_Parrived(part_rreq, i, &flag);
            if (MPI_SUCCESS != rc) {
                record_failure("MPI_Parrived failed");
                break;
            }
        }
        if (!flag) {
            record_failure("MPI_Parrived timed out");
        }
    }

cleanup:
    if (sreq_started && MPI_SUCCESS != MPI_Wait(&part_sreq, MPI_STATUS_IGNORE)) {
        record_failure("MPI_Wait(part_sreq) failed");
    }
    if (rreq_started && MPI_SUCCESS != MPI_Wait(&part_rreq, MPI_STATUS_IGNORE)) {
        record_failure("MPI_Wait(part_rreq) failed");
    }

    if (sreq_started && rreq_started) {
        for (i = 0; i < PARTITIONS * COUNT; ++i) {
            if (part_recvbuf[i] != (double) i) {
                record_failure("part_recvbuf data mismatch");
                break;
            }
        }
    }

    if (MPI_REQUEST_NULL != part_sreq
        && MPI_SUCCESS != MPI_Request_free(&part_sreq)) {
        record_failure("MPI_Request_free(part_sreq) failed");
    }
    if (MPI_REQUEST_NULL != part_rreq
        && MPI_SUCCESS != MPI_Request_free(&part_rreq)) {
        record_failure("MPI_Request_free(part_rreq) failed");
    }
}

int main(int argc, char **argv)
{
    int provided = 0;
    int buf = 42;
    int rbuf = 0;
    MPI_Request req = MPI_REQUEST_NULL;
    int flag = 0;
    int i;
    pthread_t threads[NUM_THREADS];
    int tids[NUM_THREADS];
    int created = 0;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);

    if (MPI_THREAD_MULTIPLE > provided) {
        fprintf(stderr, "SKIP: MPI_THREAD_MULTIPLE not supported (provided=%d)\n",
                provided);
        MPI_Finalize();
        return 77;
    }

    MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);

    /* (a) Exercise opal_progress() / mca_part_persist_progress() while
     * no partitioned requests have been initialized (init_world == -1). */
    if (MPI_SUCCESS != MPI_Irecv(&rbuf, 1, MPI_INT, 0, 999, MPI_COMM_SELF,
                                 &req)) {
        record_failure("MPI_Irecv failed");
    }
    for (i = 0; i < 1000; ++i) {
        if (MPI_SUCCESS != MPI_Test(&req, &flag, MPI_STATUS_IGNORE)) {
            record_failure("MPI_Test on uncompleted Irecv failed");
            break;
        }
        if (flag) {
            record_failure("MPI_Irecv unexpectedly completed early");
            break;
        }
    }
    if (MPI_SUCCESS != MPI_Send(&buf, 1, MPI_INT, 0, 999, MPI_COMM_SELF)
        || MPI_SUCCESS != MPI_Wait(&req, MPI_STATUS_IGNORE) || buf != rbuf) {
        record_failure("non-partitioned self send/recv failed");
    }

    /* (b) Concurrent partitioned initialization (-1 -> 0 -> 1) against
     * peer threads polling opal_progress() under MPI_THREAD_MULTIPLE. */
    for (i = 0; i < NUM_THREADS; ++i) {
        void *(*worker_fn)(void *) = (0 == i) ? part_worker : poll_worker;
        tids[i] = i;
        if (0 != pthread_create(&threads[i], NULL, worker_fn, &tids[i])) {
            record_failure("pthread_create failed");
            abort_start_barrier();
            break;
        }
        ++created;
    }

    for (i = 0; i < created; ++i) {
        pthread_join(threads[i], NULL);
    }

    complete_and_free_part_requests();

    if (0 == failures) {
        printf("PASS: part_persist_thread_init\n");
    }

    MPI_Finalize();
    return (0 == failures) ? EXIT_SUCCESS : EXIT_FAILURE;
}

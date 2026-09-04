/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Stress communication-establishment patterns after MPI_Init.
 *
 * Lazy endpoint construction is sensitive to who talks first: a ring
 * wires only neighbors, a one-to-many (bcast) wires from the root, a
 * many-to-one (reduce) wires into the root, and a many-to-many
 * (alltoall) wires the complete graph.  Each pattern is checked twice:
 * with the MPI collective, whose implementation may use a tree or other
 * overlay, and with a blunt point-to-point emulation that posts every
 * logical edge as a non-blocking send or receive before waiting, so
 * wire-up is concurrent rather than serialized.
 *
 * The rma pattern is different in kind: a window has to wire its peers
 * inside its own constructor, and it has to make one decision -- which
 * BTL carries the window -- identically on every rank, because that
 * choice sizes the state each rank exposes.  A rank that could not
 * enumerate a peer when the choice was made would drop a BTL its peers
 * kept, and nothing downstream detects it, so this runs the window
 * before anything else has wired anything.
 *
 * The ring and rma patterns have no collective counterpart; they are
 * always point-to-point.
 *
 * usage: wireup_stress [pattern] [flavor]
 *   pattern: ring | bcast | reduce | alltoall | rma | all  (default: all)
 *   flavor:  p2p   | coll   | all                          (default: all)
 *
 * Launch each pattern in its own mpirun if the goal is to observe
 * first-use wire-up: endpoints persist for the life of the job, so a
 * combined run only stresses establishment on the first pattern.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAG_RING     201
#define TAG_BCAST    202
#define TAG_REDUCE   203
#define TAG_ALLTOALL 204

#define BCAST_VALUE  0xC0FFEE
#define RING_LAPS    3

enum {
    PATTERN_RING = 0,
    PATTERN_BCAST,
    PATTERN_REDUCE,
    PATTERN_ALLTOALL,
    PATTERN_RMA,
    PATTERN_ALL
};

enum {
    FLAVOR_P2P = 0,
    FLAVOR_COLL,
    FLAVOR_ALL
};

static void fail(int rank, const char *msg)
{
    fprintf(stderr, "ERROR: rank %d: %s\n", rank, msg);
    MPI_Abort(MPI_COMM_WORLD, 1);
}

static void *xmalloc(int rank, size_t bytes)
{
    void *p = malloc(bytes);
    if (NULL == p) {
        fail(rank, "out of memory");
    }
    return p;
}

static int parse_pattern(const char *s)
{
    if (0 == strcmp(s, "ring")) {
        return PATTERN_RING;
    }
    if (0 == strcmp(s, "bcast")) {
        return PATTERN_BCAST;
    }
    if (0 == strcmp(s, "reduce")) {
        return PATTERN_REDUCE;
    }
    if (0 == strcmp(s, "alltoall")) {
        return PATTERN_ALLTOALL;
    }
    if (0 == strcmp(s, "rma")) {
        return PATTERN_RMA;
    }
    if (0 == strcmp(s, "all")) {
        return PATTERN_ALL;
    }
    return -1;
}

static int parse_flavor(const char *s)
{
    if (0 == strcmp(s, "p2p")) {
        return FLAVOR_P2P;
    }
    if (0 == strcmp(s, "coll")) {
        return FLAVOR_COLL;
    }
    if (0 == strcmp(s, "all")) {
        return FLAVOR_ALL;
    }
    return -1;
}

static int want_pattern(int selected, int pattern)
{
    return (PATTERN_ALL == selected) || (selected == pattern);
}

static int want_p2p(int flavor)
{
    return (FLAVOR_P2P == flavor) || (FLAVOR_ALL == flavor);
}

static int want_coll(int flavor)
{
    return (FLAVOR_COLL == flavor) || (FLAVOR_ALL == flavor);
}

/* Neighbor-only ring. Every rank posts Irecv(prev) and Isend(next)
   before waiting, so both neighbor endpoints are constructed at once. */
static void do_ring_p2p(int rank, int size)
{
    int next = (rank + 1) % size;
    int prev = (rank + size - 1) % size;
    int outgoing, incoming;
    MPI_Request reqs[2];

    for (int lap = 0; lap < RING_LAPS; ++lap) {
        outgoing = rank * 100 + lap;
        incoming = -1;
        MPI_Irecv(&incoming, 1, MPI_INT, prev, TAG_RING, MPI_COMM_WORLD, &reqs[0]);
        MPI_Isend(&outgoing, 1, MPI_INT, next, TAG_RING, MPI_COMM_WORLD, &reqs[1]);
        MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
        if (prev * 100 + lap != incoming) {
            fail(rank, "ring p2p: received the wrong value");
        }
    }
}

/* One-to-many: root posts a send to every other rank. */
static void do_bcast_p2p(int rank, int size)
{
    int value = (0 == rank) ? BCAST_VALUE : -1;

    if (0 == rank) {
        MPI_Request *reqs = xmalloc(rank, (size_t) (size - 1) * sizeof(MPI_Request));
        for (int dest = 1; dest < size; ++dest) {
            MPI_Isend(&value, 1, MPI_INT, dest, TAG_BCAST, MPI_COMM_WORLD, &reqs[dest - 1]);
        }
        MPI_Waitall(size - 1, reqs, MPI_STATUSES_IGNORE);
        free(reqs);
    } else {
        MPI_Request req;
        MPI_Irecv(&value, 1, MPI_INT, 0, TAG_BCAST, MPI_COMM_WORLD, &req);
        MPI_Wait(&req, MPI_STATUS_IGNORE);
    }

    if (BCAST_VALUE != value) {
        fail(rank, "bcast p2p: received the wrong value");
    }
}

static void do_bcast_coll(int rank, int size)
{
    int value = (0 == rank) ? BCAST_VALUE : -1;

    MPI_Bcast(&value, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (BCAST_VALUE != value) {
        fail(rank, "bcast coll: received the wrong value");
    }
    (void) size;
}

/* Many-to-one: every non-root rank sends to 0; 0 posts a receive from
   each of them. */
static void do_reduce_p2p(int rank, int size)
{
    int contrib = rank + 1;
    int expected = size * (size + 1) / 2;

    if (0 == rank) {
        int sum = contrib;
        int *vals = xmalloc(rank, (size_t) (size - 1) * sizeof(int));
        MPI_Request *reqs = xmalloc(rank, (size_t) (size - 1) * sizeof(MPI_Request));
        for (int src = 1; src < size; ++src) {
            MPI_Irecv(&vals[src - 1], 1, MPI_INT, src, TAG_REDUCE, MPI_COMM_WORLD,
                      &reqs[src - 1]);
        }
        MPI_Waitall(size - 1, reqs, MPI_STATUSES_IGNORE);
        for (int i = 0; i < size - 1; ++i) {
            sum += vals[i];
        }
        free(vals);
        free(reqs);
        if (expected != sum) {
            fail(rank, "reduce p2p: sum mismatch");
        }
    } else {
        MPI_Request req;
        MPI_Isend(&contrib, 1, MPI_INT, 0, TAG_REDUCE, MPI_COMM_WORLD, &req);
        MPI_Wait(&req, MPI_STATUS_IGNORE);
    }
}

static void do_reduce_coll(int rank, int size)
{
    int contrib = rank + 1;
    int sum = 0;
    int expected = size * (size + 1) / 2;

    MPI_Reduce(&contrib, &sum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    if (0 == rank && expected != sum) {
        fail(rank, "reduce coll: sum mismatch");
    }
}

/* Many-to-many: every rank posts a receive from every other rank, then
   a send to every other rank. */
static void do_alltoall_p2p(int rank, int size)
{
    int npeers = size - 1;
    int *sendbuf = xmalloc(rank, (size_t) size * sizeof(int));
    int *recvbuf = xmalloc(rank, (size_t) size * sizeof(int));
    MPI_Request *reqs = xmalloc(rank, (size_t) (2 * npeers) * sizeof(MPI_Request));
    int r = 0;

    for (int i = 0; i < size; ++i) {
        sendbuf[i] = rank * 1000 + i;
        recvbuf[i] = -1;
    }
    recvbuf[rank] = sendbuf[rank];

    for (int src = 0; src < size; ++src) {
        if (src == rank) {
            continue;
        }
        MPI_Irecv(&recvbuf[src], 1, MPI_INT, src, TAG_ALLTOALL, MPI_COMM_WORLD, &reqs[r++]);
    }
    for (int dest = 0; dest < size; ++dest) {
        if (dest == rank) {
            continue;
        }
        MPI_Isend(&sendbuf[dest], 1, MPI_INT, dest, TAG_ALLTOALL, MPI_COMM_WORLD, &reqs[r++]);
    }
    MPI_Waitall(2 * npeers, reqs, MPI_STATUSES_IGNORE);

    for (int src = 0; src < size; ++src) {
        int expect = src * 1000 + rank;
        if (expect != recvbuf[src]) {
            fail(rank, "alltoall p2p: received the wrong value");
        }
    }

    free(sendbuf);
    free(recvbuf);
    free(reqs);
}

static void do_alltoall_coll(int rank, int size)
{
    int *sendbuf = xmalloc(rank, (size_t) size * sizeof(int));
    int *recvbuf = xmalloc(rank, (size_t) size * sizeof(int));

    for (int i = 0; i < size; ++i) {
        sendbuf[i] = rank * 1000 + i;
        recvbuf[i] = -1;
    }

    MPI_Alltoall(sendbuf, 1, MPI_INT, recvbuf, 1, MPI_INT, MPI_COMM_WORLD);

    for (int src = 0; src < size; ++src) {
        int expect = src * 1000 + rank;
        if (expect != recvbuf[src]) {
            fail(rank, "alltoall coll: received the wrong value");
        }
    }

    free(sendbuf);
    free(recvbuf);
}

/* One-sided: the window is created before anything else has wired a
   peer, so its constructor is what resolves them -- and the BTL it picks
   for the window has to be the same on every rank.  Then a put and an
   accumulate to every rank, self included, and a get back from every
   rank, so both the data and the atomic path are used on every edge. */
static void do_rma(int rank, int size)
{
    int expected_sum = size * (size + 1) / 2;
    int contrib = rank + 1;
    int *base = NULL;
    int *sendvals, *fetched;
    MPI_Win win;

    MPI_Win_allocate((MPI_Aint) (size + 1) * sizeof(int), sizeof(int), MPI_INFO_NULL,
                     MPI_COMM_WORLD, &base, &win);

    for (int i = 0; i < size; ++i) {
        base[i] = -1;
    }
    base[size] = 0;

    /* origin buffers, so they have to outlive the epoch */
    sendvals = xmalloc(rank, (size_t) size * sizeof(int));
    fetched = xmalloc(rank, (size_t) size * sizeof(int));
    for (int dest = 0; dest < size; ++dest) {
        sendvals[dest] = rank * 1000 + dest;
        fetched[dest] = -1;
    }

    MPI_Win_fence(0, win);

    /* my own slot on every rank, so no two origins write the same word,
       and every rank's accumulator, where they all do */
    for (int dest = 0; dest < size; ++dest) {
        MPI_Put(&sendvals[dest], 1, MPI_INT, dest, rank, 1, MPI_INT, win);
        MPI_Accumulate(&contrib, 1, MPI_INT, dest, size, 1, MPI_INT, MPI_SUM, win);
    }

    MPI_Win_fence(0, win);

    for (int src = 0; src < size; ++src) {
        if (src * 1000 + rank != base[src]) {
            fail(rank, "rma: put landed with the wrong value");
        }
    }
    if (expected_sum != base[size]) {
        fail(rank, "rma: accumulate did not sum to the expected value");
    }

    /* read back what I wrote, from every rank */
    for (int target = 0; target < size; ++target) {
        MPI_Get(&fetched[target], 1, MPI_INT, target, rank, 1, MPI_INT, win);
    }

    MPI_Win_fence(0, win);

    for (int target = 0; target < size; ++target) {
        if (rank * 1000 + target != fetched[target]) {
            fail(rank, "rma: get returned the wrong value");
        }
    }

    free(sendvals);
    free(fetched);
    MPI_Win_free(&win);
}

static void usage(void)
{
    fprintf(stderr,
            "usage: wireup_stress [pattern] [flavor]\n"
            "  pattern: ring | bcast | reduce | alltoall | rma | all  (default: all)\n"
            "  flavor:  p2p  | coll  | all                            (default: all)\n"
            "  ring and rma are point-to-point only; coll is skipped for them.\n");
}

int main(int argc, char *argv[])
{
    int rank, size;
    int pattern = PATTERN_ALL;
    int flavor = FLAVOR_ALL;
    int ran = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 2) {
        if (0 == rank) {
            fprintf(stderr, "ERROR: this test requires at least 2 ranks\n");
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    if (argc > 3) {
        if (0 == rank) {
            usage();
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
    if (argc >= 2) {
        pattern = parse_pattern(argv[1]);
        if (pattern < 0) {
            if (0 == rank) {
                usage();
            }
            MPI_Abort(MPI_COMM_WORLD, 1);
            return 1;
        }
    }
    if (argc >= 3) {
        flavor = parse_flavor(argv[2]);
        if (flavor < 0) {
            if (0 == rank) {
                usage();
            }
            MPI_Abort(MPI_COMM_WORLD, 1);
            return 1;
        }
    }

    if ((PATTERN_RING == pattern || PATTERN_RMA == pattern) && FLAVOR_COLL == flavor) {
        if (0 == rank) {
            fprintf(stderr, "ERROR: %s has no collective flavor; use p2p\n",
                    (PATTERN_RING == pattern) ? "ring" : "rma");
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    /* First, and before any barrier: what the window has to cope with is
       peers that nothing has wired yet. */
    if (want_pattern(pattern, PATTERN_RMA) && want_p2p(flavor)) {
        do_rma(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }

    if (want_pattern(pattern, PATTERN_RING) && want_p2p(flavor)) {
        do_ring_p2p(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }

    if (want_pattern(pattern, PATTERN_BCAST) && want_p2p(flavor)) {
        do_bcast_p2p(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }
    if (want_pattern(pattern, PATTERN_BCAST) && want_coll(flavor)) {
        do_bcast_coll(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }

    if (want_pattern(pattern, PATTERN_REDUCE) && want_p2p(flavor)) {
        do_reduce_p2p(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }
    if (want_pattern(pattern, PATTERN_REDUCE) && want_coll(flavor)) {
        do_reduce_coll(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }

    if (want_pattern(pattern, PATTERN_ALLTOALL) && want_p2p(flavor)) {
        do_alltoall_p2p(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }
    if (want_pattern(pattern, PATTERN_ALLTOALL) && want_coll(flavor)) {
        do_alltoall_coll(rank, size);
        MPI_Barrier(MPI_COMM_WORLD);
        ran++;
    }

    if (0 == ran) {
        fail(rank, "no pattern/flavor combination ran");
    }

    if (0 == rank) {
        printf("wireup stress: PASSED (%d ranks, %d checks)\n", size, ran);
        fflush(stdout);
    }

    MPI_Finalize();
    return 0;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * First contact with a peer long after MPI_Init, in a job where nothing
 * in the background will ever make that peer reachable.
 *
 * Run with pmix_base_collect_data=0 there is no data-collecting fence at
 * all: a peer's connection info becomes local only because some process
 * asked for that one peer, and only that process's own progress carries
 * the fetch to completion.  A wire-up cannot ride on anything else, and
 * an operation parked waiting for one is woken by nobody -- so a parked
 * operation that is not re-driven from a progress callback leaves this
 * test hung rather than failed, which is why it must be launched with
 * --timeout.
 *
 * Ranks 0 and 1 trade messages, which wires that pair and nobody else.
 * The remaining ranks meanwhile only probe: a wild MPI_Iprobe turns the
 * progress engine without naming a peer, so they produce the ticks that
 * would drain parked work while still having wired nothing.  Only then
 * do they speak for the first time.
 *
 *   - Rank 2 sends a small message, which rank 0 takes with
 *     MPI_ANY_SOURCE.  A wild receive constructs nothing, so that
 *     fragment arrives from a peer the receiver has never heard from and
 *     holds no endpoint for.
 *
 *   - Rank 3 sends one large enough to be a rendezvous.  Rank 0 then has
 *     to acknowledge it, which is a control message towards a peer it
 *     cannot yet reach: it can only be queued, and the retry is what has
 *     to build the endpoint.
 *
 * Both directions of the on-demand fetch are covered without racing the
 * peers' own startup.  A rank commits what it publishes before it sends
 * anything, so a peer it sends to can always read it; had rank 0 done
 * the late sending instead, it could have asked about a rank that had
 * not published yet, which is a different question from this one.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define TAG_PINGPONG 401
#define TAG_EAGER    402
#define TAG_RNDV     403

/* Enough traffic that the pair is long since wired, and enough ticks
   elsewhere that a drain had every chance to run, without either taking
   measurable time on an oversubscribed CI machine. */
#define PINGPONG_LAPS 200
#define LATE_PROBES   20000

#define EAGER_VALUE   0x5A5A

/* 1 MiB: past the eager limit of every BTL, so this is a rendezvous and
   the receiver has to answer the sender to get the data. */
#define RNDV_INTS     (256 * 1024)

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

static int rndv_value(int i)
{
    return i * 7 + 3;
}

/* Ranks 0 and 1 only: wires that pair, and nothing else. */
static void pingpong(int rank)
{
    int peer = (0 == rank) ? 1 : 0;

    for (int lap = 0; lap < PINGPONG_LAPS; ++lap) {
        int outgoing = rank * 1000 + lap;
        int incoming = -1;

        if (0 == rank) {
            MPI_Send(&outgoing, 1, MPI_INT, peer, TAG_PINGPONG, MPI_COMM_WORLD);
            MPI_Recv(&incoming, 1, MPI_INT, peer, TAG_PINGPONG, MPI_COMM_WORLD,
                     MPI_STATUS_IGNORE);
        } else {
            MPI_Recv(&incoming, 1, MPI_INT, peer, TAG_PINGPONG, MPI_COMM_WORLD,
                     MPI_STATUS_IGNORE);
            MPI_Send(&outgoing, 1, MPI_INT, peer, TAG_PINGPONG, MPI_COMM_WORLD);
        }

        if (peer * 1000 + lap != incoming) {
            fail(rank, "ping-pong: received the wrong value");
        }
    }
}

/* Progress without a peer.  A wild probe drives the engine that drains
   parked work, but names nobody, so this rank has still wired nothing
   when it sends below. */
static void turn_progress(void)
{
    for (int i = 0; i < LATE_PROBES; ++i) {
        int flag = 0;
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag,
                   MPI_STATUS_IGNORE);
    }
}

static void receive_late(int rank, int size)
{
    MPI_Status status;
    int value = -1;

    /* Wild, so nothing is built for the sender ahead of its fragment. */
    MPI_Recv(&value, 1, MPI_INT, MPI_ANY_SOURCE, TAG_EAGER, MPI_COMM_WORLD, &status);
    if (EAGER_VALUE != value) {
        fail(rank, "eager first contact: received the wrong value");
    }
    if (2 != status.MPI_SOURCE) {
        fail(rank, "eager first contact: received from the wrong rank");
    }

    if (size > 3) {
        int *buf = xmalloc(rank, (size_t) RNDV_INTS * sizeof(int));

        MPI_Recv(buf, RNDV_INTS, MPI_INT, 3, TAG_RNDV, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
        for (int i = 0; i < RNDV_INTS; ++i) {
            if (rndv_value(i) != buf[i]) {
                fail(rank, "rendezvous first contact: received the wrong data");
                break;
            }
        }
        free(buf);
    }
}

static void send_rndv(int rank)
{
    int *buf = xmalloc(rank, (size_t) RNDV_INTS * sizeof(int));

    for (int i = 0; i < RNDV_INTS; ++i) {
        buf[i] = rndv_value(i);
    }
    MPI_Send(buf, RNDV_INTS, MPI_INT, 0, TAG_RNDV, MPI_COMM_WORLD);
    free(buf);
}

int main(int argc, char *argv[])
{
    int rank, size;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* Two ranks cannot hold a peer in reserve: the pair that generates
       the traffic is the pair that would have to make first contact. */
    if (size < 3) {
        if (0 == rank) {
            fprintf(stderr, "ERROR: this test requires at least 3 ranks\n");
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    if (0 == rank || 1 == rank) {
        pingpong(rank);
    } else {
        turn_progress();
    }

    if (0 == rank) {
        receive_late(rank, size);
    } else if (2 == rank) {
        int value = EAGER_VALUE;
        MPI_Send(&value, 1, MPI_INT, 0, TAG_EAGER, MPI_COMM_WORLD);
    } else if (3 == rank) {
        send_rndv(rank);
    }

    /* Everything wired at last, so this also says the ranks nobody spoke
       to are still reachable. */
    MPI_Barrier(MPI_COMM_WORLD);

    if (0 == rank) {
        printf("late first contact: PASSED (%d ranks)\n", size);
        fflush(stdout);
    }

    MPI_Finalize();
    return 0;
}

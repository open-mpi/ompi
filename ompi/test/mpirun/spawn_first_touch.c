/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * First contact across a namespace boundary.
 *
 * Every other test here wires peers of one job, whose connection info
 * the launcher published together.  A spawned child is the one case
 * where the peer belongs to a job that did not exist when we started,
 * so nothing could have been collected for it in advance and every
 * parent-child pair has to resolve the other on demand.
 *
 * Merging the intercommunicator is what makes that exhaustive: on the
 * merged communicator each rank then talks to every other, so each
 * cross-namespace pair makes first contact, not just the roots.
 *
 * Repeated, because a cycle can leave something behind that only the
 * next one trips over: nothing reclaims a disconnected peer's proc or
 * its endpoints, so cycle N+1 wires up with all of cycle N still in
 * memory.  That is how the tcp duel abort was found.
 *
 * Acts as its own child.  Run it with pmix_base_collect_data=0, and
 * with --timeout, since an unresolved peer hangs rather than fails.
 */

#include <mpi.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define CHILDREN 2
#define TAG_PAIR 801

/* Spawning is the slow part of this test, so: enough cycles that one
   can inherit the previous one's leavings, and no more.  argv[1] raises
   it, and does not reach the children -- they are spawned argv-less. */
#define CYCLES 3

static int payload(int rank)
{
    return rank * 31 + 7;
}

/* Every pair, by walking the distances: rank r talks to r+d while r-d
   talks to r, so one Sendrecv per distance covers the whole matrix. */
static void touch_everyone(MPI_Comm comm, const char *who)
{
    int rank, size;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);

    for (int distance = 1; distance < size; ++distance) {
        int dst = (rank + distance) % size;
        int src = (rank - distance + size) % size;
        int outgoing = payload(rank);
        int incoming = -1;

        MPI_Sendrecv(&outgoing, 1, MPI_INT, dst, TAG_PAIR, &incoming, 1, MPI_INT, src, TAG_PAIR,
                     comm, MPI_STATUS_IGNORE);

        if (payload(src) != incoming) {
            fprintf(stderr, "ERROR: %s rank %d: expected %d from rank %d, got %d\n", who, rank,
                    payload(src), src, incoming);
            MPI_Abort(comm, 1);
        }
    }
}

static void exchange_and_disconnect(MPI_Comm remote, bool child)
{
    const char *who = child ? "child" : "parent";
    MPI_Comm merged = MPI_COMM_NULL;

    /* The roots alone would leave most pairs untouched. */
    MPI_Intercomm_merge(remote, child, &merged);
    touch_everyone(merged, who);

    MPI_Comm_disconnect(&merged);
    MPI_Comm_disconnect(&remote);
}

int main(int argc, char *argv[])
{
    MPI_Comm parent = MPI_COMM_NULL;
    int rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_get_parent(&parent);

    if (MPI_COMM_NULL != parent) {
        exchange_and_disconnect(parent, true);
        MPI_Finalize();
        return 0;
    }

    int cycles = (argc > 1) ? atoi(argv[1]) : CYCLES;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    for (int cycle = 0; cycle < cycles; ++cycle) {
        MPI_Comm children = MPI_COMM_NULL;
        int errcodes[CHILDREN];

        /* Returning, not fatal: a launcher that will not spawn here is
           a reason to skip, not to fail. */
        MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
        int rc = MPI_Comm_spawn(argv[0], MPI_ARGV_NULL, CHILDREN, MPI_INFO_NULL, 0, MPI_COMM_WORLD,
                                &children, errcodes);
        MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL);

        if (MPI_SUCCESS != rc) {
            if (0 == rank) {
                printf("SKIP: this launcher will not spawn %d children here\n", CHILDREN);
                fflush(stdout);
            }
            MPI_Finalize();
            return 0;
        }

        exchange_and_disconnect(children, false);

        /* Says the parents still reach each other after the children
           go, and that the next cycle starts from a sane world. */
        MPI_Barrier(MPI_COMM_WORLD);
    }

    if (0 == rank) {
        printf("spawn first touch: PASSED (%d cycles of %d children)\n", cycles, CHILDREN);
        fflush(stdout);
    }

    MPI_Finalize();
    return 0;
}

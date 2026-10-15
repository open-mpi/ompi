/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Two overlapping sessions, so that the ranks reach the second one
 * holding different numbers of wired peers.
 *
 * Rank 0 and rank 1 talk over a communicator built from a two-member
 * group, which leaves each of them with an endpoint for the other and
 * rank 2 with none.  All three then build a communicator over the whole
 * group and talk across it.  Arriving there, rank 0 and rank 1 have one
 * peer left to wire and rank 2 has two, so whatever add_procs() is
 * reached is reached with a different set of procs on different ranks --
 * the asymmetry a BTL that reads the proc array as one object, rather
 * than as a list of independent peers, cannot survive.
 *
 * The run modes matter more than the ranks here.  Lazy wire-up resolves
 * one peer per call and never assembles a set; the eager whole-job path
 * taken for MCA_BTL_FLAGS_REQUIRE_SYNC_INIT or
 * MCA_BTL_FLAGS_SINGLE_ADD_PROCS assembles one in the first session and
 * finds nothing left to do in the second.  Both are worth holding still:
 * it is the step between them -- a set that is neither one peer nor the
 * whole job -- that has no symmetric reading, and a change that starts
 * producing one here should have to say so.
 *
 * A failure is as likely to hang as to report an unreachable peer, so
 * launch with --timeout.
 */

#include <mpi.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* Ranks 0 and 1: the pair that wires up early. */
#define PARTIAL_MEMBERS 2

#define TAG_PARTIAL 811
#define TAG_FULL    821

/* Distinct per ordered pair and per phase, so a payload that arrives
   from the wrong peer or the wrong phase does not read as correct. */
static int payload(int from, int to, int phase)
{
    return (from + 1) * 10000 + (to + 1) * 100 + phase;
}

static bool failed(const char *what, int rc)
{
    char message[MPI_MAX_ERROR_STRING] = "";
    int length = 0;

    if (MPI_SUCCESS == rc) {
        return false;
    }

    MPI_Error_string(rc, message, &length);
    fprintf(stderr, "ERROR: %s failed: %s\n", what, message);
    return true;
}

/* Sendrecv with every other rank of comm, nearest partner outwards. */
static bool exchange_all(MPI_Comm comm, int rank, int size, int tag, int phase)
{
    for (int distance = 1; distance < size; ++distance) {
        int dst = (rank + distance) % size;
        int src = (rank - distance + size) % size;
        int outgoing = payload(rank, dst, phase);
        int incoming = -1;
        int rc = MPI_Sendrecv(&outgoing, 1, MPI_INT, dst, tag, &incoming, 1, MPI_INT, src, tag,
                              comm, MPI_STATUS_IGNORE);

        if (failed("MPI_Sendrecv", rc)) {
            return true;
        }
        if (payload(src, rank, phase) != incoming) {
            fprintf(stderr, "ERROR: rank %d phase %d: expected %d from rank %d, got %d\n", rank,
                    phase, payload(src, rank, phase), src, incoming);
            return true;
        }
    }
    return false;
}

int main(void)
{
    MPI_Session session = MPI_SESSION_NULL;
    MPI_Group world_group = MPI_GROUP_NULL;
    MPI_Group partial_group = MPI_GROUP_NULL;
    MPI_Comm full_comm = MPI_COMM_NULL;
    const int members[PARTIAL_MEMBERS] = {0, 1};
    int world_size = -1;
    int world_rank = -1;
    int rc;

    rc = MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
    if (failed("MPI_Session_init", rc)) {
        return 1;
    }

    rc = MPI_Group_from_session_pset(session, "mpi://WORLD", &world_group);
    if (failed("MPI_Group_from_session_pset", rc)) {
        return 1;
    }

    MPI_Group_size(world_group, &world_size);
    MPI_Group_rank(world_group, &world_rank);
    if (world_size < PARTIAL_MEMBERS + 1) {
        if (0 == world_rank) {
            fprintf(stderr, "ERROR: this test requires at least %d ranks\n", PARTIAL_MEMBERS + 1);
        }
        return 1;
    }

    rc = MPI_Group_incl(world_group, PARTIAL_MEMBERS, members, &partial_group);
    if (failed("MPI_Group_incl", rc)) {
        return 1;
    }

    /* Only the members may call this, so rank 2 goes straight on to the
       whole-group communicator below and waits for the pair there. */
    if (world_rank < PARTIAL_MEMBERS) {
        MPI_Comm partial_comm = MPI_COMM_NULL;

        rc = MPI_Comm_create_from_group(partial_group, "ompi-session-partial-batch-pair",
                                        MPI_INFO_NULL, MPI_ERRORS_RETURN, &partial_comm);
        if (failed("MPI_Comm_create_from_group (pair)", rc)) {
            return 1;
        }
        if (exchange_all(partial_comm, world_rank, PARTIAL_MEMBERS, TAG_PARTIAL, 1)) {
            return 1;
        }

        /* The endpoints outlive it: they belong to the session, not to
           the communicator that first needed them. */
        MPI_Comm_free(&partial_comm);
    }
    MPI_Group_free(&partial_group);

    rc = MPI_Comm_create_from_group(world_group, "ompi-session-partial-batch-all", MPI_INFO_NULL,
                                    MPI_ERRORS_RETURN, &full_comm);
    if (failed("MPI_Comm_create_from_group (all)", rc)) {
        return 1;
    }
    MPI_Group_free(&world_group);

    if (exchange_all(full_comm, world_rank, world_size, TAG_FULL, 2)) {
        return 1;
    }

    /* Again, now that every peer is warm: a peer wired from a partial
       set must keep working, not just answer once. */
    if (exchange_all(full_comm, world_rank, world_size, TAG_FULL, 3)) {
        return 1;
    }

    MPI_Comm_free(&full_comm);
    MPI_Session_finalize(&session);

    if (0 == world_rank) {
        printf("session partial batch: PASSED (%d ranks, %d early)\n", world_size,
               PARTIAL_MEMBERS);
        fflush(stdout);
    }

    return 0;
}

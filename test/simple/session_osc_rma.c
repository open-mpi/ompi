/* SPDX-License-Identifier: BSD-3-Clause-Open-MPI */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * A program that only uses sessions never initializes MPI_COMM_WORLD.
 * Create a communicator from a session, a window on it, and put the rank
 * of every process into every other one under a passive target epoch
 * (MPI_Win_lock_all), then check what arrived.
 *
 * osc/ucx keeps the endpoint to each peer for all the windows.  It used to
 * find it by the rank of the peer in MPI_COMM_WORLD, which reads 0 in every
 * process of a sessions-only program.  Every target then shared the
 * endpoint of the first one, and the first lock on any other target failed
 * with a remote access error.
 *
 * usage: session_osc_rma
 *
 * Run with at least two processes.  Only osc/ucx has the problem, so
 * select it with --mca osc ucx; the test passes with the other components.
 */

#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

int main(int argc, char *argv[])
{
    MPI_Session session;
    MPI_Group group;
    MPI_Comm comm;
    MPI_Win win;
    long *base, value;
    int rank, size, peer, errors = 0;

    (void) argc;
    (void) argv;

    MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
    MPI_Group_from_session_pset(session, "mpi://WORLD", &group);
    MPI_Comm_create_from_group(group, "session_osc_rma", MPI_INFO_NULL, MPI_ERRORS_RETURN,
                               &comm);
    MPI_Group_free(&group);
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);

    MPI_Win_allocate(size * sizeof(long), sizeof(long), MPI_INFO_NULL, comm, &base, &win);
    for (peer = 0; peer < size; peer++) {
        base[peer] = -1;
    }
    MPI_Barrier(comm);

    MPI_Win_lock_all(0, win);
    for (peer = 0; peer < size; peer++) {
        value = rank;
        MPI_Put(&value, 1, MPI_LONG, peer, rank, 1, MPI_LONG, win);
    }
    MPI_Win_flush_all(win);
    MPI_Win_unlock_all(win);
    MPI_Barrier(comm);

    for (peer = 0; peer < size; peer++) {
        if (base[peer] != peer) {
            fprintf(stderr, "rank %d: slot %d holds %ld, expected %d\n", rank, peer, base[peer],
                    peer);
            errors++;
        }
    }

    MPI_Win_free(&win);
    MPI_Comm_disconnect(&comm);
    MPI_Session_finalize(&session);

    if (0 == rank && 0 == errors) {
        printf("session_osc_rma: ok\n");
    }
    return errors ? EXIT_FAILURE : EXIT_SUCCESS;
}

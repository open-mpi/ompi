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
 * Processes spawned by MPI_Comm_spawn belong to another job, whose ranks
 * start again at 0.  Merge the parents and the children, create a window
 * on the merged communicator, and put the rank of every process into
 * every other one under a passive target epoch (MPI_Win_lock_all), then
 * check what arrived.
 *
 * osc/ucx keeps the endpoint to each peer for all the windows.  It used to
 * find it by the rank of the peer in its own job, so a parent and a child
 * of the same rank shared one endpoint, and the first lock on a target
 * that was reached through the wrong one failed with a remote access
 * error.
 *
 * usage: spawn_osc_rma
 *
 * The parents spawn as many children as they are.  Only osc/ucx has the
 * problem, so select it with --mca osc ucx; the test passes with the other
 * components.
 */

#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

int main(int argc, char *argv[])
{
    MPI_Comm parent_inter, inter, merged;
    MPI_Win win;
    long *base, value;
    int rank, size, peer, nparents, is_child, errors = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_get_parent(&parent_inter);
    if (MPI_COMM_NULL == parent_inter) {
        MPI_Comm_size(MPI_COMM_WORLD, &nparents);
        MPI_Comm_spawn(argv[0], MPI_ARGV_NULL, nparents, MPI_INFO_NULL, 0, MPI_COMM_WORLD,
                       &inter, MPI_ERRCODES_IGNORE);
        is_child = 0;
    } else {
        inter = parent_inter;
        is_child = 1;
    }
    MPI_Intercomm_merge(inter, is_child, &merged);
    MPI_Comm_rank(merged, &rank);
    MPI_Comm_size(merged, &size);

    MPI_Win_allocate(size * sizeof(long), sizeof(long), MPI_INFO_NULL, merged, &base, &win);
    for (peer = 0; peer < size; peer++) {
        base[peer] = -1;
    }
    MPI_Barrier(merged);

    MPI_Win_lock_all(0, win);
    for (peer = 0; peer < size; peer++) {
        value = rank;
        MPI_Put(&value, 1, MPI_LONG, peer, rank, 1, MPI_LONG, win);
    }
    MPI_Win_flush_all(win);
    MPI_Win_unlock_all(win);
    MPI_Barrier(merged);

    for (peer = 0; peer < size; peer++) {
        if (base[peer] != peer) {
            fprintf(stderr, "rank %d: slot %d holds %ld, expected %d\n", rank, peer, base[peer],
                    peer);
            errors++;
        }
    }

    MPI_Win_free(&win);
    MPI_Comm_free(&merged);
    MPI_Comm_disconnect(&inter);
    MPI_Finalize();

    if (0 == rank && 0 == errors) {
        printf("spawn_osc_rma: ok\n");
    }
    return errors ? EXIT_FAILURE : EXIT_SUCCESS;
}

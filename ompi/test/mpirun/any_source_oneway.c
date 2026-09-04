/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Rank 0 posts MPI_ANY_SOURCE and never sends. Rank 1 sends one
 * message. With lazy SM endpoints that is one-way traffic: A attaches
 * B in order to write B's FIFO, and B must attach A on the incoming
 * item so it can translate the fragment and write the completion
 * back. A wild recv must not add_proc every communicator rank.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int rank, size, val = 0;

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

    if (0 == rank) {
        MPI_Status status;
        MPI_Recv(&val, 1, MPI_INT, MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, &status);
        if (42 != val || 1 != status.MPI_SOURCE) {
            fprintf(stderr, "ERROR: rank 0 received %d from %d\n", val, status.MPI_SOURCE);
            MPI_Abort(MPI_COMM_WORLD, 1);
            return 1;
        }
        printf("any-source one-way: PASSED (%d ranks)\n", size);
    } else if (1 == rank) {
        val = 42;
        MPI_Send(&val, 1, MPI_INT, 0, 1, MPI_COMM_WORLD);
    }

    MPI_Finalize();
    return 0;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier:  MIT
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Minimal send/recv workload for validating HWPC_CXI output ordering.
 *
 * Intentionally prints very little to stdout: one status line on rank 0.
 */

#include <mpi.h>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    int rank = 0;
    int size = 0;
    int loops = 100;
    const int tag = 42;
    int32_t payload = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (argc > 1) {
        int arg_loops = atoi(argv[1]);
        if (arg_loops > 0) {
            loops = arg_loops;
        }
    }

    if (size < 2) {
        if (0 == rank) {
            printf("hwpc_cxi_sendrecv_test requires at least 2 MPI ranks\n");
            fflush(stdout);
        }
        MPI_Finalize();
        return EXIT_FAILURE;
    }

    if (rank < 2) {
        for (int i = 0; i < loops; ++i) {
            if (0 == rank) {
                payload = i;
                MPI_Send(&payload, 1, MPI_INT32_T, 1, tag, MPI_COMM_WORLD);
                MPI_Recv(&payload, 1, MPI_INT32_T, 1, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            } else {
                MPI_Recv(&payload, 1, MPI_INT32_T, 0, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                payload += 1;
                MPI_Send(&payload, 1, MPI_INT32_T, 0, tag, MPI_COMM_WORLD);
            }
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (0 == rank) {
        printf("hwpc_cxi_sendrecv_test complete\n");
        fflush(stdout);
    }

    MPI_Finalize();
    return EXIT_SUCCESS;
}

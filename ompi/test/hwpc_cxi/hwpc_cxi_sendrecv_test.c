/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
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
#include <sys/time.h>

static double wall_time(void)
{
    struct timeval tv;

    gettimeofday(&tv, NULL);
    return (double) tv.tv_sec + (double) tv.tv_usec / 1000000.0;
}

int main(int argc, char **argv)
{
    int rank = 0;
    int size = 0;
    int loops = 5000;
    const int tag = 42;
    const int work_iters = 32;
    const int payload_count = 4096;
    int32_t *payload = NULL;
    uint64_t checksum = 0;
    double elapsed_with_mpi_init_finalize = 0.0;
    double elapsed_without_mpi_init_finalize = 0.0;
    double start_with_mpi_init_finalize = wall_time();
    double start_without_mpi_init_finalize = 0.0;

    MPI_Init(&argc, &argv);
    start_without_mpi_init_finalize = MPI_Wtime();
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
            printf("hwpc_cxi_sendrecv_test requires at least 2 MPI ranks, skipping test\n");
            fflush(stdout);
        }
        MPI_Finalize();
        return 77; /* 77 indicates the test was skipped. */
    }

    payload = (int32_t *) malloc((size_t) payload_count * sizeof(int32_t));
    if (NULL == payload) {
        if (0 == rank) {
            fprintf(stderr, "hwpc_cxi_sendrecv_test failed to allocate payload buffer\n");
        }
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    if (rank < 2) {
        for (int i = 0; i < loops; ++i) {
            for (int j = 0; j < payload_count; ++j) {
                payload[j] = (int32_t) ((i + 1) * 131 + j * 17);
            }

            if (0 == rank) {
                for (int k = 0; k < work_iters; ++k) {
                    MPI_Send(payload, payload_count, MPI_INT32_T, 1, tag, MPI_COMM_WORLD);
                    MPI_Recv(payload, payload_count, MPI_INT32_T, 1, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                    for (int j = 0; j < payload_count; ++j) {
                        checksum += (uint64_t) payload[j] ^ (uint64_t) (j * (i + 1));
                        payload[j] = (int32_t) ((payload[j] + 13) ^ (checksum + (uint64_t) j));
                    }
                }
            } else {
                for (int k = 0; k < work_iters; ++k) {
                    MPI_Recv(payload, payload_count, MPI_INT32_T, 0, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                    for (int j = 0; j < payload_count; ++j) {
                        checksum += (uint64_t) payload[j] ^ (uint64_t) (j * (i + 1));
                        payload[j] = (int32_t) ((payload[j] + 19) ^ (checksum + (uint64_t) j));
                    }
                    MPI_Send(payload, payload_count, MPI_INT32_T, 0, tag, MPI_COMM_WORLD);
                }
            }
        }
    }

    free(payload);

    MPI_Barrier(MPI_COMM_WORLD);

    elapsed_without_mpi_init_finalize = MPI_Wtime() - start_without_mpi_init_finalize;
    MPI_Finalize();

    elapsed_with_mpi_init_finalize = wall_time() - start_with_mpi_init_finalize;
    if (0 == rank) {
        printf("hwpc_cxi_sendrecv_test complete elapsed_with_mpi_init_finalize=%.6f elapsed_without_mpi_init_finalize=%.6f\n",
               elapsed_with_mpi_init_finalize, elapsed_without_mpi_init_finalize);
        fflush(stdout);
    }
    return EXIT_SUCCESS;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/*
 * Large message truncation test.
 *
 * Rank 0 sends N bytes to rank 1, which posts a smaller receive of M < N
 * bytes. Large messages go through the single-copy / RDMA protocols of the
 * PML (e.g., sm with CMA, XPMEM or KNEM), which must never read or write
 * past the end of the posted receive buffer. For each (N, M) pair we check
 * that:
 *
 *   - both sides complete (the sender must not hang),
 *   - the receive completes with MPI_ERR_TRUNCATE,
 *   - the first M bytes of the receive buffer are the first M bytes sent,
 *   - the guard area right after the M receive bytes is left untouched.
 *
 * Every pair is run in two synchronized modes, so that both matching paths
 * of the PML are exercised deterministically:
 *
 *   - preposted: the receive is posted before the sender starts,
 *   - unexpected: the message has arrived (MPI_Probe) before the receive
 *     is posted.
 *
 * CAVEAT: this test provides only PARTIAL validation in CI. The paths it
 * was written for (the RGET protocol: get completion, partial get failure
 * and fallback, zero-capacity receives) are reached only when the pair of
 * processes selects a BTL with a GET path, such as sm with a single-copy
 * mechanism (CMA, XPMEM or KNEM) or a network BTL. Most CI machines have
 * none, so there the test falls back to the rendezvous / pipeline
 * protocols (e.g., over tcp). That still checks the protocol-independent
 * truncation semantics, but not the RGET code. To validate the RGET paths,
 * run it by hand on a node or fabric that provides a GET-capable BTL.
 *
 * Run with at least 2 processes on the same node (to exercise sm), e.g.:
 *
 *   mpirun -np 2 ./truncate_rdma
 *
 * and compare with, e.g., --mca smsc ^cma (or whatever single-copy
 * mechanism the node provides) to see the protocol-dependent behavior.
 */

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GUARD_SIZE 8192
#define GUARD_BYTE 0xA5
#define SYNC_TAG 30000

enum { MODE_PREPOSTED = 0, MODE_UNEXPECTED = 1, MODE_COUNT = 2 };
static const char *mode_name[MODE_COUNT] = {"preposted", "unexpected"};

static unsigned char pattern(size_t i)
{
    return (unsigned char) (i % 251);
}

static int run_case(int rank, size_t send_len, size_t recv_len, int tag, int mode)
{
    int rc, failures = 0;
    MPI_Request req;
    MPI_Status status;

    if (0 == rank) {
        unsigned char *sbuf = malloc(send_len);
        if (NULL == sbuf) {
            fprintf(stderr, "rank 0: out of memory\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        for (size_t i = 0; i < send_len; i++) {
            sbuf[i] = pattern(i);
        }
        if (MODE_PREPOSTED == mode) {
            /* do not send before the receive is posted */
            MPI_Recv(NULL, 0, MPI_BYTE, 1, SYNC_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
        rc = MPI_Isend(sbuf, (int) send_len, MPI_BYTE, 1, tag, MPI_COMM_WORLD, &req);
        if (MPI_SUCCESS == rc) {
            rc = MPI_Wait(&req, MPI_STATUS_IGNORE);
        }
        if (MPI_SUCCESS != rc) {
            fprintf(stderr, "rank 0: [%s] send %zu/%zu failed: %d\n", mode_name[mode], send_len,
                    recv_len, rc);
            failures++;
        }
        free(sbuf);
    } else if (1 == rank) {
        /* the receive buffer is followed by a guard area that must remain untouched */
        unsigned char *rbuf = malloc(recv_len + GUARD_SIZE);
        if (NULL == rbuf) {
            fprintf(stderr, "rank 1: out of memory\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        memset(rbuf, 0, recv_len);
        memset(rbuf + recv_len, GUARD_BYTE, GUARD_SIZE);

        if (MODE_UNEXPECTED == mode) {
            /* the message is queued as unexpected once the probe returns */
            MPI_Probe(0, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
        rc = MPI_Irecv(rbuf, (int) recv_len, MPI_BYTE, 0, tag, MPI_COMM_WORLD, &req);
        if (MODE_PREPOSTED == mode) {
            /* the receive is posted: release the sender */
            MPI_Send(NULL, 0, MPI_BYTE, 0, SYNC_TAG, MPI_COMM_WORLD);
        }
        if (MPI_SUCCESS == rc) {
            rc = MPI_Wait(&req, &status);
        }
        if (send_len > recv_len) {
            if (MPI_ERR_TRUNCATE != rc) {
                fprintf(stderr, "rank 1: [%s] recv %zu/%zu: expected MPI_ERR_TRUNCATE, got %d\n",
                        mode_name[mode], send_len, recv_len, rc);
                failures++;
            }
        } else if (MPI_SUCCESS != rc) {
            fprintf(stderr, "rank 1: [%s] recv %zu/%zu failed: %d\n", mode_name[mode], send_len,
                    recv_len, rc);
            failures++;
        }

        /* only the bytes that were actually sent and fit are meaningful */
        const size_t valid_len = send_len < recv_len ? send_len : recv_len;
        for (size_t i = 0; i < valid_len; i++) {
            if (rbuf[i] != pattern(i)) {
                fprintf(stderr, "rank 1: [%s] recv %zu/%zu: data mismatch at byte %zu\n",
                        mode_name[mode], send_len, recv_len, i);
                failures++;
                break;
            }
        }
        for (size_t i = 0; i < GUARD_SIZE; i++) {
            if (GUARD_BYTE != rbuf[recv_len + i]) {
                fprintf(stderr,
                        "rank 1: [%s] recv %zu/%zu: write past the receive buffer (byte +%zu)\n",
                        mode_name[mode], send_len, recv_len, i);
                failures++;
                break;
            }
        }
        free(rbuf);
    }

    return failures;
}

int main(int argc, char *argv[])
{
    static const size_t send_sizes[] = {64 * 1024, 1024 * 1024, 16 * 1024 * 1024};
    int rank, size, failures = 0, total, tag = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    if (size < 2) {
        if (0 == rank) {
            fprintf(stderr, "This test needs at least 2 processes\n");
        }
        MPI_Finalize();
        return 1;
    }

    for (size_t s = 0; s < sizeof(send_sizes) / sizeof(send_sizes[0]); s++) {
        const size_t n = send_sizes[s];
        const size_t recv_sizes[] = {0, 1, 4096, n / 2, n - 1, n, n + 1};

        for (size_t r = 0; r < sizeof(recv_sizes) / sizeof(recv_sizes[0]); r++) {
            for (int mode = 0; mode < MODE_COUNT; mode++) {
                failures += run_case(rank, n, recv_sizes[r], tag++, mode);
            }
        }
    }

    /* the communication state must be sane after all the truncated receives */
    for (int mode = 0; mode < MODE_COUNT; mode++) {
        failures += run_case(rank, 1024 * 1024, 1024 * 1024, tag++, mode);
    }

    MPI_Allreduce(&failures, &total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (0 == rank) {
        printf("truncate_rdma: %s (%d failure(s))\n", 0 == total ? "PASSED" : "FAILED", total);
    }

    MPI_Finalize();
    return 0 == total ? 0 : 1;
}

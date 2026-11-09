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
 * A communicator created with MPI_Comm_create_from_group has an extended
 * CID.  Until the receiver has told the sender its local index, a message
 * on it carries a CID header in front of the match header, and one that
 * reaches a process before that process has added the communicator to the
 * PML is held on a pending list and claimed later.  Nothing in
 * MPI_Comm_create_from_group orders a peer's first message against the
 * receiver finishing the call.
 *
 * Create a communicator, send a message of every length from 0 up to
 * max_len bytes to the next rank as the first traffic on it, and check
 * that the receive matches, counts and delivers the data.  Lengths near
 * the PML's unexpected-fragment limit (128 bytes by default) take the
 * allocator-backed path.
 *
 * usage: create_from_group_first_msg [iterations [max_len [timeout_s]]]
 *
 * Run with at least two processes.  Failures are reported and abort the
 * job: a wrong header shows up as a receive that never matches, so the
 * waits are bounded.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <mpi.h>

static double now_seconds(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double) ts.tv_sec + 1.0e-9 * (double) ts.tv_nsec;
}

static void fail(MPI_Comm comm, int rank, int iteration, int length, const char *what)
{
    fprintf(stderr, "FAIL rank %d iteration %d length %d: %s\n", rank, iteration, length, what);
    MPI_Abort(comm, 2);
}

int main(int argc, char *argv[])
{
    const int iterations = argc > 1 ? atoi(argv[1]) : 400;
    const int max_len = argc > 2 ? atoi(argv[2]) : 300;
    const double timeout = argc > 3 ? atof(argv[3]) : 60.0;
    MPI_Session session;
    MPI_Group group;
    int rank, size;
    unsigned char *send_buffer, *recv_buffer;

    if (iterations < 1 || max_len < 0) {
        fprintf(stderr, "bad arguments\n");
        return 1;
    }

    if (MPI_SUCCESS != MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session)
        || MPI_SUCCESS != MPI_Group_from_session_pset(session, "mpi://WORLD", &group)) {
        fprintf(stderr, "cannot create a group from the world process set\n");
        return 1;
    }
    MPI_Group_rank(group, &rank);
    MPI_Group_size(group, &size);
    if (size < 2) {
        if (0 == rank) {
            fprintf(stderr, "needs at least two processes\n");
        }
        return 77;
    }

    send_buffer = malloc(max_len + 1);
    recv_buffer = malloc(max_len + 1);
    if (NULL == send_buffer || NULL == recv_buffer) {
        return 1;
    }

    for (int iteration = 0; iteration < iterations; ++iteration) {
        const int length = iteration % (max_len + 1);
        const int next = (rank + 1) % size;
        const int previous = (rank + size - 1) % size;
        char string_tag[64];
        MPI_Comm comm;
        MPI_Request requests[2];
        MPI_Status statuses[2];
        int done = 0, count = -1;
        double start;

        snprintf(string_tag, sizeof(string_tag), "create-from-group-first-msg-%d", iteration);
        if (MPI_SUCCESS
            != MPI_Comm_create_from_group(group, string_tag, MPI_INFO_NULL, MPI_ERRORS_RETURN,
                                          &comm)) {
            fprintf(stderr, "FAIL rank %d iteration %d: MPI_Comm_create_from_group\n", rank,
                    iteration);
            return 1;
        }

        /* the first traffic on the new communicator, as soon as it exists here */
        for (int i = 0; i < length; ++i) {
            send_buffer[i] = (unsigned char) (rank * 31 + iteration + i);
        }
        memset(recv_buffer, 0xA5, max_len + 1);

        MPI_Isend(send_buffer, length, MPI_BYTE, next, 17, comm, &requests[0]);
        MPI_Irecv(recv_buffer, length, MPI_BYTE, previous, 17, comm, &requests[1]);

        start = now_seconds();
        while (!done) {
            MPI_Testall(2, requests, &done, statuses);
            if (!done && now_seconds() - start > timeout) {
                fail(comm, rank, iteration, length, "the exchange did not complete");
            }
        }

        MPI_Get_count(&statuses[1], MPI_BYTE, &count);
        if (count != length) {
            fail(comm, rank, iteration, length, "wrong byte count");
        }
        for (int i = 0; i < length; ++i) {
            if (recv_buffer[i] != (unsigned char) (previous * 31 + iteration + i)) {
                fail(comm, rank, iteration, length, "wrong data");
            }
        }
        if (0xA5 != recv_buffer[length]) {
            fail(comm, rank, iteration, length, "wrote past the end of the receive buffer");
        }

        MPI_Comm_free(&comm);
    }

    free(send_buffer);
    free(recv_buffer);
    MPI_Group_free(&group);
    MPI_Session_finalize(&session);

    if (0 == rank) {
        printf("PASS: %d communicators, lengths 0..%d\n", iterations, max_len);
    }
    return 0;
}

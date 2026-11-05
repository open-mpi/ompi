/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * MPI_Comm_idup returns once the creation is scheduled, so the context
 * id rounds and the activation round are posted later, from the progress
 * loop, at a position that depends on how far each rank happened to get.
 * Meanwhile the application may keep using the parent.  Four overlaps
 * that are legal and have all been broken at some point:
 *
 *   overlap     a nonblocking collective on the parent, posted while the
 *               idup request is outstanding.  If the cid rounds share the
 *               parent's collective stream they take a different slot in
 *               it on different ranks, and a cid round cross-matches the
 *               user collective: a truncation abort under libnbc, wrong
 *               results under a component that does not check sizes.
 *
 *   blocking    MPI_Comm_dup of the same parent while the idup is
 *               outstanding, which pairs the rounds of two creations.
 *
 *   concurrent  two idups of one parent in flight at once.
 *
 *   orphan      MPI_Comm_free of the parent before the idup completes.
 *               MPI-4.1 7.4.3 requires pending operations to complete
 *               normally, so every later step of the request -- the cid
 *               allocation, each round, the activation and the collective
 *               component selection -- still reaches the parent after the
 *               application has let go of it.
 *
 * Ranks are deliberately skewed: the odd ones drive their idup request
 * forward with MPI_Test before posting anything else, the even ones post
 * immediately.  That is what puts the rounds at different positions on
 * different ranks, which is the whole point.
 *
 * The concurrent and orphan cases run again on an intercommunicator,
 * whose cid rounds also use the local communicator of each group.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define ROUNDS 16

static int rank, size, expected, remote_expected;

/* Let the odd ranks run their cid rounds ahead of the even ones, so that
 * whatever the creation posts on the parent lands at a different point in
 * the parent's stream on either side. */
static void skew(MPI_Request *req)
{
    if (0 == (rank & 1)) {
        return;
    }
    for (int i = 0; i < 64; ++i) {
        int flag = 0;
        MPI_Test(req, &flag, MPI_STATUS_IGNORE);
        if (flag) {
            break;
        }
    }
}

/* A nonblocking allreduce on comm, checked.  Returns 0 on success. */
static int checked_iallreduce(MPI_Comm comm, const char *what)
{
    int in = rank + 1, out = 0, inter, want;
    MPI_Request req;

    MPI_Iallreduce(&in, &out, 1, MPI_INT, MPI_SUM, comm, &req);
    MPI_Wait(&req, MPI_STATUS_IGNORE);

    /* On an intercommunicator each group receives the other group's sum. */
    MPI_Comm_test_inter(comm, &inter);
    want = inter ? remote_expected : expected;
    if (out != want) {
        fprintf(stderr, "ERROR: %s: rank %d reduced %d, expected %d\n",
                what, rank, out, want);
        return 1;
    }
    return 0;
}

/* A nonblocking collective on the parent while an idup of it is in flight. */
static int test_overlap(void)
{
    for (int i = 0; i < ROUNDS; ++i) {
        MPI_Comm dup;
        MPI_Request req;

        MPI_Comm_idup(MPI_COMM_WORLD, &dup, &req);
        skew(&req);

        if (checked_iallreduce(MPI_COMM_WORLD, "overlap, parent")) {
            return 1;
        }

        MPI_Wait(&req, MPI_STATUS_IGNORE);

        if (checked_iallreduce(dup, "overlap, duplicate")) {
            return 1;
        }
        MPI_Comm_free(&dup);
    }
    return 0;
}

/* A blocking creation from the parent while an idup of it is in flight. */
static int test_blocking(void)
{
    for (int i = 0; i < ROUNDS; ++i) {
        MPI_Comm idup_comm, dup_comm;
        MPI_Request req;

        MPI_Comm_idup(MPI_COMM_WORLD, &idup_comm, &req);
        skew(&req);

        MPI_Comm_dup(MPI_COMM_WORLD, &dup_comm);
        MPI_Wait(&req, MPI_STATUS_IGNORE);

        if (checked_iallreduce(dup_comm, "blocking, dup") ||
            checked_iallreduce(idup_comm, "blocking, idup")) {
            return 1;
        }
        MPI_Comm_free(&dup_comm);
        MPI_Comm_free(&idup_comm);
    }
    return 0;
}

/* Two idups of one parent in flight at the same time. */
static int test_concurrent(void)
{
    for (int i = 0; i < ROUNDS; ++i) {
        MPI_Comm first, second;
        MPI_Request reqs[2];

        MPI_Comm_idup(MPI_COMM_WORLD, &first, &reqs[0]);
        MPI_Comm_idup(MPI_COMM_WORLD, &second, &reqs[1]);
        skew(&reqs[0]);

        MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);

        if (checked_iallreduce(first, "concurrent, first") ||
            checked_iallreduce(second, "concurrent, second")) {
            return 1;
        }
        MPI_Comm_free(&first);
        MPI_Comm_free(&second);
    }
    return 0;
}

/* The parent is freed before the idup of it completes. */
static int test_orphan(void)
{
    for (int i = 0; i < ROUNDS; ++i) {
        MPI_Comm parent, child;
        MPI_Request req;

        MPI_Comm_dup(MPI_COMM_WORLD, &parent);
        MPI_Comm_idup(parent, &child, &req);

        /* The request still needs the parent for every step that is left. */
        MPI_Comm_free(&parent);
        skew(&req);
        MPI_Wait(&req, MPI_STATUS_IGNORE);

        if (checked_iallreduce(child, "orphan, child")) {
            return 1;
        }
        MPI_Comm_free(&child);
    }
    return 0;
}

#define INTER_IDUPS 4

/* Several idups of one intercommunicator in flight, with a barrier on it. */
static int test_inter_concurrent(MPI_Comm inter)
{
    for (int i = 0; i < ROUNDS; ++i) {
        MPI_Comm child[INTER_IDUPS];
        MPI_Request reqs[INTER_IDUPS + 1];

        for (int j = 0; j < INTER_IDUPS; ++j) {
            MPI_Comm_idup(inter, &child[j], &reqs[j]);
        }
        MPI_Ibarrier(inter, &reqs[INTER_IDUPS]);
        skew(&reqs[0]);

        MPI_Waitall(INTER_IDUPS + 1, reqs, MPI_STATUSES_IGNORE);

        for (int j = 0; j < INTER_IDUPS; ++j) {
            if (checked_iallreduce(child[j], "inter concurrent, child")) {
                return 1;
            }
            MPI_Comm_free(&child[j]);
        }
    }
    return 0;
}

/* An intercommunicator is freed with several idups of it in flight. */
static int test_inter_orphan(MPI_Comm inter)
{
    for (int i = 0; i < ROUNDS; ++i) {
        MPI_Comm parent, child[INTER_IDUPS];
        MPI_Request reqs[INTER_IDUPS];

        MPI_Comm_dup(inter, &parent);
        for (int j = 0; j < INTER_IDUPS; ++j) {
            MPI_Comm_idup(parent, &child[j], &reqs[j]);
        }

        /* Freeing it also releases its local communicator. */
        MPI_Comm_free(&parent);
        skew(&reqs[0]);
        MPI_Waitall(INTER_IDUPS, reqs, MPI_STATUSES_IGNORE);

        for (int j = 0; j < INTER_IDUPS; ++j) {
            if (checked_iallreduce(child[j], "inter orphan, child")) {
                return 1;
            }
            MPI_Comm_free(&child[j]);
        }
    }
    return 0;
}

int main(int argc, char *argv[])
{
    MPI_Comm local, inter;
    int failed;

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

    expected = size * (size + 1) / 2;

    /* Even and odd ranks form the two groups; each leader is the group's lowest rank. */
    MPI_Comm_split(MPI_COMM_WORLD, rank & 1, rank, &local);
    MPI_Intercomm_create(local, 0, MPI_COMM_WORLD, (rank & 1) ? 0 : 1, 0, &inter);
    MPI_Comm_free(&local);
    remote_expected = 0;
    for (int r = 0; r < size; ++r) {
        if ((r & 1) != (rank & 1)) {
            remote_expected += r + 1;
        }
    }

    failed = test_overlap() || test_blocking() || test_concurrent() || test_orphan() ||
             test_inter_concurrent(inter) || test_inter_orphan(inter);

    if (failed) {
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    MPI_Comm_free(&inter);
    if (0 == rank) {
        printf("comm idup overlap: PASSED (%d ranks, %d rounds each)\n", size, ROUNDS);
    }

    MPI_Finalize();
    return 0;
}

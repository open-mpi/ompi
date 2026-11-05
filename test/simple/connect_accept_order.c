/* -*- C -*-
 *
 * Copyright (c) 2026      Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Connect/accept between two separately launched jobs, run under a DVM:
 *
 *   prun -n 3 ./connect_accept_order server [iters] &
 *   prun -n 2 ./connect_accept_order client [iters]
 *
 * The server opens a port, publishes it as "connect_accept_order" and
 * accepts `iters` times on that same port; the client looks it up and
 * connects `iters` times. Accepting repeatedly on one port reuses
 * whatever rendezvous the runtime derives from it.
 *
 * Each side connects over a communicator whose rank order is the reverse
 * of MPI_COMM_WORLD's, so the remote group's order is not the order any
 * sorted membership list would produce. MPI requires remote rank r of the
 * intercommunicator to be rank r of the remote communicator; that, point
 * to point and a collective across the intercommunicator are checked on
 * every iteration. Each check prints
 *   PASS|FAIL <side> it=<n> rank=<r> <what>
 * and each connect after the first prints its time as
 *   TIME <side> it=<n> connect_ms=<ms>
 * The program exits non-zero if any check failed.
 */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int fails = 0;

static void check(int ok, const char *side, int it, int rank, const char *what)
{
    if (!ok) {
        ++fails;
    }
    if (!ok || 0 == rank) {
        printf("%s %s it=%d rank=%d %s\n", ok ? "PASS" : "FAIL", side, it, rank, what);
        fflush(stdout);
    }
}

int main(int argc, char **argv)
{
    char port[MPI_MAX_PORT_NAME];
    int wrank, wsize, lrank, lsize, rsize, it, iters = 3, r, ok;
    int server;
    MPI_Comm rev, inter;
    const char *side;

    if (argc < 2) {
        fprintf(stderr, "usage: connect_accept_order server|client [iters]\n");
        return 2;
    }
    server = (0 == strcmp(argv[1], "server"));
    side = server ? "server" : "client";
    if (argc > 2) {
        iters = atoi(argv[2]);
    }

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
    MPI_Comm_size(MPI_COMM_WORLD, &wsize);

    /* reverse the order: world rank 0 becomes the last local rank */
    MPI_Comm_split(MPI_COMM_WORLD, 0, wsize - wrank, &rev);
    MPI_Comm_rank(rev, &lrank);
    MPI_Comm_size(rev, &lsize);

    if (server) {
        if (0 == lrank) {
            MPI_Open_port(MPI_INFO_NULL, port);
            MPI_Publish_name("connect_accept_order", MPI_INFO_NULL, port);
        }
    } else if (0 == lrank) {
        /* the server may not have published yet */
        int tries;
        MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
        for (tries = 0; tries < 600; tries++) {
            if (MPI_SUCCESS == MPI_Lookup_name("connect_accept_order", MPI_INFO_NULL, port)) {
                break;
            }
            sleep(1);
        }
        MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL);
    }

    for (it = 0; it < iters; it++) {
        int *got, mine, sum, expect;

        double t0, t1, tmax;
        /* line up our side so the time is the connect, not the skew */
        MPI_Barrier(rev);
        t0 = MPI_Wtime();
        if (server) {
            MPI_Comm_accept(port, MPI_INFO_NULL, 0, rev, &inter);
        } else {
            MPI_Comm_connect(port, MPI_INFO_NULL, 0, rev, &inter);
        }
        t1 = MPI_Wtime() - t0;
        MPI_Reduce(&t1, &tmax, 1, MPI_DOUBLE, MPI_MAX, 0, rev);
        if (0 == lrank && it > 0) {
            /* it 0 includes waiting for the other job to start */
            printf("TIME %s it=%d connect_ms=%.3f\n", side, it, tmax * 1000.0);
        }
        MPI_Comm_remote_size(inter, &rsize);

        /* remote rank r must be the proc whose rank in ITS local comm is r */
        got = calloc(rsize, sizeof(int));
        mine = lrank;
        MPI_Allgather(&mine, 1, MPI_INT, got, 1, MPI_INT, inter);
        ok = 1;
        for (r = 0; r < rsize; r++) {
            if (got[r] != r) {
                ok = 0;
                fprintf(stderr, "%s it=%d lrank=%d: remote rank %d reports local rank %d\n",
                        side, it, lrank, r, got[r]);
            }
        }
        free(got);
        MPI_Allreduce(MPI_IN_PLACE, &ok, 1, MPI_INT, MPI_LAND, rev);
        check(ok, side, it, lrank, "remote group order matches the remote comm's order");

        /* point-to-point both ways: send our lrank to remote rank lrank % rsize */
        if (server) {
            /* the server's rank r receives from the client ranks mapping to it */
            expect = 0;
            for (r = 0; r < rsize; r++) {
                if (r % lsize == lrank) {
                    MPI_Recv(&mine, 1, MPI_INT, r, 7, inter, MPI_STATUS_IGNORE);
                    expect += (mine == r);
                }
            }
            sum = 0;
            for (r = 0; r < rsize; r++) {
                sum += (r % lsize == lrank);
            }
            ok = (expect == sum);
        } else {
            MPI_Send(&lrank, 1, MPI_INT, lrank % rsize, 7, inter);
            ok = 1;
        }
        MPI_Allreduce(MPI_IN_PLACE, &ok, 1, MPI_INT, MPI_LAND, rev);
        check(ok, side, it, lrank, "point-to-point across the intercomm");

        /* a collective across both sides */
        mine = 1;
        MPI_Allreduce(&mine, &sum, 1, MPI_INT, MPI_SUM, inter);
        check(sum == rsize, side, it, lrank, "allreduce over the intercomm");

        MPI_Comm_disconnect(&inter);
    }

    if (server && 0 == lrank) {
        MPI_Unpublish_name("connect_accept_order", MPI_INFO_NULL, port);
        MPI_Close_port(port);
    }
    MPI_Allreduce(MPI_IN_PLACE, &fails, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (0 == wrank) {
        printf("%s DONE fails=%d\n", side, fails);
    }
    MPI_Comm_free(&rev);
    MPI_Finalize();
    return fails ? 1 : 0;
}

/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Regression test for the asynchronous modex endpoint wire-up race.
 *
 * When the asynchronous modex is enabled (pmix_base_async_modex), the
 * job-wide data-collecting fence is launched in the background and
 * MPI_Init continues while it is in flight.  Endpoint wire-up (the
 * BTL/SMSC add_procs path) reads each peer's modex blob; PMIx does not
 * defer a get for a peer that has not yet committed its data -- it
 * returns NOT_FOUND -- so any peer that is merely slow to reach its
 * fence reads as a peer that posted nothing.  Its endpoint is then
 * never wired up, and, because the peer wires up *our* endpoint
 * normally, messages it sends us land in a transport we are not
 * polling.  Depending on which transports are available, that presents
 * either as an "unable to reach" abort or as a silent hang the first
 * time this process has to receive.
 *
 * Rather than rely on timing luck, this test *forces* the window: every
 * non-zero rank sleeps before calling MPI_Init, guaranteeing that rank
 * 0 performs its wire-up before its peers have committed anything.  The
 * subsequent collectives require traffic in both directions, so a
 * half-wired endpoint cannot go unnoticed.
 *
 * This test must be run with the asynchronous modex explicitly enabled
 * (see the run-tests target in this directory's Makefile); it passes
 * trivially otherwise.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* How long non-zero ranks stall before MPI_Init.  Long enough that rank
   0 is reliably through add_procs first, short enough to keep the test
   fast in CI. */
#define STALL_SECONDS 3

int main(int argc, char *argv[])
{
    int rank, size, sum, expected;
    const char *rank_str;

    /* We have to decide whether to stall *before* MPI_Init, so use the
       environment rather than MPI_Comm_rank().  If the variable is not
       set (i.e., we were not launched by mpirun), no rank stalls and
       the test still runs correctly -- it just is not a useful race
       test. */
    rank_str = getenv("OMPI_COMM_WORLD_RANK");
    if (NULL != rank_str && 0 != atoi(rank_str)) {
        sleep(STALL_SECONDS);
    }

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

    /* An allreduce forces rank 0 to receive from every peer -- which is
       precisely the direction a half-wired endpoint loses. */
    sum = rank;
    MPI_Allreduce(MPI_IN_PLACE, &sum, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    expected = size * (size - 1) / 2;
    if (sum != expected) {
        fprintf(stderr, "ERROR: rank %d: allreduce gave %d, expected %d\n", rank, sum, expected);
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (0 == rank) {
        printf("async modex wire-up test: PASSED (%d ranks)\n", size);
    }

    MPI_Finalize();
    return 0;
}

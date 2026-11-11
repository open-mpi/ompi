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
 * MPI_Session_finalize is not required to wait for the other processes
 * of the job: a process that released every communicator it derived from
 * its session has nothing left to synchronize on.  An elastic application
 * depends on that when it shrinks.  The processes that leave disconnect,
 * finalize their session and exit, to give their nodes back, while the
 * rest keep running.
 *
 * The UCX components synchronize the whole job with a PMIx fence while
 * they release their resources, which holds a leaving process until every
 * other one has finalized.  Setting the MCA parameter
 * opal_common_ucx_finalize_fence to 0 removes that fence.
 *
 * Every process builds a communicator from the session.  The last rank is
 * split out of it, disconnects and finalizes.  The others keep working in
 * the smaller communicator for work_s seconds, then do the same.  The
 * leaving rank reports how long its MPI_Session_finalize took and fails
 * if it was held for more than half of work_s.
 *
 * usage: mpirun -n 3 --mca opal_common_ucx_finalize_fence 0 \
 *            session_finalize_shrink [work_s]
 *
 * Run with at least two processes.  Without UCX in use the test passes
 * whatever the parameter is.  With UCX in use it fails unless the
 * parameter is 0, which is the point: it checks that the parameter
 * does what it says.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <mpi.h>

int main(int argc, char *argv[])
{
    MPI_Session session;
    MPI_Group group;
    MPI_Comm world_comm, survivors_comm;
    int rank, size, leaving;
    int work_s = 6;
    double start, finalize_s;
    int failed = 0;

    if (argc > 1) {
        work_s = atoi(argv[1]);
        if (work_s < 2) {
            fprintf(stderr, "usage: %s [work_s >= 2]\n", argv[0]);
            return 2;
        }
    }

    MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
    MPI_Group_from_session_pset(session, "mpi://WORLD", &group);
    MPI_Comm_create_from_group(group, "session_finalize_shrink", MPI_INFO_NULL,
                               MPI_ERRORS_RETURN, &world_comm);
    MPI_Group_free(&group);
    MPI_Comm_rank(world_comm, &rank);
    MPI_Comm_size(world_comm, &size);

    if (size < 2) {
        fprintf(stderr, "%s: needs at least two processes\n", argv[0]);
        MPI_Comm_disconnect(&world_comm);
        MPI_Session_finalize(&session);
        return 2;
    }

    /* Shrink: the last rank leaves, the others carry on in a smaller communicator */
    leaving = (rank == size - 1);
    MPI_Comm_split(world_comm, leaving ? MPI_UNDEFINED : 0, rank, &survivors_comm);
    MPI_Comm_disconnect(&world_comm);

    if (!leaving) {
        sleep(work_s); /* the rest of the run */
        MPI_Barrier(survivors_comm);
        MPI_Comm_disconnect(&survivors_comm);
    }

    start = MPI_Wtime();
    MPI_Session_finalize(&session);
    finalize_s = MPI_Wtime() - start;

    if (leaving) {
        failed = (finalize_s > work_s / 2.0);
        printf("rank %d (leaving): MPI_Session_finalize took %.1f s: %s\n", rank, finalize_s,
               failed ? "FAILED, held for the work of the other ranks" : "ok");
    } else {
        printf("rank %d: MPI_Session_finalize took %.1f s\n", rank, finalize_s);
    }

    return failed;
}

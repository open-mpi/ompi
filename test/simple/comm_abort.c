/* -*- C -*-
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Test aborting communicators
 */

#include <stdio.h>
#include <unistd.h>
#include "mpi.h"

#define print1(format...) if(0 == rank) printf(format)


int main(int argc, char* argv[])
{
    int rank, size, more;
    double start, now;
    MPI_Comm comm_pair_fatal, comm_pair_return, comm_pair_abort;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if(0 == rank && size%2) {
        fprintf(stderr, "This test requires an even number of processes\n\n");
        MPI_Abort(MPI_COMM_WORLD, size);
    }

    /* Setup: split our world in a set of 2-processes islands */
    MPI_Comm_split(MPI_COMM_WORLD, rank/2, rank, &comm_pair_fatal);
    MPI_Comm_set_errhandler(comm_pair_fatal, MPI_ERRORS_ARE_FATAL);
    MPI_Comm_split(MPI_COMM_WORLD, rank/2, rank, &comm_pair_return);
    MPI_Comm_set_errhandler(comm_pair_return, MPI_ERRORS_RETURN);
    MPI_Comm_split(MPI_COMM_WORLD, rank/2, rank, &comm_pair_abort);
    /* If this code fails to compile, the MPI implementation is not compliant 
     * with MPI-4 (TODO: add ref to chapter/line when MPI-4 published). */
    MPI_Comm_set_errhandler(comm_pair_abort, MPI_ERRORS_ABORT);
    MPI_Barrier(MPI_COMM_WORLD);

    print1(
"This program will test partial abort functionality (communicator scoped abort).\n"
"   Each test will perform a loop of communication on a subcommunicator for about\n"
"   1 second between printouts, and then, a 1 second cooldown.\n");

    print1("\n\n"
"Test1: MPI_Abort(MPI_COMM_SELF) aborts only one process?\n"
"       In a high quality implementation, all ranks except %d\n"
"       should report their presence.\n", 1);
    if(rank == 1) {
        MPI_Abort(MPI_COMM_SELF, 1);
    }
    /* Spin on communication for 1 second to let time for Abort to have an
     * effect, if any. */
    more = 1; start = MPI_Wtime();
    do {
        now = MPI_Wtime();
        if(now - start > 1.) more = 0;
        if(rank > 1) /* don't reduce on aborted pairs */
            MPI_Allreduce(MPI_IN_PLACE, &more, 1, MPI_INT, MPI_MIN, comm_pair_fatal);
    } while(more);
    printf("  This is rank %d: still kickin after %d MPI_Abort'ed self\n", rank, 1);

    sleep(1);
    print1("===============================================================\n");

    print1("\n\n"
"Test2: MPI_Abort(comm) aborts all processes in comm?\n"
"       In a high quality implementation, all ranks except %d--%d\n"
"       should report their presence.\n", 1, 3);
    if(rank == 3) {
        MPI_Abort(comm_pair_return, 2);
    }
    /* Spin on communication for 1 second to let time for Abort to have an
     * effect, if any. */
    more = 1; start = MPI_Wtime();
    do {
        now = MPI_Wtime();
        if(now - start > 1.) more = 0;
        if(rank > 3) /* don't reduce on aborted pairs */
            MPI_Allreduce(MPI_IN_PLACE, &more, 1, MPI_INT, MPI_MIN, comm_pair_fatal);
    } while(more);
    printf("  This is rank %d: still kickin after %d aborted comm pair %d-%d\n", rank, 3, 2, 3);

    /* This process should have aborted, give it an opportunity to do so if no
     * async progress: message to self to spin MPI progress. */
    if(rank == 2) {
        MPI_Sendrecv(&start, 1, MPI_DOUBLE, 0, 0,
                     &now, 1, MPI_DOUBLE, 0, 0, 
                     MPI_COMM_SELF, MPI_STATUS_IGNORE);
        printf("  This is rank %d: ERROR: I SHOULD HAVE ABORTED!\n", 2);
    }

    sleep(1);
    print1("===============================================================\n");

    print1("\n\n"
"Test3: MPI_ERRORS_ABORT aborts all processes in comm?\n"
"       In a high quality implementation, all ranks except %d--%d\n"
"       should report their presence.\n", 1, 5);
    if(rank == 5) {
        MPI_Comm_call_errhandler(comm_pair_abort, 3);
    }
    /* Spin on communication for 1 second to let time for Abort to have an
     * effect, if any. */
    more = 1; start = MPI_Wtime();
    do {
        now = MPI_Wtime();
        if(now - start > 1.) more = 0;
        if(rank > 5) /* don't reduce on aborted pairs */
            MPI_Allreduce(MPI_IN_PLACE, &more, 1, MPI_INT, MPI_MIN, comm_pair_fatal);
    } while(more);
    printf("  This is rank %d: still kickin after %d aborted comm pair %d-%d\n", rank, 5, 4, 5);

    /* This process should have aborted, give it an opportunity to do so if no
     * async progress: message to self to spin MPI progress. */
    if(rank == 4) {
        MPI_Sendrecv(&start, 1, MPI_DOUBLE, 0, 0,
                     &now, 1, MPI_DOUBLE, 0, 0, 
                     MPI_COMM_SELF, MPI_STATUS_IGNORE);
        printf("  This is rank %d: ERROR: I SHOULD HAVE ABORTED!\n", 4);
    }

    sleep(1);
    print1("===============================================================\n");

    print1("\n\n"\
"Test4: Communicating with an aborted process %d returns a good error code?\n"
"       In a high quality implementation, rank %d should print an error string;\n"
"       In a higher quality implementation the error should be of class\n"
"       MPI_ERR_PROC_ABORTED.\n", 1, 0);
    if(rank == 0) {
        int err, class, slen;
        char str[MPI_MAX_ERROR_STRING];
        /* remember, 1 aborted in test1 */
        MPI_Error_class(err, &class);
        MPI_Error_string(err, str, &slen);
        err = MPI_Recv(&more, 1, MPI_INT, 1, 0, comm_pair_return, MPI_STATUS_IGNORE);
        printf("  This is rank %d: Recv(from=%d) returned code=%d: class=%d: %s\n", 0, 1, err, class, str);
    }

    sleep(1);
    print1("===============================================================\n");

    print1("\n\n"
"Test5: MPI_ERRORS_ARE_FATAL aborts all processes?\n");
    if(rank == 0) {
        MPI_Comm_call_errhandler(comm_pair_fatal, 5);
    }
    /* Spin on communication for 1 second to let time for Abort to have an
     * effect, if any. */
    more = 1; start = MPI_Wtime();
    do {
        now = MPI_Wtime();
        if(now - start > 1.) more = 0;
        if(rank > 5) /* don't reduce on aborted pairs */
            MPI_Allreduce(MPI_IN_PLACE, &more, 1, MPI_INT, MPI_MIN, comm_pair_fatal);
    } while(more);
    MPI_Sendrecv(&start, 1, MPI_DOUBLE, 0, 0,
                 &now, 1, MPI_DOUBLE, 0, 0, 
                 MPI_COMM_SELF, MPI_STATUS_IGNORE);
    printf("  This is rank %d: ERROR: I SHOULD HAVE ABORTED!\n", rank);

    /* Should never get there */

    MPI_Finalize();
    return 0;
}

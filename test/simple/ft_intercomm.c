/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * ULFM on an intercommunicator: agreement, non-blocking agreement, shrink,
 * and communication over the result.
 *
 * The program spawns a second set of processes and runs every phase on the
 * resulting intercommunicator.  Run it as
 *
 *     mpirun --with-ft ulfm -np 2 ./ft_intercomm [nchildren]
 *
 * By default no process is killed, so the run is expected to succeed and
 * exit 0.  Add the 'fail' argument
 *
 *     mpirun --with-ft ulfm -np 2 ./ft_intercomm 3 fail
 *
 * to also kill one of the children and exercise the recovery phases.  mpirun
 * reports the killed process, so a nonzero exit status is expected even when
 * the test passes; look for the PASS/FAIL lines instead.
 *
 * Use 'both' instead of 'fail' to kill the last process of each side at
 * once, so that each survivor sees one failure in its local group and one in
 * its remote group, and to check how ANY_SOURCE receives follow the
 * acknowledgement of them.  It needs at least 2 parents and 2 children.
 *
 * Nothing is killed unless ULFM is actually enabled: without it the argument
 * is ignored and the run stays fault free.
 *
 * The surviving processes do not reach the end of MPI_Finalize: its PMIx
 * fence waits on every process of the namespace, including the one that
 * died.  That is not specific to intercommunicators -- any ULFM job that
 * loses a process hangs there -- so the test prints its last result and
 * mpirun's timeout, not the exit, is what ends the run.
 *
 * An agreement on an intercommunicator combines the contributions of the
 * remote group, like any other intercommunicator collective, so the two
 * groups contribute distinguishable values and check that they get back the
 * other group's and not their own.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

#include <mpi.h>
#include <mpi-ext.h>

#if !defined(OMPI_HAVE_MPI_EXT_FTMPI) || !OMPI_HAVE_MPI_EXT_FTMPI

int main(int argc, char *argv[])
{
    MPI_Init(&argc, &argv);
    printf("SKIP: this Open MPI was built without ULFM\n");
    MPI_Finalize();
    return 0;
}

#else

#define PARENT_CONTRIB ~0x1
#define CHILD_CONTRIB  ~0x2

#define ANY_SOURCE_TAG   77
#define FROM_PARENT      0xcafe
#define FROM_CHILD       0xbeef

static const char *whoami = "?";
static int rank = -1;
static int errors = 0;

static void check(int cond, const char *what)
{
    if (cond) {
        if (0 == rank) {
            printf("PASS %s: %s\n", whoami, what);
        }
    } else {
        printf("FAIL %s rank %d: %s\n", whoami, rank, what);
        errors++;
    }
    fflush(stdout);
}

/* ULFM is a run time choice, and without it a killed process is just a dead
 * job rather than a recovery to test.  mpirun exports the switch it set to
 * every process it launches, so the test can tell before it kills one. */
static int ft_enabled(void)
{
    const char *val = getenv("OMPI_MCA_mpi_ft_enable");

    if (NULL == val || '\0' == val[0]) {
        return 0;
    }
    /* an MCA boolean: 0, f(alse), n(o) and d(isabled) are off */
    return (NULL == strchr("0fFnNdD", val[0]));
}

static const char *errstr(int rc)
{
    static char str[MPI_MAX_ERROR_STRING];
    int len;

    if (MPI_SUCCESS != MPI_Error_string(rc, str, &len)) {
        snprintf(str, sizeof(str), "unknown error %d", rc);
    }
    return str;
}

/* Every survivor sees one failed process in its local group and one in its
 * remote group.  MPI_ANY_SOURCE on an intercommunicator draws from the remote
 * group, so it is disabled by the failure there, and by that one only: once
 * the acknowledgements cover it, the one in the local group no longer
 * matters.  Which of the two comes first in the order acknowledgements count
 * in depends on the order the failures were noticed, so the expectation is
 * derived from it. */
static void any_source_with_both_failures(MPI_Comm icomm, int nlocal)
{
    MPI_Group fgroup, rgroup;
    MPI_Request anyreq;
    double start;
    int anyval = 0, nfailed = 0, nacked = -1, first_is_remote = 0;
    int first = 0, translated = MPI_UNDEFINED, done = 0, rc;

    if (rank == nlocal - 1) {
        printf("%s rank %d: exiting abruptly\n", whoami, rank);
        fflush(stdout);
        raise(SIGKILL);
        /* not reached */
    }

    /* Failures are noticed asynchronously, and only while the library makes
     * progress: keep it busy until both are known */
    start = MPI_Wtime();
    do {
        int ignored;

        MPI_Iprobe(MPI_ANY_SOURCE, ANY_SOURCE_TAG, MPI_COMM_SELF, &ignored, MPI_STATUS_IGNORE);
        rc = MPIX_Comm_get_failed(icomm, &fgroup);
        if (MPI_SUCCESS == rc) {
            MPI_Group_size(fgroup, &nfailed);
            if (2 > nfailed) {
                MPI_Group_free(&fgroup);
            }
        }
        if (2 > nfailed) {
            usleep(1000);
        }
    } while (2 > nfailed && 60.0 > MPI_Wtime() - start);
    if (2 != nfailed) {
        printf("     %s rank %d: only %d failures known\n", whoami, rank, nfailed);
    }
    check(2 == nfailed, "get_failed reports the failure of each group");
    if (2 != nfailed) {
        return;
    }

    MPI_Comm_remote_group(icomm, &rgroup);
    MPI_Group_translate_ranks(fgroup, 1, &first, rgroup, &translated);
    first_is_remote = (MPI_UNDEFINED != translated);
    MPI_Group_free(&rgroup);
    MPI_Group_free(&fgroup);

    rc = MPI_Irecv(&anyval, 1, MPI_INT, MPI_ANY_SOURCE, ANY_SOURCE_TAG, icomm, &anyreq);
    check(MPI_SUCCESS == rc, "an ANY_SOURCE receive can be posted");
    rc = MPI_Test(&anyreq, &done, MPI_STATUS_IGNORE);
    check(MPIX_ERR_PROC_FAILED_PENDING == rc && !done,
          "ANY_SOURCE is disabled by the failure in the remote group");

    /* Acknowledge the failure that came first, and only that one */
    rc = MPIX_Comm_ack_failed(icomm, 1, &nacked);
    check(MPI_SUCCESS == rc && 1 == nacked, "ack_failed acknowledges the first failure");
    rc = MPI_Test(&anyreq, &done, MPI_STATUS_IGNORE);
    if (first_is_remote) {
        check(MPI_SUCCESS == rc && !done,
              "ANY_SOURCE resumes when the remote failure is acknowledged first");
    } else {
        check(MPIX_ERR_PROC_FAILED_PENDING == rc && !done,
              "ANY_SOURCE stays disabled while the remote failure is not acknowledged");
    }

    rc = MPIX_Comm_ack_failed(icomm, 2, &nacked);
    check(MPI_SUCCESS == rc && 2 == nacked, "ack_failed acknowledges both failures");
    rc = MPI_Test(&anyreq, &done, MPI_STATUS_IGNORE);
    check(MPI_SUCCESS == rc && !done, "ANY_SOURCE resumes once both are acknowledged");

    MPI_Cancel(&anyreq);
    MPI_Wait(&anyreq, MPI_STATUS_IGNORE);
}

int main(int argc, char *argv[])
{
    MPI_Comm icomm, shrunk;
    MPI_Group fgroup;
    MPI_Request req, anyreq = MPI_REQUEST_NULL;
    int nfailed = -1, nacked = -1;
    int anyval = 0, anydone = 0, outgoing;
    int nchildren = 2;
    int dofail = 0;
    int doboth = 0;
    int parent_side, nlocal, nremote;
    int contrib, expected;
    int flag, rc, i;

    MPI_Init(&argc, &argv);

    for (i = 1; i < argc; i++) {
        if (0 == strcmp(argv[i], "fail")) {
            dofail = 1;
        } else if (0 == strcmp(argv[i], "both")) {
            doboth = 1;
        } else {
            nchildren = atoi(argv[i]);
        }
    }

    /* The local group of the intercommunicator is MPI_COMM_WORLD on either
     * side, so this rank is the one the phases below report under. */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Comm_get_parent(&icomm);
    parent_side = (MPI_COMM_NULL == icomm);
    whoami = parent_side ? "parent" : "child";

    if ((dofail || doboth) && !ft_enabled()) {
        if (parent_side && 0 == rank) {
            printf("SKIP parent: the recovery phases need 'mpirun --with-ft "
                   "ulfm'; running fault free instead\n");
            fflush(stdout);
        }
        dofail = doboth = 0;
    }

    if (parent_side) {
        char *spawn_argv[2] = { NULL, NULL };

        if (doboth) {
            int nparents;

            MPI_Comm_size(MPI_COMM_WORLD, &nparents);
            spawn_argv[0] = (char *) "both";
            if (nchildren < 2 || nparents < 2) {
                printf("FAIL parent: 'both' kills a process of each side and then "
                       "communicates with another one, so it needs at least 2 "
                       "parents and 2 children\n");
                MPI_Abort(MPI_COMM_WORLD, 1);
            }
        } else if (dofail) {
            spawn_argv[0] = (char *) "fail";
            if (nchildren < 2) {
                printf("FAIL parent: 'fail' kills a child and then communicates "
                       "with another one, so it needs at least 2 children\n");
                MPI_Abort(MPI_COMM_WORLD, 1);
            }
        }
        rc = MPI_Comm_spawn(argv[0], spawn_argv, nchildren, MPI_INFO_NULL, 0,
                            MPI_COMM_WORLD, &icomm, MPI_ERRCODES_IGNORE);
        if (MPI_SUCCESS != rc) {
            printf("FAIL parent: could not spawn %d children: %s\n", nchildren, errstr(rc));
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    MPI_Comm_rank(icomm, &rank);
    MPI_Comm_set_errhandler(icomm, MPI_ERRORS_RETURN);

    /* The children were spawned together, so their MPI_COMM_WORLD is exactly
     * the local group of the intercommunicator; the children are not told how
     * many of them there are. */
    MPI_Comm_size(MPI_COMM_WORLD, &nlocal);

    contrib  = (0 == rank) ? (parent_side ? PARENT_CONTRIB : CHILD_CONTRIB) : ~0;
    expected = parent_side ? CHILD_CONTRIB : PARENT_CONTRIB;

    /* Phase 1: blocking agreement over the intercommunicator */
    flag = contrib;
    rc = MPIX_Comm_agree(icomm, &flag);
    check(MPI_SUCCESS == rc, "agree returns success");
    check(expected == flag, "agree combines the remote group's contributions");

    /* Phase 2: the same agreement, started and then waited for.  The value
     * is only valid once the request completes, so check it is untouched
     * while the agreement is in flight. */
    flag = contrib;
    rc = MPIX_Comm_iagree(icomm, &flag, &req);
    check(MPI_SUCCESS == rc, "iagree starts");
    rc = MPI_Wait(&req, MPI_STATUS_IGNORE);
    check(MPI_SUCCESS == rc, "iagree completes");
    check(expected == flag, "iagree combines the remote group's contributions");

    /* Phase 3: once more, to check that the state the first agreements left
     * behind on the communicator is reusable rather than merely harmless. */
    flag = contrib;
    rc = MPIX_Comm_agree(icomm, &flag);
    check(MPI_SUCCESS == rc && expected == flag, "a later agreement reuses the earlier state");

    if (doboth) {
        any_source_with_both_failures(icomm, nlocal);
        MPI_Finalize();
        return errors;
    }

    if (!dofail) {
        MPI_Comm_disconnect(&icomm);
        MPI_Finalize();
        return errors;
    }

    MPI_Comm_remote_size(icomm, &nremote);

    /* Phase 4: kill the last child, and let everybody else discover it
     * through the agreement. */
    if (!parent_side && rank == nlocal - 1) {
        printf("child rank %d: exiting abruptly\n", rank);
        fflush(stdout);
        raise(SIGKILL);
        /* not reached */
    }

    flag = contrib;
    rc = MPIX_Comm_agree(icomm, &flag);
    check(MPIX_ERR_PROC_FAILED == rc, "agree reports the failure");

    /* The agreement is still agreeing: everybody must see the same value
     * even in the round that reports the failure. */
    if (expected != flag) {
        printf("     %s rank %d: agreed 0x%x, expected 0x%x\n", whoami, rank, flag, expected);
    }
    check(expected == flag, "agree is still a correct agreement after a failure");

    /* The dead process is in the remote group as the parents see it and in
     * the local group as the children do; the query has to find it either
     * way. */
    rc = MPIX_Comm_get_failed(icomm, &fgroup);
    if (MPI_SUCCESS == rc) {
        MPI_Group_size(fgroup, &nfailed);
        MPI_Group_free(&fgroup);
    }
    check(MPI_SUCCESS == rc && 1 == nfailed, "get_failed reports it from either side");

    /* MPI_ANY_SOURCE on an intercommunicator only draws from the remote
     * group, so only a failure there may disable it.  The dead child is
     * remote to the parents and local to the other children.  Nothing has
     * been acknowledged yet. */
    if (0 == rank) {
        rc = MPI_Irecv(&anyval, 1, MPI_INT, MPI_ANY_SOURCE, ANY_SOURCE_TAG, icomm, &anyreq);
        check(MPI_SUCCESS == rc, "an ANY_SOURCE receive can be posted after a failure");
        if (parent_side) {
            rc = MPI_Test(&anyreq, &anydone, MPI_STATUS_IGNORE);
            check(MPIX_ERR_PROC_FAILED_PENDING == rc && !anydone,
                  "a failure in the remote group disables ANY_SOURCE");
            outgoing = FROM_PARENT;
            rc = MPI_Send(&outgoing, 1, MPI_INT, 0, ANY_SOURCE_TAG, icomm);
            check(MPI_SUCCESS == rc, "send to a live child after a failure");
        } else {
            rc = MPI_Wait(&anyreq, MPI_STATUS_IGNORE);
            check(MPI_SUCCESS == rc && FROM_PARENT == anyval,
                  "a failure in the local group leaves ANY_SOURCE enabled");
        }
    }

    /* Acknowledging it has to work from either side too, and an agreement
     * stops reporting a failure that has been acknowledged. */
    rc = MPIX_Comm_ack_failed(icomm, nlocal + nremote, &nacked);
    check(MPI_SUCCESS == rc && 1 == nacked, "ack_failed acknowledges it from either side");

    flag = contrib;
    rc = MPIX_Comm_agree(icomm, &flag);
    check(MPI_SUCCESS == rc && expected == flag, "agree ignores an acknowledged failure");

    /* The parents' pending ANY_SOURCE receive resumes now that the remote
     * failure is acknowledged, and a new one gets matched too. */
    if (0 == rank) {
        if (parent_side) {
            rc = MPI_Wait(&anyreq, MPI_STATUS_IGNORE);
            check(MPI_SUCCESS == rc && FROM_CHILD == anyval,
                  "ANY_SOURCE resumes once the remote failure is acknowledged");
        } else {
            outgoing = FROM_CHILD;
            rc = MPI_Send(&outgoing, 1, MPI_INT, 0, ANY_SOURCE_TAG, icomm);
            check(MPI_SUCCESS == rc, "send to the parent after the acknowledgement");
        }
    }

    /* Phase 5: rebuild, and use what we rebuilt */
    rc = MPIX_Comm_shrink(icomm, &shrunk);
    if (MPI_SUCCESS != rc) {
        printf("     %s rank %d: shrink failed with %s\n", whoami, rank, errstr(rc));
    }
    check(MPI_SUCCESS == rc, "shrink rebuilds the intercommunicator");
    if (MPI_SUCCESS != rc) {
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Comm_set_errhandler(shrunk, MPI_ERRORS_RETURN);

    MPI_Comm_test_inter(shrunk, &i);
    check(i, "shrink produced an intercommunicator");
    MPI_Comm_size(shrunk, &i);
    check(i == (parent_side ? nlocal : nlocal - 1), "the local group lost its dead");
    MPI_Comm_remote_size(shrunk, &i);
    check(i == (parent_side ? nremote - 1 : nremote), "the remote group lost its dead");

    flag = contrib;
    rc = MPIX_Comm_agree(shrunk, &flag);
    check(MPI_SUCCESS == rc && expected == flag, "agree over the shrunk intercommunicator");

    /* Point to point across the shrunk intercommunicator: the agreement
     * machinery must not have disturbed the identity of either side. */
    MPI_Comm_rank(shrunk, &rank);
    if (0 == rank) {
        int sent = 0xfeed, recvd = 0;

        if (parent_side) {
            rc = MPI_Send(&sent, 1, MPI_INT, 0, 42, shrunk);
            check(MPI_SUCCESS == rc, "send over the shrunk intercommunicator");
        } else {
            rc = MPI_Recv(&recvd, 1, MPI_INT, 0, 42, shrunk, MPI_STATUS_IGNORE);
            check(MPI_SUCCESS == rc && 0xfeed == recvd,
                  "receive over the shrunk intercommunicator");
        }
    }

    MPI_Comm_disconnect(&shrunk);
    MPI_Finalize();

    return errors;
}

#endif /* OMPI_HAVE_MPI_EXT_FTMPI */

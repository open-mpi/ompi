/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Single-process (no launcher required) regression test for partitioned
 * communication with MPI_PROC_NULL.
 *
 * MPI-5.0 section 3.10 (p.110) states:
 *
 *   "The special value MPI_PROC_NULL can be used instead of a rank
 *    wherever a source or a destination argument is required in a call.
 *    A communication with MPI_PROC_NULL has no effect.  A send to
 *    MPI_PROC_NULL succeeds and returns as soon as possible.  A receive
 *    from MPI_PROC_NULL succeeds and returns as soon as possible with no
 *    modifications to the receive buffer.  When a receive with source =
 *    MPI_PROC_NULL is executed then the status object returns source =
 *    MPI_PROC_NULL, tag = MPI_ANY_TAG and count = 0."
 *
 * MPI_Psend_init and MPI_Precv_init take dest and source arguments, so
 * MPI_PROC_NULL is valid for both.  Open MPI used to pass MPI_PROC_NULL
 * straight through to the PML as if it were a real peer rank, which read
 * past the start of the communicator's process array.
 */

#include <stdio.h>

#include "mpi.h"

#define PARTITIONS 4
#define COUNT 3

static int failures = 0;

static void check(int condition, const char *msg)
{
    if (!condition) {
        fprintf(stderr, "FAIL: %s\n", msg);
        ++failures;
    }
}

static void check_rc(int rc, const char *msg)
{
    if (MPI_SUCCESS != rc) {
        char estr[MPI_MAX_ERROR_STRING];
        int elen;
        MPI_Error_string(rc, estr, &elen);
        fprintf(stderr, "FAIL: %s returned '%s' (expected MPI_SUCCESS)\n",
                msg, estr);
        ++failures;
    }
}

/* An MPI_PROC_NULL receive must leave the buffer untouched. */
static void check_untouched(const double *buf, const char *msg)
{
    int i;

    for (i = 0; i < PARTITIONS * COUNT; ++i) {
        if (buf[i] != -1.0) {
            fprintf(stderr, "FAIL: %s: receive buffer was modified\n", msg);
            ++failures;
            return;
        }
    }
}

int main(int argc, char *argv[])
{
    double sendbuf[PARTITIONS * COUNT];
    double recvbuf[PARTITIONS * COUNT];
    MPI_Request sreq = MPI_REQUEST_NULL;
    MPI_Request rreq = MPI_REQUEST_NULL;
    MPI_Status status;
    int i, flag, rc, count;

    MPI_Init(&argc, &argv);

    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    for (i = 0; i < PARTITIONS * COUNT; ++i) {
        sendbuf[i] = (double) i;
        recvbuf[i] = -1.0;
    }

    /* Both of these must succeed rather than reaching the PML with
       MPI_PROC_NULL as a peer rank. */
    rc = MPI_Psend_init(sendbuf, PARTITIONS, COUNT, MPI_DOUBLE, MPI_PROC_NULL,
                        1, MPI_COMM_WORLD, MPI_INFO_NULL, &sreq);
    check_rc(rc, "MPI_Psend_init(MPI_PROC_NULL)");
    rc = MPI_Precv_init(recvbuf, PARTITIONS, COUNT, MPI_DOUBLE, MPI_PROC_NULL,
                        1, MPI_COMM_WORLD, MPI_INFO_NULL, &rreq);
    check_rc(rc, "MPI_Precv_init(MPI_PROC_NULL)");

    if (0 == failures) {
        /* An inactive request is trivially arrived (MPI-5.0 sec 4.2.2). */
        flag = 0;
        rc = MPI_Parrived(rreq, 0, &flag);
        check_rc(rc, "MPI_Parrived (inactive, MPI_PROC_NULL)");
        check(flag, "inactive MPI_PROC_NULL request: expected flag = true");

        rc = MPI_Start(&sreq);
        check_rc(rc, "MPI_Start (MPI_PROC_NULL send)");
        rc = MPI_Start(&rreq);
        check_rc(rc, "MPI_Start (MPI_PROC_NULL receive)");

        /* Marking partitions ready has no effect, but must succeed. */
        for (i = 0; i < PARTITIONS; ++i) {
            rc = MPI_Pready(i, sreq);
            check_rc(rc, "MPI_Pready (MPI_PROC_NULL)");
        }
        rc = MPI_Pready_range(0, PARTITIONS - 1, sreq);
        check_rc(rc, "MPI_Pready_range (MPI_PROC_NULL)");
        {
            int list[PARTITIONS];
            for (i = 0; i < PARTITIONS; ++i) {
                list[i] = i;
            }
            rc = MPI_Pready_list(PARTITIONS, list, sreq);
            check_rc(rc, "MPI_Pready_list (MPI_PROC_NULL)");
        }

        /* A receive from MPI_PROC_NULL "succeeds ... as soon as possible":
           every partition is arrived, without any peer ever sending. */
        for (i = 0; i < PARTITIONS; ++i) {
            flag = 0;
            rc = MPI_Parrived(rreq, i, &flag);
            check_rc(rc, "MPI_Parrived (active, MPI_PROC_NULL)");
            check(flag, "active MPI_PROC_NULL request: expected flag = true");
        }

        /* Completion must not block: there is no peer. */
        rc = MPI_Wait(&sreq, MPI_STATUS_IGNORE);
        check_rc(rc, "MPI_Wait (MPI_PROC_NULL send)");

        status.MPI_SOURCE = 0;
        status.MPI_TAG = 0;
        rc = MPI_Wait(&rreq, &status);
        check_rc(rc, "MPI_Wait (MPI_PROC_NULL receive)");

        /* MPI-5.0 sec 3.10: source = MPI_PROC_NULL, tag = MPI_ANY_TAG,
           count = 0, and the receive buffer is not modified. */
        check(MPI_PROC_NULL == status.MPI_SOURCE,
              "expected status.MPI_SOURCE = MPI_PROC_NULL");
        check(MPI_ANY_TAG == status.MPI_TAG,
              "expected status.MPI_TAG = MPI_ANY_TAG");
        rc = MPI_Get_count(&status, MPI_DOUBLE, &count);
        check_rc(rc, "MPI_Get_count");
        check(0 == count, "expected status count = 0");
        check_untouched(recvbuf, "MPI_PROC_NULL receive");
    }

    MPI_Request_free(&sreq);
    MPI_Request_free(&rreq);

    if (0 == failures) {
        printf("PASS: partitioned_proc_null\n");
    }

    MPI_Finalize();

    return failures ? 1 : 0;
}

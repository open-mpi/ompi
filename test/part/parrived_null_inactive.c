/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Single-process (no launcher required) regression test for Open MPI
 * issue #14003: MPI_Parrived must return flag = true when called with a
 * null or an inactive request.
 *
 * MPI-5.0 section 4.2.2 (p.119) states:
 *
 *   "MPI_PARRIVED may be called with a null or inactive request
 *    argument.  In either case, the operation returns with flag = true.
 *    Calling MPI_PARRIVED on a request that does not correspond to a
 *    partitioned receive operation is erroneous."
 *
 * Four cases are covered: a null request, an inactive request that has
 * never been started, an active request being probed per partition, and
 * an inactive request that has completed and been reset.
 *
 * The partitioned send/receive pair is set up on MPI_COMM_SELF, so this
 * is a valid single-process test and needs no launcher.
 *
 * Note: Open MPI only guarantees the null-request case when MPI
 * parameter checking is enabled (the check lives in the C binding, which
 * is compiled/branched out when checking is off, so as not to burden the
 * probing fast path).  The null case is therefore skipped when the
 * mpi_param_check MCA variable reads as false.
 */

#include <stdio.h>
#include <string.h>

#include "mpi.h"

/* Bound the polling loops so that a regression fails the test rather
   than hanging "make check" forever. */
#define MAX_POLL 10000000

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

/* Verify that MPI_Parrived succeeded and returned the expected flag. */
static void check_parrived(int rc, int flag, int expected, const char *msg)
{
    if (MPI_SUCCESS != rc) {
        char estr[MPI_MAX_ERROR_STRING];
        int elen;
        MPI_Error_string(rc, estr, &elen);
        fprintf(stderr, "FAIL: %s: MPI_Parrived returned '%s' (expected "
                "MPI_SUCCESS)\n", msg, estr);
        ++failures;
    } else if (!!flag != !!expected) {
        fprintf(stderr, "FAIL: %s: MPI_Parrived returned flag=%s (expected "
                "%s)\n", msg, flag ? "true" : "false",
                expected ? "true" : "false");
        ++failures;
    }
}

/* Read the mpi_param_check MCA variable through MPI_T.  Assume checking
   is enabled if it cannot be read (that is the default). */
static int param_check_enabled(void)
{
    int provided, num_cvar, i, enabled = 1;

    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        return enabled;
    }
    if (MPI_SUCCESS != MPI_T_cvar_get_num(&num_cvar)) {
        MPI_T_finalize();
        return enabled;
    }

    for (i = 0; i < num_cvar; ++i) {
        char name[128];
        int name_len = (int) sizeof(name);
        int desc_len = 0;
        int verbosity, bind, scope, count;
        MPI_Datatype datatype;
        MPI_T_enum enumtype;
        MPI_T_cvar_handle handle;
        int value;

        if (MPI_SUCCESS != MPI_T_cvar_get_info(i, name, &name_len, &verbosity,
                                               &datatype, &enumtype, NULL,
                                               &desc_len, &bind, &scope)) {
            continue;
        }
        if (0 != strcmp(name, "mpi_param_check") || MPI_INT != datatype) {
            continue;
        }
        if (MPI_SUCCESS == MPI_T_cvar_handle_alloc(i, NULL, &handle, &count)) {
            if (1 == count && MPI_SUCCESS == MPI_T_cvar_read(handle, &value)) {
                enabled = value;
            }
            MPI_T_cvar_handle_free(&handle);
        }
        break;
    }

    MPI_T_finalize();

    return enabled;
}

int main(int argc, char *argv[])
{
    double sendbuf[PARTITIONS * COUNT];
    double recvbuf[PARTITIONS * COUNT];
    MPI_Request sreq = MPI_REQUEST_NULL;
    MPI_Request rreq = MPI_REQUEST_NULL;
    int i, j, flag, rc;

    MPI_Init(&argc, &argv);

    /* Report errors instead of aborting, so that a non-conformant
       MPI_ERR_REQUEST is counted as a test failure. */
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);

    for (i = 0; i < PARTITIONS * COUNT; ++i) {
        sendbuf[i] = (double) i;
        recvbuf[i] = -1.0;
    }

    /* Case 1: a null request is trivially arrived.  MPI_Parrived may be
       called more than once for a partition, so call it twice. */
    if (param_check_enabled()) {
        for (i = 0; i < 2; ++i) {
            flag = 0;
            rc = MPI_Parrived(MPI_REQUEST_NULL, 0, &flag);
            check_parrived(rc, flag, 1, "null request");
        }
    } else {
        printf("SKIP: null request (MPI parameter checking is disabled)\n");
    }

    /* Case 2: an inactive (initialized but never started) partitioned
       receive request is trivially arrived. */
    rc = MPI_Precv_init(recvbuf, PARTITIONS, COUNT, MPI_DOUBLE, 0, 1,
                        MPI_COMM_SELF, MPI_INFO_NULL, &rreq);
    check(MPI_SUCCESS == rc, "MPI_Precv_init failed");
    if (MPI_SUCCESS == rc) {
        for (i = 0; i < PARTITIONS; ++i) {
            flag = 0;
            rc = MPI_Parrived(rreq, i, &flag);
            check_parrived(rc, flag, 1, "never-started inactive request");
        }
        MPI_Request_free(&rreq);
    }

    /* Cases 3 and 4: an active partitioned receive must report per-
       partition arrival, and the same request must again report arrived
       once it has completed and returned to the inactive state.

       Send to and receive from MPI_COMM_SELF: matching is by the order in
       which the initialization calls are made (MPI-5.0 section 4.2.3). */
    rc = MPI_Psend_init(sendbuf, PARTITIONS, COUNT, MPI_DOUBLE, 0, 1,
                        MPI_COMM_SELF, MPI_INFO_NULL, &sreq);
    check(MPI_SUCCESS == rc, "MPI_Psend_init failed");
    rc = MPI_Precv_init(recvbuf, PARTITIONS, COUNT, MPI_DOUBLE, 0, 1,
                        MPI_COMM_SELF, MPI_INFO_NULL, &rreq);
    check(MPI_SUCCESS == rc, "MPI_Precv_init failed");

    if (0 == failures) {
        MPI_Start(&sreq);
        MPI_Start(&rreq);

        /* Case 3a: no partition has been marked ready yet, so no
           partition can have arrived. */
        flag = 1;
        rc = MPI_Parrived(rreq, 0, &flag);
        check_parrived(rc, flag, 0, "active request, no MPI_Pready yet");

        /* Case 3b: mark the send partitions ready one at a time; each
           must eventually arrive on the receive side (MPI-5.0 section
           4.2.2: "Repeated calls to MPI_PARRIVED ... will eventually
           return flag = true"). */
        for (i = 0; i < PARTITIONS; ++i) {
            rc = MPI_Pready(i, sreq);
            check(MPI_SUCCESS == rc, "MPI_Pready failed");

            flag = 0;
            for (j = 0; j < MAX_POLL && !flag; ++j) {
                rc = MPI_Parrived(rreq, i, &flag);
                if (MPI_SUCCESS != rc) {
                    break;
                }
            }
            check_parrived(rc, flag, 1, "active request, partition ready");
        }

        MPI_Wait(&sreq, MPI_STATUS_IGNORE);
        MPI_Wait(&rreq, MPI_STATUS_IGNORE);

        for (i = 0; i < PARTITIONS * COUNT; ++i) {
            check(recvbuf[i] == (double) i, "received wrong data");
        }

        /* Case 4: the operation completed, so the request is inactive
           again -- but, unlike case 2, it has been started before.  It
           must still report arrived. */
        for (i = 0; i < PARTITIONS; ++i) {
            flag = 0;
            rc = MPI_Parrived(rreq, i, &flag);
            check_parrived(rc, flag, 1, "completed, reset inactive request");
        }
    }

    MPI_Request_free(&sreq);
    MPI_Request_free(&rreq);

    if (0 == failures) {
        printf("PASS: parrived_null_inactive\n");
    }

    MPI_Finalize();

    return failures ? 1 : 0;
}
